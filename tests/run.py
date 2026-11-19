#!/usr/bin/env python3
"""The doslib regression tests: everything is run on DOS, in QEMU.

    tests/run.py                 all of them (about twenty minutes)
    tests/run.py unit fire irc   only these
    tests/run.py --list          their names
    tests/run.py --internet      also the ones that need the Internet
    tests/run.py --look fire     run one example, show what is on the screen
                                 after a few seconds, and leave it: for
                                 writing a new test

It tests what "./run-dos.sh build" last built -- the launcher's disks are
copied, and the copies are what is booted -- so after changing the sources
run "./run-dos.sh update" and "./run-dos.sh build" first.  The machine has
no window; the copies, and the screen of every test that fails, are kept
in the launcher's vm/test directory.

Three kinds of test, all in cases.py:
  unit      tests/unit.lisp, loaded into Lisp on the DOS machine: the
            library, function by function
  examples  each example is started, looked at (the text on the screen, or
            the video mode and whether anything was drawn), operated where
            that is cheap, and left; an error in it fails the test
  dos       programs typed at the DOS prompt: DEMO, a saved program, and
            EMACS.EXE, which is driven through its keys; the files it
            saved are compared afterwards

Needs qemu-system-i386, mtools and Python 3.
"""
import argparse, http.server, os, socket, subprocess, sys, tempfile, threading, time

here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, here)
from dosvm import Machine, Failure, DOS_PROMPT


def free_port():
    with socket.socket() as s:
        s.bind(('127.0.0.1', 0))
        return s.getsockname()[1]


class Host:
    """What the DOS machine talks to on this computer: a web server and an
    IRC server (10.0.2.2 from inside), and the ports forwarded to it."""
    def __init__(self):
        self.web_root = tempfile.mkdtemp(prefix='doslib-test-web-')
        with open(os.path.join(self.web_root, 'hello.txt'), 'w') as f:
            f.write('Hello from the host.\n')
        with open(os.path.join(self.web_root, 'index.html'), 'w') as f:
            f.write('<html><head><title>Test page</title></head><body><h1>The first page</h1>'
                    '<p>Some words, and <a href="second.html">a link to follow</a>.</p></body></html>\n')
        with open(os.path.join(self.web_root, 'second.html'), 'w') as f:
            f.write('<html><body><h1>The second page</h1><p>You have arrived.</p></body></html>\n')
        root = self.web_root

        class Handler(http.server.SimpleHTTPRequestHandler):
            def __init__(self, *args, **kw):
                super().__init__(*args, directory=root, **kw)

            def log_message(self, *args):
                pass
        self.web = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)
        self.web_port = self.web.server_address[1]
        threading.Thread(target=self.web.serve_forever, daemon=True).start()
        self.irc_port = free_port()
        self.irc = subprocess.Popen([sys.executable, os.path.join(here, '..', 'examples', 'support',
                                                                  'irc-test-server.py'), str(self.irc_port)],
                                    stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        # host ports that reach the DOS machine's 8080 (web servers), 2323
        # (the remote listener) and 7777 (netpong)
        self.to_web, self.to_repl, self.to_pong = free_port(), free_port(), free_port()

    def forwards(self):
        return [(self.to_web, 8080), (self.to_repl, 2323), (self.to_pong, 7777)]

    def close(self):
        self.web.shutdown()
        self.irc.kill()


class Session:
    """The DOS machine and where it is: off, at the DOS prompt, or in Lisp."""
    def __init__(self, machine, host, verbose=False):
        self.m, self.host, self.verbose = machine, host, verbose
        self.state = 'off'
        self.number = 0

    def to_dos(self):
        if self.state == 'off':
            self.m.boot()
            if not self.m.wait_prompt(DOS_PROMPT, 120):
                raise Failure('DOS did not start')
            time.sleep(3)                       # the keyboard is not listened to at once
        elif self.state == 'lisp':
            self.m.line('(sb-ext:exit)')
            if not self.m.wait_prompt(DOS_PROMPT, 30):
                raise Failure('Lisp did not exit')
        self.state = 'dos'

    def to_lisp(self):
        if self.state == 'lisp':
            return
        self.to_dos()
        self.m.line('sbcl --core DOSLIB.COR --noinform --load T.LSP')
        if not self.m.wait_prompt('*', 180):
            raise Failure('Lisp did not start')
        self.state = 'lisp'

    def reset(self):
        """After a test that left the machine who knows where."""
        self.m.stop()
        self.state = 'off'

    # ---- steps: what a test does once its program is running
    def do(self, steps):
        m = self.m
        for step in steps:
            kind, rest = step[0], step[1:]
            if self.verbose:
                print('    ', step, flush=True)
            if kind == 'see':                   # ('see', text [, seconds])
                if not m.wait_text(rest[0], rest[1] if len(rest) > 1 else 40):
                    raise Failure('never showed "%s"' % rest[0])
            elif kind == 'gone':                # ('gone', text [, seconds])
                if not m.wait_for(lambda rows: not any(rest[0] in row for row in rows),
                                  rest[1] if len(rest) > 1 else 40):
                    raise Failure('"%s" stayed on the screen' % rest[0])
            elif kind == 'gfx':                 # ('gfx', mode [, colours [, seconds]])
                self.graphics(*rest)
            elif kind == 'key':                 # ('key', name, ...)
                m.key(*rest)
            elif kind == 'hold':                # ('hold', name): a key kept down a while
                m.hold(rest[0])
            elif kind == 'type':                # ('type', text)
                m.type(rest[0])
            elif kind == 'line':                # ('line', text): at a prompt that echoes
                m.line(rest[0])
            elif kind == 'wait':                # ('wait', seconds)
                time.sleep(rest[0])
            elif kind == 'prompt':              # ('prompt', text [, seconds])
                if not m.wait_prompt(rest[0], rest[1] if len(rest) > 1 else 60):
                    raise Failure('no "%s" prompt' % rest[0])
            elif kind == 'host':                # ('host', function): raises Failure, or not
                rest[0](self)
            else:
                raise ValueError(step)

    def graphics(self, mode, colours=2, seconds=40, device=None):
        """Wait for video mode MODE with at least COLOURS colours on the display."""
        end = time.time() + seconds
        seen = None
        while time.time() < end:
            seen = self.m.video_mode()
            if mode is None or seen == mode:
                count = self.m.picture('look.ppm', device)
                if count >= colours:
                    return
                seen = '%d colours in mode %xh' % (count, seen)
            else:
                seen = 'mode %xh' % seen
            time.sleep(0.5)
        raise Failure('no picture%s: %s' % (' in mode %xh' % mode if mode else '', seen))

    # ---- one example
    def example(self, case):
        self.to_lisp()
        self.number += 1
        marker = '=== %d ' % self.number
        arguments = case.get('args')
        if callable(arguments):
            arguments = arguments(self.host)
        self.m.line('(r %d "%s"%s)' % (self.number, case.get('example', case['name']),
                                       ' ' + arguments if arguments else ''))

        def ended(rows):
            for row in rows:
                if row.startswith(marker):
                    return row[len(marker):].strip()

        problem = None
        try:
            self.do(case.get('steps', ()))
        except Failure as failure:
            problem = failure
            self.keep_screen(case['name'])
        # leave it: with the keys the test names, pressed in turn until it has ended
        keys = case.get('leave', ('esc', 'ret'))
        end = time.time() + (15 if problem else case.get('patience', 60))
        result, turn = None, 0
        while True:
            result = self.m.wait_for(ended, 2)
            if result:
                time.sleep(0.3)                 # (the line may have been half written)
                result = ended(self.m.screen())
            if result or time.time() > end:
                break
            if keys:
                self.m.key(keys[turn % len(keys)])
                turn += 1
        if not result:
            if not problem:
                self.keep_screen(case['name'])
            self.reset()
            raise problem or Failure('did not end')
        if problem:
            raise problem
        if result != 'ok':
            raise Failure(result)

    def keep_screen(self, name):
        with open(os.path.join(self.m.work, 'failed-%s.txt' % name), 'w') as f:
            f.write('video mode %xh\n' % self.m.video_mode() + '\n'.join(self.m.screen()) + '\n')
        try:
            self.m.picture('failed-%s.ppm' % name)
        except Exception:
            pass

    # ---- the unit tests
    def unit(self):
        self.to_lisp()
        self.m.line('(u)')
        tally = self.m.wait_for(lambda rows: next((row for row in rows if row.startswith('=== unit')), None), 300)
        if not tally:
            self.keep_screen('unit')
            self.reset()
            raise Failure('the unit tests did not finish')
        if not tally.endswith(' 0 failed'):
            failures = [row for row in self.m.screen() if row.startswith('FAIL')]
            raise Failure(tally[4:] + ''.join('\n        ' + row for row in failures))
        return tally[9:]

    # ---- a program typed at the DOS prompt: it must come back to it
    def dos(self, case):
        self.to_dos()
        steps = case['steps']
        try:
            self.do(steps(self.host) if callable(steps) else steps)
            if not self.m.wait_prompt(DOS_PROMPT, 60):
                raise Failure('did not come back to the DOS prompt')
        except Failure:
            self.keep_screen(case['name'])
            self.reset()
            raise


def main():
    parser = argparse.ArgumentParser(description='Run the doslib tests on DOS, in QEMU.')
    parser.add_argument('names', nargs='*', help='tests to run (default: all)')
    parser.add_argument('--list', action='store_true', help='list the tests')
    parser.add_argument('--internet', action='store_true', help='also run tests that need the Internet')
    parser.add_argument('--look', metavar='EXAMPLE', help='run an example and show its screen')
    parser.add_argument('--args', default='', help='with --look: arguments, as Lisp')
    parser.add_argument('--seconds', type=float, default=8, help='with --look: how long to let it run')
    parser.add_argument('--work', help='directory for the machine (default: vm/test beside the launcher\'s disks)')
    parser.add_argument('--verbose', action='store_true', help='print each step')
    options = parser.parse_args()

    import cases
    tests = cases.all_cases()
    if options.list:
        for case in tests:
            print('%-8s %s%s' % (case['kind'], case['name'], '   (needs the Internet)' if case.get('internet') else ''))
        return 0

    tools = os.environ.get('SBCL_DOS_TOOLS') or os.path.dirname(os.path.dirname(here))
    vm = os.path.join(tools, 'vm')
    boot, data = os.path.join(vm, 'boot.img'), os.path.join(vm, 'data.img')
    if not (os.path.exists(boot) and os.path.exists(data)):
        sys.exit('run.py: no disks in %s: run ./run-dos.sh setup and ./run-dos.sh build first' % vm)
    newest = max(os.path.getmtime(os.path.join(directory, name))
                 for directory, _, names in os.walk(os.path.dirname(here))
                 if os.sep + 'tests' not in directory and os.sep + '.git' not in directory
                 for name in names if name.endswith(('.lisp', '.asd')))
    if newest > os.path.getmtime(data):
        print('note: the sources are newer than the disks; this tests what was last built\n'
              '      (./run-dos.sh update && ./run-dos.sh build brings the disks up to date)')

    host = Host()
    host.serial = free_port()                   # the DOS machine's COM1, as a TCP port here
    machine = Machine(options.work or os.path.join(vm, 'test'), boot, data, forwards=host.forwards(),
                      options=['-serial', 'tcp:127.0.0.1:%d,server,nowait' % host.serial,
                               '-parallel', 'file:lpt.txt'])
    machine.fresh_disks()
    for name in ('DOSLIB.COR', 'EMACS.EXE'):
        if machine.mtools('mdir', 'd:/SBCL/' + name).returncode != 0:
            sys.exit('run.py: there is no %s on the disk: run ./run-dos.sh build first' % name)
    machine.put(os.path.join(here, 'unit.lisp'), 'SBCL/T.LSP')
    for name, text in cases.FILES.items():
        path = os.path.join(machine.work, 'file.tmp')
        with open(path, 'wb') as f:
            f.write(text.replace('\n', '\r\n').encode('latin-1'))
        machine.put(path, 'SBCL/' + name)
        os.remove(path)
    session = Session(machine, host, options.verbose)

    try:
        if options.look:
            return look(session, options)
        if options.names:
            unknown = set(options.names) - set(case['name'] for case in tests)
            if unknown:
                sys.exit('run.py: no test named %s' % ', '.join(sorted(unknown)))
            tests = [case for case in tests if case['name'] in options.names]
        else:
            tests = [case for case in tests if options.internet or not case.get('internet')]
        passed, failed, start = 0, [], time.time()
        for case in tests:
            began = time.time()
            try:
                note = {'unit': lambda c: session.unit(), 'example': session.example,
                        'dos': session.dos}[case['kind']](case)
                passed += 1
                print('ok    %-18s %s' % (case['name'], note or '%.0f s' % (time.time() - began)), flush=True)
            except Failure as failure:
                failed.append(case['name'])
                print('FAIL  %-18s %s' % (case['name'], failure), flush=True)
            except Exception as error:      # QEMU went away, or the like: start again
                failed.append(case['name'])
                print('FAIL  %-18s %s: %s' % (case['name'], type(error).__name__, error), flush=True)
                session.reset()
        # what the tests left on the disk
        session.reset()
        for name, check in cases.AFTER:
            if not any(case['name'] == name for case in tests) or name in failed:
                continue
            try:
                check(machine, host)
            except Failure as failure:
                passed -= 1
                failed.append(name)
                print('FAIL  %-18s afterwards: %s' % (name, failure), flush=True)
        print('\n%d passed, %d failed, in %.0f minutes' % (passed, len(failed), (time.time() - start) / 60))
        if failed:
            print('failed: %s\n(the screens they left are in %s)' % (' '.join(failed), machine.work))
        return 1 if failed else 0
    finally:
        machine.stop()
        host.close()


def look(session, options):
    """Run one example and print what can be seen: for writing its test."""
    m = session.m
    session.to_lisp()
    m.line('(r 1 "%s"%s)' % (options.look, ' ' + options.args if options.args else ''))
    time.sleep(options.seconds)
    mode = m.video_mode()
    print('video mode %xh, %d colours' % (mode, m.picture('look.ppm')))
    print('\n'.join(m.screen()).rstrip())
    began = time.time()
    for key in [None, 'esc', 'ret'] * 6:
        if key:
            m.key(key)
        if m.wait_text('=== 1 ', 3):
            print('--- ended after %s (%.0f s): %s' % (key or 'no key', time.time() - began,
                                                       [r for r in m.screen() if r.startswith('=== 1 ')][0]))
            return 0
    print('--- did not end; the screen now:\n' + '\n'.join(m.screen()).rstrip())
    return 1


if __name__ == '__main__':
    sys.exit(main())
