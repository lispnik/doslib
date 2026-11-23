#!/usr/bin/env python3
"""Pictures of the examples, for the README: each is run on DOS, in QEMU,
as its test runs it, and the display is saved when the test's steps are
done and before the example is left.

    tests/shots.py               all of them, into screenshots/NAME.png,
                                 and screenshots/README.md, the page of them
    tests/shots.py fire plasma   only these (the page is made again from
                                 the pictures that are there)
    tests/shots.py --internet    also the ones that need the Internet

Like run.py, it shows what "./run-dos.sh build" last built.  Needs
qemu-system-i386, mtools and Python 3.
"""
import argparse, os, re, struct, sys, time, zlib

here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, here)
from dosvm import Machine, Failure
import run, cases

project = os.path.dirname(here)
shots = os.path.join(project, 'screenshots')


def png(path, width, height, pixels, scale=1):
    """Write RGB PIXELS (bytes, 3 a pixel) as a PNG file, each pixel SCALE square."""
    rows = []
    for y in range(height):
        row = pixels[3 * width * y:3 * width * (y + 1)]
        if scale > 1:
            row = b''.join(row[i:i + 3] * scale for i in range(0, len(row), 3))
        rows.extend([b'\0' + row] * scale)

    def chunk(kind, data):
        return struct.pack('>I', len(data)) + kind + data + struct.pack('>I', zlib.crc32(kind + data))
    with open(path, 'wb') as f:
        f.write(b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', width * scale, height * scale, 8, 2, 0, 0, 0))
                + chunk(b'IDAT', zlib.compress(b''.join(rows), 9)) + chunk(b'IEND', b''))


def save(machine, name):
    machine.picture('shot.ppm')
    data = open(os.path.join(machine.work, 'shot.ppm'), 'rb').read()
    header = re.match(rb'P6\s+(\d+)\s+(\d+)\s+(\d+)\s', data)
    width, height = int(header.group(1)), int(header.group(2))
    # the small modes twice the size, so that they can be seen
    png(os.path.join(shots, name + '.png'), width, height, data[header.end():], 2 if width <= 400 else 1)


class Shooter(run.Session):
    def example(self, case):
        self.to_lisp()
        self.number += 1
        marker = '=== %d ' % self.number
        arguments = case.get('args')
        if callable(arguments):
            arguments = arguments(self.host)
        # (on an empty screen: setting the mode clears it)
        self.m.line('(progn (doslib:video-mode 3) (r %d "%s"%s))'
                    % (self.number, case.get('example', case['name']), ' ' + arguments if arguments else ''))
        # The picture is taken after the last step that looks for a picture
        # (some examples go back to text and say what they did), or else
        # after all the steps.
        steps = list(case.get('steps', ()))
        looks = [i for i, step in enumerate(steps) if step[0] in ('gfx', 'host')]
        cut = looks[-1] + 1 if looks else len(steps)
        problem = None
        try:
            self.do(steps[:cut])
            time.sleep(case.get('settle', 1.5))
            save(self.m, case['name'])
            self.do(steps[cut:])
        except Failure as failure:
            problem = failure
        keys = case.get('leave', ('esc', 'ret'))
        end, turn = time.time() + 40, 0
        while not self.m.wait_for(lambda rows: any(row.startswith(marker) for row in rows), 2):
            if time.time() > end:
                self.reset()
                raise problem or Failure('did not end')
            if keys:
                self.m.key(keys[turn % len(keys)])
                turn += 1
        if problem:
            raise problem


def descriptions():
    """name -> (category, description), from the examples' sources."""
    found = {}
    for directory, _, names in os.walk(os.path.join(project, 'examples')):
        for name in names:
            if name.endswith('.lisp'):
                text = open(os.path.join(directory, name), encoding='latin-1').read()
                for match in re.finditer(r'define-example\s+"([^"]+)".*?:description\s+"((?:[^"\\]|\\.)*)"', text, re.S):
                    found[match.group(1)] = (os.path.basename(directory),
                                             re.sub(r'\s+', ' ', match.group(2).replace('\\"', '"')))
    return found


def page():
    """screenshots/README.md: every picture there is, by kind of example."""
    about = descriptions()
    have = sorted(name[:-4] for name in os.listdir(shots) if name.endswith('.png') and name[:-4] in about)
    kinds = {}
    for name in have:
        kind, text = about.get(name, ('other', ''))
        kinds.setdefault(kind, []).append((name, text))
    lines = ['# The examples, as they look', '',
             'Each picture is the display of the DOS machine (QEMU) while the example runs,',
             'taken by `tests/shots.py`.  Run one with `demo NAME` at the DOS prompt, or',
             '`(doslib-examples:run "NAME")` in Lisp.', '']
    for kind in sorted(kinds):
        lines += ['## %s' % kind, '', '| | | |', '|---|---|---|']
        cells = ['<img src="%s.png" width="260"><br>**%s**<br>%s' % (name, name, text.replace('|', '/'))
                 for name, text in kinds[kind]]
        for i in range(0, len(cells), 3):
            row = cells[i:i + 3]
            lines.append('| ' + ' | '.join(row + [''] * (3 - len(row))) + ' |')
        lines.append('')
    missing = sorted(set(about) - set(have))
    if missing:
        lines += ['No picture: %s.' % ', '.join('`%s`' % name for name in missing), '']
    with open(os.path.join(shots, 'README.md'), 'w') as f:
        f.write('\n'.join(lines))
    return len(have)


def main():
    parser = argparse.ArgumentParser(description='Take pictures of the doslib examples on DOS, in QEMU.')
    parser.add_argument('names', nargs='*', help='examples (default: all)')
    parser.add_argument('--internet', action='store_true', help='also the ones that need the Internet')
    parser.add_argument('--page', action='store_true', help='only make the page again')
    options = parser.parse_args()
    os.makedirs(shots, exist_ok=True)
    if options.page:
        print('%d pictures on the page' % page())
        return 0
    # (an example the tests run more than one way is pictured the plain way)
    known = descriptions()
    tests = [case for case in cases.all_cases() if case['kind'] == 'example' and case['name'] in known]
    if options.names:
        tests = [case for case in tests if case['name'] in options.names]
    else:
        tests = [case for case in tests if options.internet or not case.get('internet')]
    tools = os.environ.get('SBCL_DOS_TOOLS') or os.path.dirname(project)
    vm = os.path.join(tools, 'vm')
    host = run.Host()
    host.serial = run.free_port()
    machine = Machine(os.path.join(vm, 'shots'), os.path.join(vm, 'boot.img'), os.path.join(vm, 'data.img'),
                      forwards=host.forwards(),
                      options=['-serial', 'tcp:127.0.0.1:%d,server,nowait' % host.serial,
                               '-parallel', 'file:lpt.txt'])
    machine.fresh_disks()
    machine.put(os.path.join(here, 'unit.lisp'), 'SBCL/T.LSP')
    for name, text in cases.FILES.items():
        path = os.path.join(machine.work, 'file.tmp')
        with open(path, 'wb') as f:
            f.write(text.replace('\n', '\r\n').encode('latin-1'))
        machine.put(path, 'SBCL/' + name)
        os.remove(path)
    session = Shooter(machine, host)
    failed = []
    try:
        for case in tests:
            try:
                session.example(case)
                print('ok    %s' % case['name'], flush=True)
            except Failure as failure:
                failed.append(case['name'])
                print('FAIL  %-18s %s' % (case['name'], failure), flush=True)
            except Exception as error:
                failed.append(case['name'])
                print('FAIL  %-18s %s: %s' % (case['name'], type(error).__name__, error), flush=True)
                session.reset()
    finally:
        machine.stop()
        host.close()
    print('%d pictures on the page; %d examples went wrong%s'
          % (page(), len(failed), ': ' + ' '.join(failed) if failed else ''))
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main())
