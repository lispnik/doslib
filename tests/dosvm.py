"""A DOS machine in QEMU, driven from Python: for the doslib tests.

The machine runs without a window on a private copy of the launcher's
disks.  It is controlled through QEMU's machine protocol (QMP): keys are
typed with "sendkey", the text screen is read out of video memory, and
pictures of the screen are saved with "screendump".  Files go on and off
its D: drive with mtools while it is switched off.
"""
import json, os, re, shutil, socket, subprocess, time

KEYS = {' ': 'spc', '\n': 'ret', '-': 'minus', '=': 'equal', ',': 'comma', '.': 'dot', '/': 'slash',
        ';': 'semicolon', "'": 'apostrophe', '\\': 'backslash', '[': 'bracket_left', ']': 'bracket_right',
        '`': 'grave_accent', '(': 'shift-9', ')': 'shift-0', '*': 'shift-8', '<': 'shift-comma',
        '>': 'shift-dot', '"': 'shift-apostrophe', ':': 'shift-semicolon', '~': 'shift-grave_accent',
        '%': 'shift-5', '#': 'shift-3', '^': 'shift-6', '$': 'shift-4', '+': 'shift-equal',
        '_': 'shift-minus', '!': 'shift-1', '?': 'shift-slash', '&': 'shift-7', '@': 'shift-2',
        '{': 'shift-bracket_left', '}': 'shift-bracket_right', '|': 'shift-backslash'}

DOS_PROMPT = 'D:\\SBCL>'


class Failure(Exception):
    """A test has failed; the text says how."""


class Machine:
    def __init__(self, work, boot_image, data_image, forwards=(), options=()):
        """WORK is a directory for the machine's own files.  FORWARDS is a
        list of (host port, port on the DOS machine); OPTIONS are more QEMU
        arguments."""
        self.work = work
        self.sources = (boot_image, data_image)
        self.forwards = list(forwards)
        self.options = list(options)
        self.qemu = None
        self.qmp = None
        os.makedirs(work, exist_ok=True)
        self.env = dict(os.environ, TZ='UTC', MTOOLS_SKIP_CHECK='1',
                        MTOOLSRC=os.path.join(work, 'mtoolsrc'))

    # ---- the disks
    def fresh_disks(self):
        """Copy the launcher's disks: the tests never write on those."""
        for source, name in zip(self.sources, ('boot.img', 'data.img')):
            target = os.path.join(self.work, name)
            if os.path.exists(target):
                os.remove(target)
            # (on macOS "cp -c" makes a copy-on-write clone, which is instant)
            if subprocess.call(['cp', '-c', source, target], stderr=subprocess.DEVNULL) != 0:
                shutil.copyfile(source, target)
        with open(self.env['MTOOLSRC'], 'w') as f:
            f.write('drive c: file="%s" partition=1\ndrive d: file="%s" partition=1\n'
                    % (os.path.join(self.work, 'boot.img'), os.path.join(self.work, 'data.img')))

    def mtools(self, *arguments):
        assert self.qemu is None, 'the machine must be switched off to touch its disks'
        return subprocess.run(arguments, env=self.env, capture_output=True)

    def put(self, host_file, dos_path):
        """Copy a file to D:, e.g. DOS_PATH = 'SBCL/T.LSP'."""
        result = self.mtools('mcopy', '-o', host_file, 'd:/' + dos_path)
        if result.returncode != 0:
            raise RuntimeError('mcopy: ' + result.stderr.decode())

    def read(self, dos_path):
        """The contents of a file on D: (bytes), or None if it is not there."""
        result = self.mtools('mtype', 'd:/' + dos_path)
        return result.stdout if result.returncode == 0 else None

    # ---- switching it on and off
    def boot(self):
        # (QMP over TCP, not a Unix socket: those cannot have long path names)
        with socket.socket() as s:
            s.bind(('127.0.0.1', 0))
            port = s.getsockname()[1]
        nic = 'user,model=pcnet' + ''.join(',hostfwd=tcp:127.0.0.1:%d-:%d' % f for f in self.forwards)
        command = ['qemu-system-i386', '-m', '512', '-cpu', 'pentium3', '-display', 'none', '-no-reboot',
                   '-qmp', 'tcp:127.0.0.1:%d,server,nowait' % port, '-nic', nic,
                   # the second display the editor's M-x live uses, and silent sound cards
                   '-device', 'secondary-vga,id=vga2',
                   '-audiodev', 'none,id=snd', '-machine', 'pcspk-audiodev=snd', '-device', 'adlib,audiodev=snd',
                   '-device', 'sb16,audiodev=snd',
                   '-drive', 'file=boot.img,format=raw,if=ide,index=0',
                   '-drive', 'file=data.img,format=raw,if=ide,index=1'] + self.options
        log = open(os.path.join(self.work, 'qemu.log'), 'w')
        self.qemu = subprocess.Popen(command, cwd=self.work, stdout=log, stderr=log)
        for attempt in range(100):
            try:
                self.qmp = socket.create_connection(('127.0.0.1', port))
                break
            except OSError:
                if self.qemu.poll() is not None or attempt == 99:
                    raise RuntimeError('QEMU did not start: ' + open(log.name).read())
                time.sleep(0.1)
        self.file = self.qmp.makefile('rw')
        self.file.readline()                                    # QEMU's greeting
        self.command('qmp_capabilities')

    def stop(self):
        if self.qemu is None:
            return
        try:
            self.file.write(json.dumps({'execute': 'quit'}) + '\n')
            self.file.flush()
            self.qemu.wait(timeout=5)
        except Exception:
            self.qemu.kill()
            self.qemu.wait()
        self.qmp.close()
        self.qemu = self.qmp = None

    # ---- talking to QEMU
    def command(self, name, **arguments):
        message = {'execute': name}
        if arguments:
            message['arguments'] = arguments
        self.file.write(json.dumps(message) + '\n')
        self.file.flush()
        while True:
            line = self.file.readline()
            if not line:
                raise RuntimeError('QEMU has gone away')
            reply = json.loads(line)
            if 'return' in reply:
                return reply['return']
            if 'error' in reply:
                raise RuntimeError('QEMU: %s' % reply['error'].get('desc'))
            # (anything else is an event: not for us)

    def monitor(self, line):
        """A command of QEMU's human monitor; returns what it printed."""
        return self.command('human-monitor-command', **{'command-line': line})

    # ---- the keyboard
    def key(self, *names, pause=0.12):
        """Press keys by QEMU's names: ret esc spc tab backspace up f12 ctrl-x alt-x shift-a ..."""
        for name in names:
            self.monitor('sendkey %s 40' % name)
            time.sleep(pause)

    def hold(self, name, milliseconds=400):
        """Press a key or a chord and keep it down for a while: long enough
        for something that looks at the keyboard 18 times a second to see it."""
        self.monitor('sendkey %s %d' % (name, milliseconds))
        time.sleep(milliseconds / 1000 + 0.2)

    def type(self, text):
        self.key(*[KEYS.get(c) or ('shift-' + c.lower() if c.isupper() else c) for c in text])

    def line(self, text, tries=3):
        """Type TEXT and Enter at a prompt that echoes (DOS, the Lisp
        listener).  What arrived is checked on the screen first: keys typed
        while the machine is busy can be lost, and Esc starts the line again."""
        for attempt in range(tries):
            self.type(text)
            time.sleep(0.2)
            shown = ''.join(row.ljust(80) for row in self.screen()).rstrip()
            if shown.endswith(text[-60:]):
                break
            self.key('esc')
            time.sleep(1)
        self.key('ret')

    # ---- the screen
    def memory(self, address, count):
        values = []
        for line in self.monitor('xp /%dxb 0x%x' % (count, address)).splitlines():
            match = re.match(r'^[0-9a-f]+: (0x.*)$', line.strip())
            if match:
                values += [int(x, 16) for x in match.group(1).split()]
        return values

    def screen(self):
        """The text screen: a string for each row (25 of 80 columns, unless
        the BIOS says the screen is another size)."""
        columns = self.memory(0x44A, 1)[0] or 80
        rows = min(60, max(25, self.memory(0x484, 1)[0] + 1))
        if columns not in (80, 132):
            columns, rows = 80, 25
        cells = self.memory(0xB8000, 2 * columns * rows)[0::2]
        return [''.join(chr(c) if 32 <= c < 127 else ' ' for c in cells[row * columns:(row + 1) * columns]).rstrip()
                for row in range(rows)]

    def video_mode(self):
        """The BIOS's video mode: 3 is 80x25 text, 0x13 is 320x200."""
        return self.memory(0x449, 1)[0]

    def picture(self, name, device=None):
        """Save the display as NAME (a PPM file in the work directory);
        returns how many different colours are in it."""
        path = os.path.join(self.work, name)
        if os.path.exists(path):
            os.remove(path)
        self.monitor('screendump %s%s' % (path, ' ' + device if device else ''))
        for attempt in range(40):
            if os.path.exists(path) and os.path.getsize(path) > 100:
                break
            time.sleep(0.05)
        data = open(path, 'rb').read()
        header = re.match(rb'P6\s+(\d+)\s+(\d+)\s+(\d+)\s', data)
        self.picture_size = (int(header.group(1)), int(header.group(2)))
        pixels = data[header.end():]
        return len(set(pixels[i:i + 3] for i in range(0, len(pixels) - 2, 3)))

    def wait_for(self, test, timeout=60, every=0.5):
        """Call TEST with the text screen until it returns something true;
        returns that, or None when TIMEOUT seconds have passed."""
        end = time.time() + timeout
        while True:
            found = test(self.screen())
            if found or time.time() > end:
                return found or None
            time.sleep(every)

    def wait_text(self, text, timeout=60):
        return self.wait_for(lambda rows: any(text in row for row in rows), timeout)

    def wait_prompt(self, prompt, timeout=120):
        """Wait until the last line with anything on it is PROMPT."""
        def at_prompt(rows):
            last = [row for row in rows if row.strip()][-1:]
            return bool(last) and last[0].rstrip() == prompt.rstrip()
        return self.wait_for(at_prompt, timeout)
