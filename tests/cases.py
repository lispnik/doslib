"""The tests: what to run, and what must be seen.  See run.py.

A test is a dictionary:
  kind      'unit', 'example' (run from Lisp through R in unit.lisp) or
            'dos' (typed at the DOS prompt: the editor, DEMO, saved programs;
            it must end back at the prompt)
  name      what the test is called; for an example, the example's name
            unless 'example' gives that
  args      for an example: its arguments, as Lisp text (or a function of
            the Host that returns them)
  steps     what to do and look for once it is running (Session.do in
            run.py says what a step can be)
  leave     for an example: keys pressed in turn until it has ended
            (default: Esc, Enter); None for one that ends by itself
  patience  seconds to wait for it to end (default 60)
  internet  true if it needs the Internet: not run unless asked for

To write a new one, "tests/run.py --look NAME" shows what the example puts
on the screen.
"""
import os, socket, time, urllib.request
from dosvm import Failure, DOS_PROMPT

# Files put in D:\SBCL before the machine starts (with DOS line endings).
FILES = {
    'T2.TXT': 'one\ntwo\nthree\n',
    'T9.FAS': 'compiled\n', 'T9.BAK': 'old\n',
    'T8.SCM': '(define (f x)\n  (* x 2))\n',
    'T3.TXT': 'alpha beta\nalpha gamma\n',
    'T4.TXT': 'one\ntwo\n',
    'T6.LSP': '(list (alpha beta) gamma)\n',
    'T7.LSP': ('(in-package #:cl-user)\n(defun seven-up (n) (+ n 7))\n(defmacro twice (x) `(progn ,x ,x))\n'
               '(defun caller ()\n  (twice (seven-up 1)))\n(defun both () (twice (twice 5)))\n'),
    # the editor's own tests, loaded into it by the test emacs-lisp-edit
    'ED.LSP': open(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'editor.lisp')).read(),
    'T5.TXT': ''.join('line %02d of the file for the mouse test: long enough to click in\n' % n for n in range(1, 21)),
}


# ---- this computer's side of the network tests
def fetch(port, path, tries=3):
    """A page from a server on the DOS machine, through a forwarded port."""
    for attempt in range(tries):
        try:
            with urllib.request.urlopen('http://127.0.0.1:%d%s' % (port, path), timeout=30) as reply:
                return reply.read().decode('latin-1')
        except OSError as error:
            problem = error
            time.sleep(2)
    raise Failure('GET %s: %s' % (path, problem))


def expect_page(path, text, port='to_web'):
    def step(session):
        page = fetch(getattr(session.host, port), path)
        if text not in page:
            raise Failure('GET %s did not say "%s" but: %s' % (path, text, page[:200].replace('\n', ' ')))
    return step


def quit_server(session):               # the server stops before it can answer
    try:
        fetch(session.host.to_web, '/quit', tries=1)
    except Failure:
        pass


def chat(port, exchanges, timeout=60):
    """Connect to PORT (a name in Host) and, for each (send, expect), send
    the line and read until the text has arrived."""
    def step(session):
        try:
            with socket.create_connection(('127.0.0.1', getattr(session.host, port)), timeout=timeout) as s:
                for send, expect in exchanges:
                    if send:
                        s.sendall(send.encode())
                    data = b''
                    while expect and expect.encode() not in data:
                        try:
                            more = s.recv(4096)
                        except socket.timeout:
                            raise Failure('"%s" never came; after %r there was only %r' % (expect, send, data[-200:]))
                        if not more:
                            raise Failure('the connection closed before "%s" came: %r' % (expect, data[-200:]))
                        data += more
        except OSError as error:
            raise Failure('port %s: %s' % (port, error))
    return step


def example(name, *steps, **more):
    return dict(kind='example', name=name, steps=steps, **more)


def text(name, *seen, **more):
    """An example that prints, and returns by itself."""
    more.setdefault('leave', None)
    return example(name, *[('see', s) for s in seen], **more)


def picture(name, mode=0x13, colours=4, **more):
    """An example that draws until a key is pressed."""
    return example(name, ('gfx', mode, colours), **more)


def click(session):
    """Press and let go the left mouse button, one cell to the right of where it was."""
    m = session.m
    m.monitor('mouse_move 30 0')
    time.sleep(0.5)
    m.monitor('mouse_button 1')
    time.sleep(0.5)
    m.monitor('mouse_button 0')
    time.sleep(0.5)


def press(down):
    """Press the left mouse button, move the mouse DOWN, and let go."""
    def step(session):
        m = session.m
        m.monitor('mouse_button 1')
        time.sleep(0.6)
        if down:
            for part in range(4):
                m.monitor('mouse_move 0 %d' % (down // 4))
                time.sleep(0.3)
        m.monitor('mouse_button 0')
        time.sleep(0.6)
    return step


def dos(name, *steps):
    return dict(kind='dos', name=name, steps=steps)


def meta_x(command):
    return [('key', 'alt-x'), ('see', 'M-x'), ('type', command), ('key', 'ret')]


def emacs(name, file, *steps):
    """An editor test: start it, do STEPS, leave with C-x C-c."""
    return dos(name, ('line', ('emacs ' + file).strip()), ('see', ' -- ', 240), *steps,
               ('key', 'ctrl-x', 'ctrl-c'))


def to_buffer(name):
    """The keys that show the buffer NAME in the editor's window."""
    return [('key', 'ctrl-x', 'b'), ('see', 'Switch to buffer'), ('type', name), ('key', 'ret')]


def all_cases():
    web = lambda host: '"http://10.0.2.2:%d/' % host.web_port
    return [
        dict(kind='unit', name='unit'),

        # ---- graphics
        picture('bmpview', colours=50),
        picture('fire', colours=20),
        text('font', 'Characters 128-137 are now pictures', leave=('esc', 'ret')),
        picture('gcviz'),
        picture('lsystem'),
        picture('mandel', colours=20),
        picture('modex'),
        example('paint', ('gfx', 0x13, 4), ('key', '4', 'f', 'p', 'c', 'u')),
        picture('plasma', colours=100),
        example('raycast', ('gfx', 0x13, 20), ('key', 'up', 'up', 'left', 'right', 'down'), ('gfx', 0x13, 20)),
        picture('raytrace', patience=400),
        picture('stars'),
        text('textfx', 'Lisp writing straight to B800:0000', leave=('esc', 'ret')),
        picture('turtle', colours=20),
        picture('vesa', mode=None, colours=16),
        picture('wire3d'),

        # ---- system
        text('asm', 'same bytes as the hand-written stub: T'),
        text('asmdsl', 'As machine code', leave=('ret',), patience=120),
        example('calc', ('gfx', 3, 3),                                  # Life, underneath
                ('key', 'f12'), ('see', 'Lisp calculator'),
                ('type', '2^10 + 1'), ('key', 'ret'), ('see', '1025'),
                ('type', '(expt 2 70)'), ('key', 'ret'), ('see', '1180591620717411303424'),
                ('key', 'esc'), ('gone', 'Lisp calculator'),
                ('key', 'f12'), ('see', 'Lisp calculator'), ('key', 'f12'), ('gone', 'Lisp calculator')),
        text('diskinfo', 'the master boot record (signature AA55)', 'FAT16'),
        text('dosinfo', 'COMSPEC', 'KB free'),
        example('fatview', ('see', 'OEM name'), ('key', 'd'), ('see', 'DOSLIB'),
                ('key', 'f'), ('wait', 1), ('key', 's'), ('wait', 1), ('key', 'b'), ('see', 'OEM name')),
        text('intvec', 'ticks arrived while Lisp was computing'),
        text('ivt', 'timer tick, IRQ 0'),
        text('mcbwalk', 'bytes in use'),
        text('memmap', 'DPMI host'),
        # the report, a screenful at a time; then all the rest, to the map at the end
        example('pciscan', ('see', 'Bridge: host'), ('see', '-- more'), ('key', 'a'),
                ('see', 'PCI functions found'), ('see', 'Interrupt lines'), leave=None),
        example('pciscan-file', ('see', 'is written to PCISCAN.TXT'), example='pciscan', args='"file"',
                leave=None),
        text('ports', 'keyboard answered FAh', 'LPT1 is at 378h', 'characters, one strobe pulse each'),
        text('printer', 'Press a key.', leave=('ret',)),
        example('printer-absent', ('see', 'there is no such printer port'), ('key', '3'), ('see', 'Press a key.'),
                example='printer', args=':printer 1', leave=('ret',)),
        text('probes', 'Real-time clock', 'Serial and parallel ports the BIOS found', args='nil'),
        example('serial', ('see', 'Listening on COM1'),
                ('host', chat('serial', [('(+ 40 2)\r', '42'), ('(expt 2 100)\r', '1267650600228229401496703205376'),
                                         ('(/ 1 0)\r', 'ERROR'), (':bye\r', 'Goodbye.')])),
                leave=None),
        text('sysinfo', 'CPU vendor', 'CPU signature'),

        # ---- lisp
        text('bench', '(fib 32)', 'digits of 1000!', patience=180),
        example('hotfix', ('see', 'Lives'), leave=('esc', '4')),        # 4: "Stop the game", if the bug has struck
        example('life', ('gfx', 3, 3)),
        text('live', 'redefine CELL and call (draw) again.'),
        example('restarts', ('see', 'drive not ready'), ('key', '4'),   # "Give up on the rest"
                ('see', 'Part 2'), ('see', 'Press a key.'), leave=('ret',)),
        picture('resume'),
        example('shell', ('see', 'Type help.'), ('line', 'dir *.bat'), ('see', 'DEMO.BAT'),
                ('line', '(* 6 7)'), ('see', '42'), ('line', 'exit'), leave=None),
        example('snake', ('gfx', 3, 3), ('key', 'down', 'right', 'up')),
        text('special', 'interpreted:', 'compiled:', 'The specialised function is', patience=240),
        example('windows', ('see', 'Counter'), ('see', 'Count: 0')),

        # ---- sound (the machine's cards are silent)
        example('adlib', ('see', 'AdLib card found'), ('see', 'Playing jacques'), ('see', 'C4 D4 E4 C4')),
        # ---- DMA.  (QEMU's Sound Blaster does not record, and the test machine's drive A: is empty.)
        example('dmascope', ('gfx', 0x13, 6), ('key', 'spc'), ('wait', 1), ('key', 'spc'), ('wait', 1)),
        example('record', ('see', 'does not record'), leave=None),
        example('modplay', ('see', 'mixing takes'), ('wait', 3)),
        # a module from a file: read, described (the same numbers the script that wrote it gives), played
        example('modplay-info', ('see', 'Once through: 288 rows, 1520 ticks, 29333 ms'),
                example='modplay', args='"song.mod" "info"', leave=None),
        example('modplay-file', ('see', 'doslib demo song'), ('see', 'mixing takes'), ('wait', 3),
                example='modplay', args='"song.mod"'),
        example('mixer', ('see', 'self-test: 6,304 bytes checked, 0 wrong', 60), ('key', '1'), ('key', 'm'),
                ('wait', 1), ('see', 'multiplications')),
        example('mixer-mono', ('see', 'mono'), ('see', '0 wrong', 60), ('wait', 1), example='mixer', args='"mono"'),
        example('spectrum', ('gfx', 0x13, 20), ('key', 'spc'), ('wait', 1)),
        example('spectrum-silent', ('gfx', 0x13, 20), ('wait', 1), example='spectrum', args='"silent"'),
        example('tune', ('see', 'Ode to Joy'), ('see', 'key on'), ('see', 'to a tick'), ('wait', 2)),
        example('midi', ('see', 'No MPU-401 at 330h'), leave=None),
        example('midi-show', ('see', 'the bytes are shown, not sent'), ('see', 'note on'), ('wait', 2),
                example='midi', args='"show"'),
        example('floppy', ('see', 'no disk in A:'), leave=None),
        example('timer', ('see', 'Against the clock chip'), ('key', 'spc'), ('wait', 1)),
        # the clock rate and the chart of copies; any key leaves, and it says which won
        example('speed', ('see', 'REP MOVSD to the screen'), ('see', 'The copies agree: yes'),
                ('key', 'spc'), ('see', 'million bytes a second'), leave=None),
        example('blaster-16', ('see', 'DMA channel 5', 150), ('see', 'interrupts handled', 90),
                example='blaster', args='"16"', leave=None, patience=200),
        # the real-time clock on interrupt request 8: its periodic rate, then its alarm
        text('alarm', 'interrupts in', 'The alarm rang'),
        # the keyboard's interrupt: a key pressed is seen, by name
        example('keys', ('see', 'Most at once'), ('key', 'a'), ('see', 'Last key: A (scan code 30)'),
                ('key', 'right'), ('see', 'Last key: Right (scan code 77)')),
        # a terminal on COM1: the host clears the screen, writes at 5,10 and asks where the cursor is
        example('term', ('see', 'F10 leaves'),
                ('host', chat('serial', [('\x1b[2J\x1b[5;10HHello from the host\x1b[6n', '\x1b[5;29R')])),
                ('see', 'Hello from the host'), leave=('f10',)),
        text('xms', 'every byte right', 'EMS'),
        # hardware the test machine does not have: each says so
        text('joystick', 'No joystick'),
        # (the test machine has a drive, and no disc in it)
        text('cdrom', 'CD-ROM extensions', 'no disc in the drive'),
        # the debugger: run two routines to their ends, a step at a time underneath
        example('step', ('see', 'gcd:'), ('key', 'spc', 'spc'), ('see', '2 instructions done'),
                ('key', 'r'), ('see', 'EAX is 21'), ('key', 'n', 'n'), ('see', 'fact:'),
                ('key', 'r'), ('see', 'EAX is 120')),
        example('blaster', ('see', 'DSP version'), ('see', 'interrupts handled', 60), leave=None),
        example('speaker', ('see', 'Playing ode'), ('see', 'E4 E4 F4 G4')),

        # ---- network: this computer is 10.0.2.2 to the DOS machine
        text('wget', 'Hello from the host.', args=lambda host: web(host) + 'hello.txt"'),
        example('websrv', ('see', 'Serving on port 8080'),
                ('host', expect_page('/', 'Evaluate')),
                ('host', expect_page('/eval?(expt%202%20100)', '1267650600228229401496703205376')),
                ('host', expect_page('/files', 'README.TXT')),
                ('host', quit_server), ('see', 'Stopped after'), leave=None),
        example('files', ('see', 'Press a key to stop'),
                ('host', expect_page('/', 'SBCL')),
                ('host', expect_page('/SBCL/README.TXT', 'SBCL for DOS')),
                ('host', expect_page('/SBCL/DEMO.BAT', 'DEMOSBCL'))),
        example('remote', ('gfx', 0x13, 20),
                ('host', chat('to_repl', [(None, 'DOS> '), ('(+ 40 2)\n', '42'),
                                          ('(defun pixel (x y time) (declare (ignore y time)) x)\n', 'PIXEL'),
                                          ('(quit-demo)\n', 'BYE')])),
                leave=None),
        example('netpong', ('wait', 3), ('host', chat('to_pong', [('P 11\n', 'S ')])), ('key', 'up', 'down')),
        example('irc', ('see', 'welcome to #test'), ('type', 'testing one two'), ('key', 'ret'),
                ('see', 'you said: testing one two'),
                args=lambda host: '"10.0.2.2" %d "dosuser" "#test"' % host.irc_port),
        example('browse', ('see', 'The first page'), ('type', '1'), ('key', 'ret'), ('see', 'You have arrived'),
                ('key', 'b'), ('see', 'The first page'),
                args=lambda host: web(host) + '"', leave=('esc',)),
        example('ntp', ('wait', 20), internet=True),
        example('browse-gopher', ('see', 'Floodgap', 90), example='browse', internet=True, leave=('esc',)),

        # ---- blitting, Mode X, the linear framebuffer, raster effects
        example('blit', ('gfx', 0x13, 20), ('key', 'spc'), ('wait', 1), ('key', 'spc'), ('wait', 1)),
        example('dirty', ('gfx', 0x13, 20), ('key', 'spc'), ('wait', 1), ('key', 'spc'), ('wait', 1)),
        example('compiled', ('gfx', 0x13, 5), ('wait', 2)),
        # a formula compiled as it is typed: the machine code on one screen, the picture on the next
        example('formula', ('see', 'bytes of machine code'), ('key', 'spc'), ('gfx', 0x13, 50),
                ('key', '3'), ('see', '(* 40 time)'), ('see', 'bytes of machine code'), ('key', 'spc'),
                ('gfx', 0x13, 50), ('key', 'esc'), leave=None),
        # instructions added to the compiler; processor faults as conditions; drivers from the registers
        text('vop', 'in line (a VOP)', 'BSWAP EAX'),
        text('faults', 'MEMORY-FAULT-ERROR', 'DIVISION-BY-ZERO', 'Still here: (+ 40 2) is 42',
             'The sum is 1000, with 1 fault'),
        example('driver', ('see', 'It is an AMD PCnet'), ('see', '52:54:00:12:34:56'),
                ('see', 'the VESA BIOS says the same'), ('see', 'Press a key'),
                ('key', 'spc'), ('host', display_is(640, 480, 100)), ('key', 'spc'),
                ('see', 'That was 640x480 in 256 colours'), leave=None),
        example('flip', ('gfx', 0x13, 8), ('key', 'spc'), ('wait', 1), ('key', 'spc'), ('wait', 1)),
        # ---- more graphics: scrolling worlds, palette and fonts, planar mode, effects, picture files
        example('tiles', ('gfx', None, 8), ('wait', 2), ('key', 'p'), ('wait', 1)),
        example('parallax', ('gfx', None, 30), ('key', 'right'), ('key', 'spc'), ('wait', 1)),
        example('split', ('gfx', None, 20), ('key', 'up'), ('key', 'up'), ('wait', 1), ('key', 'down'), ('wait', 1)),
        example('palette', ('gfx', 0x13, 60), ('key', 'spc'), ('wait', 2), ('key', 'spc'), ('gfx', 0x13, 60),
                ('wait', 1)),
        example('palettes', ('gfx', 0x13, 60), ('key', 'spc'), ('wait', 2), ('gfx', 0x13, 20),
                ('key', 'spc'), ('wait', 2), ('gfx', 0x13, 20)),
        example('shapes', ('gfx', 0x13, 12)),
        example('textfont', ('see', 'ROM'), ('key', 'spc'), ('see', '176 of 176 agree'), ('key', 'spc'),
                ('see', 'italics'), ('key', 'spc'), ('see', '50 rows of 8'), ('key', 'spc'),
                ('see', '25 rows of 16')),
        example('mode12', ('gfx', 0x12, 16), ('key', 'spc'), ('wait', 1), ('key', 'spc'), ('wait', 1),
                ('key', 'spc'), ('wait', 1), ('key', 'spc'), ('wait', 2), patience=120),
        example('rotozoom', ('gfx', 0x13, 100), ('wait', 1)),
        example('tunnel', ('gfx', 0x13, 100), ('wait', 1)),
        example('solid3d', ('gfx', 0x13, 3), ('key', 'spc'), ('gfx', 0x13, 8), ('key', 'ret'), ('gfx', 0x13, 15),
                ('wait', 1)),
        example('picview', ('gfx', 0x13, 200), ('key', 'spc'), ('wait', 1), ('gfx', 0x13, 200), ('key', 'spc'),
                ('see', 'GIF 87a, interlaced, 320 by 200'), leave=None),
        example('picview-missing', ('see', 'is not there'), example='picview', args='"NOPE.PCX"', leave=None),
        example('scroll', ('gfx', 0x13, 12), ('key', 'right'), ('key', 'left'), ('key', 'spc'), ('wait', 1)),
        example('latch', ('gfx', 0x13, 40), ('key', 'spc'), ('wait', 1)),
        example('lfb', ('gfx', None, 50), ('key', 'spc'), ('wait', 1)),
        example('copper', ('gfx', 0x13, 50), ('key', 'f'), ('wait', 1)),
        # the sprite editor: Space paints the cell under the pointer (the middle one, to
        # begin with) in white, and so does the left button; S saves what was drawn
        example('sprite', ('gfx', 0x13, 20), ('key', 'c'), ('wait', 1), ('key', 'spc'), ('wait', 1),
                ('host', click), ('key', 's'), ('wait', 1)),
        # ---- interrupting: Ctrl-Break, and Ctrl-Alt-Shift for keyboards without one
        example('interrupt-loop', ('see', 'spinning'), ('hold', 'ctrl-pause'), ('see', 'spin was interrupted'),
                example='spin', leave=None),
        example('interrupt-chord', ('see', 'spinning'), ('hold', 'ctrl-alt-shift'), ('see', 'spin was interrupted'),
                example='spin', leave=None),
        example('interrupt-graphics', ('gfx', 0x13, 100), ('hold', 'ctrl-pause'), ('see', 'plasma was interrupted'),
                example='plasma', leave=None),

        # ---- programs started from the DOS prompt
        # with nothing to catch it, an interrupt is SBCL's debugger, and CONTINUE carries on
        dos('interrupt-debugger', ('line', 'sbcl --noinform --eval "(loop)"'), ('wait', 6), ('hold', 'ctrl-pause'),
            ('see', 'Interactive interrupt'), ('see', 'Return from SB-UNIX:SIGINT'),
            ('line', '(sb-ext:exit)')),
        # Ctrl-C reaches Lisp when DOS notices it, which is when the program next calls DOS
        dos('interrupt-ctrl-c', ('line', 'sbcl --noinform --eval "(loop (print 1))"'), ('wait', 6), ('key', 'ctrl-c'),
            ('see', 'Interactive interrupt'), ('line', '(sb-ext:exit)')),
        # Ctrl-Break while SBCL is starting is dropped: the program runs as if nothing was pressed
        dos('interrupt-startup', ('line', 'sbcl --noinform --eval "(print (+ 40 2))" --quit'),
            ('hold', 'ctrl-pause'), ('wait', 1), ('hold', 'ctrl-alt-shift'), ('see', '42')),
        dos('demo-arguments', ('line', 'demo speaker ?'), ('see', 'Tunes: ')),
        dos('demo', ('line', 'demo plasma'), ('gfx', 0x13, 100), ('key', 'esc')),
        dos('demo-menu', ('line', 'demo'), ('see', 'Example to run'), ('line', 'asm'),
            ('see', 'same bytes as the hand-written stub: T'), ('key', 'ret')),
        # RESUME.EXE is the half-drawn picture, saved: it carries on
        dos('resume-saved', ('line', 'demo resume'), ('gfx', 0x13, 4), ('wait', 3), ('key', 's'),
            ('prompt', DOS_PROMPT, 180), ('line', 'resume'), ('gfx', 0x13, 4), ('key', 'esc')),

        # ---- the editor
        emacs('emacs-edit', 'T1.TXT', ('see', '(New file)'),
              ('type', 'Hello, DOS.'), ('key', 'ret'), ('type', '(+ 40 2)'),
              ('key', 'ctrl-x', 'ctrl-e'), ('see', '=> 42'),
              ('key', 'ctrl-x', 'ctrl-s'), ('see', 'Wrote 2 lines to T1.TXT')),
        emacs('emacs-kill-yank', 'T2.TXT', ('see', 'three'),
              ('key', 'ctrl-k', 'ctrl-k', 'ctrl-n', 'ctrl-y'),           # "one" goes below "two"
              ('type', '!'), ('key', 'ctrl-x', 'u'),                      # and a mistake is undone
              ('key', 'ctrl-x', 'ctrl-s'), ('see', 'Wrote')),
        # the end of the buffer is after the file's last newline; the file gets no second one
        emacs('emacs-final-newline', 'T4.TXT', ('see', 'Read 2 lines'),
              ('key', 'shift-alt-dot'), ('type', 'three'),
              ('key', 'ctrl-x', 'ctrl-s'), ('see', 'Wrote 3 lines')),
        # M-. finds a definition: in a file the compiler remembers, then in the buffer itself
        emacs('emacs-definition', '',
              ('key', 'alt-dot'), ('see', 'Find definition of'), ('type', 'doslib-emacs:emacs'), ('key', 'ret'),
              ('see', 'MAIN.LISP'), ('see', '(defun emacs (&optional file)'),
              ('key', 'alt-comma'), ('see', '*scratch*'), ('gone', '(defun emacs (&optional file)'),
              ('type', '(defun here () 1)'), ('key', 'ret', 'ret'), ('type', '(here'),
              ('key', 'alt-dot'), ('see', 'Find definition of: here'), ('key', 'ret'), ('see', '*scratch*, line 4')),
        # ... and from a use of a macro in one of the library's files to its definition in
        # another; ESC and then the key is Meta too, for keyboards whose Alt is something else
        emacs('emacs-definition-macro', 'D:\\DOSLIB\\src\\video.lisp', ('see', 'VIDEO.LISP'),
              ('key', 'ctrl-s'), ('type', '(with-dos-memory'), ('key', 'ret', 'left', 'left'),
              ('key', 'alt-dot'), ('see', 'Find definition of: with-dos-memory'), ('key', 'ret'),
              ('see', 'DOS.LISP, line'), ('see', '(defmacro with-dos-memory'),
              ('key', 'alt-comma'), ('see', 'VIDEO.LISP'),
              ('key', 'esc', 'dot'), ('see', 'Find definition of: with-dos-memory'), ('key', 'ret'),
              ('see', 'DOS.LISP, line'),
              # (some BIOSes -- DOSBox-X's -- report Alt with . or , as the character F0h, not 0)
              ('key', 'shift-alt-semicolon'), ('type', '(list (key-name 240 52 8) (key-name 0 52 8) (key-name 240 51 8))'),
              ('key', 'ret'), ('see', '("M-." "M-." "M-,")')),
        # the mouse starts in the middle of the screen: a click puts point there (line 13,
        # column 40); a drag downwards selects from there, which M-w copies
        emacs('emacs-mouse', 'T5.TXT', ('see', 'Read 20 lines'), ('wait', 1),
              ('host', press(0)), ('type', 'X'),
              ('host', press(24)), ('key', 'alt-w'),
              ('key', 'shift-alt-dot', 'ctrl-y'),
              ('key', 'ctrl-x', 'ctrl-s'), ('see', 'Wrote')),
        emacs('emacs-search', 'T3.TXT', ('see', 'gamma'),
              ('key', 'ctrl-s'), ('type', 'gamma'), ('key', 'ret'), ('type', '!'),
              ('key', 'shift-alt-comma', 'shift-alt-5'), ('see', 'Replace string'),
              ('type', 'alpha'), ('key', 'ret'), ('see', 'Replace alpha with'), ('type', 'delta'), ('key', 'ret'),
              ('key', 'ctrl-x', 'ctrl-s'), ('see', 'Wrote')),
        emacs('emacs-lisp', '',
              ('key', 'shift-alt-semicolon'), ('type', '(* 6 7)'), ('key', 'ret'), ('see', '=> 42'),
              ('key', 'ctrl-c', 'ctrl-z'), ('see', 'EMACS-USER>'),
              ('type', '(expt 2 64)'), ('key', 'ret'), ('see', '18446744073709551616'),
              ('type', '(car 1)'), ('key', 'ret'), ('see', 'Condition of type TYPE-ERROR'),
              ('key', 'q'), ('see', 'Evaluation aborted on TYPE-ERROR')),
        # Lisp by the expression: a Ctrl-Alt key, the arguments shown on Space, completion
        emacs('emacs-sexp', 'T6.LSP', ('see', 'gamma'),
              ('key', 'right', 'right', 'right', 'right', 'right', 'right'),
              ('key', 'ctrl-alt-k'), ('key', 'end', 'ctrl-y', 'ret'),
              ('type', '(mapcar '), ('see', '(mapcar function'),
              ('type', "#'multiple-value-l"), ('key', 'alt-slash'), ('see', 'multiple-value-list'),
              ('key', 'ctrl-x', 'ctrl-s'), ('see', 'Wrote')),
        # the inspector: a list, into its second element, back, and out
        emacs('emacs-inspect', '', ('key', 'ctrl-c', 'shift-i'), ('see', 'Inspect the value of'),
              ('type', '(list 1 (list 2 3 4))'), ('key', 'ret'), ('see', 'list of length 2'),
              ('key', 'down', 'ret'), ('see', 'list of length 3'),
              ('key', 'l'), ('see', 'list of length 2'), ('key', 'q'), ('gone', 'list of length 2')),
        # the commands that ask the running Lisp: compile and load, go to a definition,
        # expand, trace, disassemble, describe, apropos, undefine; then the REPL's
        # package, emptying it, and finding an old input
        emacs('emacs-lisp-tools', 'T7.LSP', ('see', 'seven-up'),
              ('key', 'ctrl-c', 'ctrl-k'), ('see', 'compiled and loaded', 120),
              ('key', 'ctrl-c', 'i'), ('see', 'Go to definition (4 here)'), ('type', 'caller'), ('key', 'ret'),
              ('see', 'caller, line 4'),
              ('key', 'down', 'right', 'right'), ('key', 'ctrl-c', 'ret'),
              ('see', '(progn (seven-up 1) (seven-up 1))'), *to_buffer('T7.LSP'),
              ('gone', '(progn (seven-up 1) (seven-up 1))'),
              ('key', 'ctrl-c', 'ctrl-t'), ('see', 'Trace, or stop tracing'), ('type', 'seven-up'), ('key', 'ret'),
              ('see', 'seven-up is traced'),
              ('key', 'shift-alt-semicolon'), ('type', '(cl-user::caller)'), ('key', 'ret'),
              ('see', '(more in *output*)'), *to_buffer('*output*'), ('see', 'SEVEN-UP 1)'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-c', 'ctrl-t'), ('see', 'Trace, or stop tracing'), ('type', 'seven-up'), ('key', 'ret'),
              ('see', 'seven-up is no longer traced'),
              ('key', 'ctrl-c', 'ctrl-t'), ('see', 'Trace, or stop tracing'), ('type', 'seven-up'), ('key', 'ret'),
              ('see', 'seven-up is traced'), *meta_x('untrace-all'), ('see', 'Nothing is traced'),
              # (so asking again traces it, where it would have stopped tracing it)
              ('key', 'ctrl-c', 'ctrl-t'), ('see', 'Trace, or stop tracing'), ('type', 'seven-up'), ('key', 'ret'),
              ('see', 'seven-up is traced'), *meta_x('untrace-all'), ('see', 'Nothing is traced'),
              # everything inside expanded too: the last expression of the file
              ('key', 'shift-alt-dot', 'left', 'left'), ('key', 'ctrl-c', 'alt-m'),
              ('see', '(progn (progn 5 5) (progn 5 5))'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-c', 'alt-d'), ('see', 'Disassemble:'), ('type', 'seven-up'), ('key', 'ret'),
              ('see', 'disassembly for'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-c', 'ctrl-d', 'd'), ('see', 'Describe symbol:'), ('type', 'twice'), ('key', 'ret'),
              ('see', 'names a macro'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-c', 'ctrl-d', 'a'), ('see', 'Apropos:'), ('type', 'seven-u'), ('key', 'ret'),
              ('see', 'with seven-u in the name'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-c', 'ctrl-u'), ('see', 'Undefine:'), ('type', 'caller'), ('key', 'ret'),
              ('see', 'caller is no longer defined'),
              ('key', 'ctrl-c', 'alt-p'), ('see', 'REPL package: common-lisp-user'), ('key', 'ret'),
              ('see', 'The REPL is in COMMON-LISP-USER'),
              ('key', 'ctrl-c', 'ctrl-z'), ('see', 'CL-USER>'),
              ('type', '(seven-up 1000)'), ('key', 'ret'), ('see', '1007'),
              ('type', '(list 3 4)'), ('key', 'ret'), ('see', '(3 4)'),
              ('key', 'ctrl-c', 'alt-o'), ('gone', '1007'),
              ('key', 'alt-r'), ('see', 'Recall the input containing'), ('type', 'seven'), ('key', 'ret'),
              ('see', '(seven-up 1000)'), ('key', 'ret'), ('see', '1007')),
        # tests/editor.lisp: the Lisp commands tried on buffers, inside the editor
        emacs('emacs-lisp-edit', '', ('key', 'ctrl-c', 'ctrl-l'), ('see', 'Load file'),
              ('type', 'ED.LSP'), ('key', 'ret'), ('see', '=> T', 120)),
        emacs('emacs-dired', '',
              ('key', 'ctrl-x', 'd'), ('see', 'Dired (directory)'), ('key', 'ret'), ('see', 'Directory of D:\\SBCL\\'),
              ('see', 'DEMO.BAT'), ('host', dired_colours(True)),
              ('key', 'shift-alt-dot'), ('host', dired_colours(True, ('T7.LSP', 'T9.FAS', 'T9.BAK'))),
              ('key', 'shift-alt-comma'),
              *meta_x('dired-toggle-colours'), ('see', 'Dired colours are off'), ('host', dired_colours(False)),
              *meta_x('dired-toggle-colours'), ('see', 'Dired colours are on'), ('host', dired_colours(True)),
              # v views the file on the line: read-only, q comes back and the buffer is gone
              ('key', 'ctrl-s'), ('type', 'T2.TXT'), ('key', 'ret'), ('key', 'v'),
              ('see', '%% T2.TXT'), ('see', '(Text View)'), ('see', 'three'),
              # (a minor mode: C-h m says so, and lists its keys)
              ('key', 'ctrl-h', 'm'), ('see', 'The minor mode View is on'), ('see', 'view-quit'),
              *to_buffer('T2.TXT'), ('see', '(Text View)'),
              ('key', 'x'), ('see', 'T2.TXT is read-only'),
              ('key', 'q'), ('see', '(Dired)'), ('gone', '(Text View)'),
              # e makes it a buffer to edit; v on a directory goes into it, as RET does
              ('key', 'v'), ('see', '(Text View)'), ('key', 'e'), ('see', 'T2.TXT can be edited'),
              ('see', '-- T2.TXT'), *to_buffer('D:\\SBCL\\'),
              ('key', 'shift-alt-comma'), ('key', 'ctrl-s'), ('type', 'CONTRIB'), ('key', 'ret'), ('key', 'v'),
              ('see', 'Directory of D:\\SBCL\\CONTRIB\\'),
              ('key', 'f1'), ('see', 'In Dired mode'), ('see', 'dired-find-file')),
        # modes: one derived from Lisp mode, with a key, a file extension and a hook of its own;
        # its parent's hook runs too (and turns a minor mode on); a buffer-local variable
        emacs('emacs-modes', '',
              ('key', 'shift-alt-semicolon'),
              ('type', '(define-major-mode scheme-mode lisp-mode (:lighter "Scheme" :extensions ("scm") '
                       ':keys (("<f9>" end-of-buffer))) "For Scheme.")'),
              ('key', 'ret'), ('see', '=> SCHEME-MODE'),
              ('key', 'shift-alt-semicolon'), ('type', '(defvar-local width 70 "A width.")'), ('key', 'ret'),
              ('see', '=> WIDTH'),
              ('key', 'shift-alt-semicolon'), ('type', "(add-hook 'scheme-mode-hook (lambda () (setf width 60)))"),
              ('key', 'ret'),
              ('key', 'shift-alt-semicolon'), ('type', "(add-hook 'lisp-mode-hook 'view-mode)"), ('key', 'ret'),
              ('key', 'ctrl-x', 'ctrl-f'), ('see', 'Find file'), ('type', 'T8.SCM'), ('key', 'ret'),
              ('see', '%% T8.SCM'), ('see', '(Scheme View)'), ('see', '(define (f x)'),
              ('key', 'f9'), ('see', 'L3 C0'),
              ('key', 'shift-alt-semicolon'), ('type', '(list width view-mode)'), ('key', 'ret'), ('see', '=> (60 T)'),
              ('key', 'ctrl-h', 'm'), ('see', 'is in Scheme mode (scheme-mode)'),
              ('see', 'It is derived from Lisp mode (lisp-mode)'), ('see', 'scheme-mode-hook'),
              ('see', 'width = 60'), ('see', 'The minor mode View is on'),
              ('key', 'ctrl-h', 'v'), ('see', 'Describe variable'), ('type', 'width'), ('key', 'ret'),
              ('see', 'width is a buffer-local variable'), ('see', 'Its default: 70'),
              *to_buffer('*scratch*'),
              ('key', 'shift-alt-semicolon'), ('type', '(list width view-mode)'), ('key', 'ret'), ('see', '=> (70 NIL)'),
              # a change of major mode is a fresh start: the minor mode goes off, the value is forgotten
              *to_buffer('T8.SCM'), *meta_x('fundamental-mode'), ('see', 'Text mode'), ('see', '-- T8.SCM'),
              ('see', '(Text)'),
              ('key', 'shift-alt-semicolon'), ('type', '(list width view-mode)'), ('key', 'ret'), ('see', '=> (70 NIL)'),
              # a globalized minor mode: Omit mode in every Dired buffer, one opened afterwards too
              *meta_x('global-dired-omit-mode'), ('key', 'ctrl-x', 'd'), ('see', 'Dired (directory)'), ('key', 'ret'),
              ('see', '(Dired Omit)'), ('see', ' omitted, by name')),
        # Omit mode, a minor mode of Dired's: M-o hides the backups and compiled files, and shows them
        emacs('emacs-dired-omit', '',
              ('key', 'ctrl-x', 'd'), ('see', 'Dired (directory)'), ('key', 'ret'), ('see', 'Directory of D:\\SBCL\\'),
              ('key', 'shift-alt-dot'), ('see', 'T9.FAS'), ('see', 'T9.BAK'), ('see', '(Dired)'),
              ('key', 'alt-o'), ('see', 'Omit mode is on: '), ('see', '(Dired Omit)'),
              ('gone', 'T9.FAS'), ('gone', 'T9.BAK'), ('see', 'T7.LSP'),
              ('key', 'shift-alt-comma'), ('see', ' omitted, by name'),
              ('key', 'ctrl-h', 'm'), ('see', 'The minor mode Omit is on'), *to_buffer('D:\\SBCL\\'),
              ('key', 'alt-o'), ('see', 'Omit mode is off'), ('see', '(Dired)'),
              ('key', 'shift-alt-dot'), ('see', 'T9.FAS'),
              # ... and it is only for Dired; a minor mode made on the spot, a global one, has its key
              *to_buffer('*scratch*'), *meta_x('dired-omit-mode'), ('see', 'Omit mode is for Dired buffers'),
              ('key', 'shift-alt-semicolon'),
              ('type', '(define-minor-mode top-mode (:lighter "Top" :global t '
                       ':keys (("<f9>" end-of-buffer))) "Test.")'),
              ('key', 'ret'), *meta_x('top-mode'), ('see', 'Top mode is on'), ('see', '(Lisp Top)'),
              ('key', 'shift-alt-comma', 'f9'), ('see', 'L4 C0'),
              *meta_x('top-mode'), ('see', 'Top mode is off'), ('gone', '(Lisp Top)')),
        # the screen's size: 50 lines and back in place, kept across an example, 28 by name
        emacs('emacs-lines', 'README.TXT',
              ('key', 'f11'), ('see', '80 columns, 50 lines'), ('host', screen_rows(50)),
              ('key', 'ctrl-x', '2'), ('host', screen_rows(50)), ('key', 'ctrl-x', '1'),
              *meta_x('run-example'), ('type', 'fire'), ('key', 'ret'), ('gfx', 0x13, 20), ('key', 'esc'),
              ('see', 'fire finished'), ('host', screen_rows(50)),
              *meta_x('set-screen-size'), ('see', 'Screen size (80x25 80x28 80x50'), ('type', '80x28'),
              ('key', 'ret'), ('see', '80 columns, 28 lines'), ('host', screen_rows(28)),
              # (from any other size, F11 goes to 25 lines)
              ('key', 'f11'), ('see', '80 columns, 25 lines'), ('host', screen_rows(25))),
        # the editor documents itself: the commands behind C-h
        emacs('emacs-help', 'T7.LSP', ('see', 'seven-up'),
              ('key', 'ctrl-h', 'a'), ('see', 'Apropos command'), ('type', 'window'), ('key', 'ret'),
              ('see', 'in the name'), ('see', 'split-window   (C-x 2)'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-h', 'w'), ('see', 'Where is command'), ('type', 'save-buffer'), ('key', 'ret'),
              ('see', 'save-buffer is on <f2>, C-x C-s'),
              ('key', 'ctrl-h', 'c', 'ctrl-x', '2'), ('see', 'C-x 2 runs split-window'),
              ('key', 'ctrl-x', '1'),
              ('key', 'ctrl-h', 'k', 'ctrl-x', 'ctrl-f'), ('see', 'C-x C-f runs the command find-file'),
              ('see', 'Keys: C-x C-f'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-h', 'm'), ('see', 'is in Lisp mode'), ('see', 'Its own keys'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-h', 'v'), ('see', 'Describe variable'), ('type', '*screen-size*'), ('key', 'ret'),
              ('see', '(80 25 3)'), ('see', 'The text screen wanted'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-h', 'd'), ('see', 'Apropos documentation'), ('type', 'echo area'), ('key', 'ret'),
              ('see', 'view-echo-area-messages'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-h', 'x'), ('see', 'Describe command'), ('type', 'where-is'), ('key', 'ret'),
              ('see', 'where-is is a command'), ('see', 'Keys: C-h w'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-h', 'l'), ('see', 'keys typed, oldest first'), ('see', 'C-h l'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-h', 'e'), ('see', 'save-buffer is on <f2>, C-x C-s'), *to_buffer('T7.LSP'),
              ('key', 'ctrl-h', 'shift-slash'), ('see', 'apropos-command'), ('see', 'F1 lists every key')),
        emacs('emacs-extras', 'README.TXT',
              ('key', 'f12'), ('see', 'Lisp calculator'), ('type', '6*7'), ('key', 'ret'), ('see', '6*7 = 42'),
              ('key', 'esc'), ('gone', 'Lisp calculator'),
              *meta_x('blocks'), ('see', 'Score'), ('key', 'left', 'up', 'spc', 'q'), ('gone', 'Score'),
              *meta_x('snake'), ('see', 'steer'), ('key', 'down', 'q'), ('gone', 'steer'),
              *meta_x('hex-find-file'), ('see', 'Hex file'), ('type', 'DEMO.BAT'), ('key', 'ret'),
              ('see', '00000000  40 65 63 68 6F'),
              *meta_x('run-example'), ('type', 'fire'), ('key', 'ret'), ('gfx', 0x13, 20), ('key', 'esc'),
              ('see', 'fire finished'),
              # the hot key again, now that an example has had the keyboard
              ('key', 'f12'), ('see', 'Lisp calculator'), ('key', 'esc'), ('gone', 'Lisp calculator')),
        # an evaluation that never ends: Ctrl-Break opens the editor's debugger on it
        emacs('emacs-interrupt', '', ('type', '(loop)'), ('key', 'ctrl-x', 'ctrl-e'), ('wait', 2),
              ('hold', 'ctrl-pause'), ('see', 'Interactive interrupt'), ('see', 'Debugger: restart number'),
              ('key', 'q'), ('gone', 'Debugger: restart number'),
              ('hold', 'ctrl-alt-shift'), ('wait', 1),             # at rest: nothing to stop
              ('key', 'shift-alt-semicolon'), ('type', '(* 6 7)'), ('key', 'ret'), ('see', '=> 42')),
        dict(kind='dos', name='emacs-web', steps=lambda host: emacs(
            '', '', *meta_x('browse-url'), ('see', 'Address'), ('type', 'http://10.0.2.2:%d/' % host.web_port),
            ('key', 'ret'), ('see', 'The first page'), ('key', 'tab', 'ret'), ('see', 'You have arrived'),
            ('key', 'b'), ('see', 'The first page'))['steps']),
        dict(kind='dos', name='emacs-irc', steps=lambda host: emacs(
            '', '', *meta_x('irc'), ('see', 'IRC server'), ('type', '10.0.2.2:%d' % host.irc_port), ('key', 'ret'),
            ('see', 'Nick'), ('type', 'dosed'), ('key', 'ret'), ('see', 'Channel to join'), ('type', '#test'),
            ('key', 'ret'), ('see', 'welcome to #test'), ('type', 'from the editor'), ('key', 'ret'),
            ('see', 'you said: from the editor'))['steps']),
        # M-x live: the editor goes to the second display, the VGA shows what the code draws
        emacs('emacs-live', '',
              *meta_x('live'), ('gfx', None, 3), ('host', second_display(3)),
              ('wait', 2), ('key', 'alt-x'), ('wait', 1), ('type', 'live-stop'), ('key', 'ret'),
              ('host', text_mode_again)),
    ]


def second_display(colours):
    def step(session):
        count = session.m.picture('second.ppm', 'vga2')
        if count < colours:
            raise Failure('the second display shows %d colours' % count)
    return step


def display_is(width, height, colours):
    def step(session):
        for attempt in range(20):
            count = session.m.picture('look.ppm')
            if session.m.picture_size == (width, height) and count >= colours:
                return
            time.sleep(0.5)
        raise Failure('the display is %d by %d with %d colours' % (session.m.picture_size + (count,)))
    return step


def dired_colours(on, need=('CONTRIB', 'DEMO.BAT')):
    """Dired's lines are coloured by kind of file (or, with ON false, only
    the directories are): the colour of each name on the screen is looked at."""
    kinds = (('CONTRIB', 9, 11), ('DEMO.BAT', 10, 7), ('T7.LSP', 15, 7), ('T9.FAS', 8, 7),
             ('T9.BAK', 8, 7), ('README.TXT', 7, 7))
    def step(session):
        time.sleep(1)
        screen = session.m.screen()
        cells = session.m.memory(0xB8000, 2 * 80 * len(screen))
        seen = set()
        for y, row in enumerate(screen[:-2]):
            for name, coloured, plain in kinds:
                if row.rstrip().upper().endswith('  ' + name):
                    x = len(row.rstrip()) - 1
                    colour, want = cells[2 * (80 * y + x) + 1] & 15, coloured if on else plain
                    seen.add(name)
                    if colour != want:
                        raise Failure('%s is in colour %d, not %d' % (name, colour, want))
        if not set(need) <= seen:
            raise Failure('not on the screen: %s' % ' '.join(sorted(set(need) - seen)))
    return step


def screen_rows(rows):
    """The text screen has ROWS rows, and the editor fills it: a mode line
    on the last but one."""
    def step(session):
        time.sleep(1)
        screen = session.m.screen()
        if len(screen) != rows:
            raise Failure('the screen has %d rows, not %d' % (len(screen), rows))
        if 'README.TXT' not in screen[rows - 2] or ' L' not in screen[rows - 2]:
            raise Failure('no mode line on row %d: "%s"' % (rows - 2, screen[rows - 2]))
    return step


def text_mode_again(session):            # the VGA is 720 dots across in text mode
    for attempt in range(40):
        session.m.picture('look.ppm')
        if session.m.picture_size[0] == 720:
            return
        time.sleep(0.5)
    raise Failure('the VGA did not go back to text: it is %d by %d' % session.m.picture_size)


# ---- what the tests must have left behind: (test, check of the stopped machine)
def sprite_saved(machine, host):
    found = machine.read('SBCL/SPRITE.DAT')
    if found is None or len(found) != 256:
        raise Failure('SPRITE.DAT was not saved')
    white = [i for i, colour in enumerate(found) if colour == 15]
    others = [i for i, colour in enumerate(found) if colour not in (0, 15)]
    # the cleared sprite, with the middle cell (8, 8) painted by Space and at least one
    # more, to its right, by the mouse button
    if others or 8 + 16 * 8 not in white or len(white) < 2:
        raise Failure('SPRITE.DAT has white at %s and other colours at %s' % (white, others[:8]))


def mouse_edited(machine, host):
    found = machine.read('SBCL/T5.TXT')
    lines = (found or b'').decode('latin-1').split('\r\n')
    if len(lines) < 21 or lines[12][40:41] != 'X':
        raise Failure('T5.TXT: the click did not put point at line 13, column 40: %r' % lines[12:13])
    # what was dragged over and copied, from the X on, is at the end as well
    tail = lines[20:]
    if not tail[0].startswith('X') or len(tail) < 2:
        raise Failure('T5.TXT: the dragged text was not copied to the end: %r' % tail)


def file_is(name, expected):
    def check(machine, host):
        found = machine.read('SBCL/' + name)
        if found != expected:
            raise Failure('%s is %r' % (name, found))
    return check


def printed(machine, host):
    import os
    path = os.path.join(machine.work, 'lpt.txt')
    if not os.path.exists(path) or os.path.getsize(path) < 78:
        raise Failure('nothing reached the printer')


def editor_tests_passed(machine, host):
    results = (machine.read('SBCL/ED.TXT') or b'').decode('latin-1').splitlines()
    if len(results) < 13 or not all(line.startswith('ok ') for line in results):
        raise Failure('the editor tests: %r' % results)


AFTER = [
    ('ports', printed),
    ('emacs-edit', file_is('T1.TXT', b'Hello, DOS.\r\n(+ 40 2)\r\n')),
    ('emacs-kill-yank', file_is('T2.TXT', b'two\r\none\r\nthree\r\n')),
    ('emacs-final-newline', file_is('T4.TXT', b'one\r\ntwo\r\nthree\r\n')),
    ('sprite', sprite_saved),
    ('emacs-mouse', mouse_edited),
    ('emacs-sexp', file_is('T6.LSP', b"(list  gamma)(alpha beta)\r\n(mapcar #'multiple-value-list\r\n")),
    ('emacs-lisp-edit', editor_tests_passed),
    ('emacs-search', file_is('T3.TXT', b'delta beta\r\ndelta gamma!\r\n')),
]
