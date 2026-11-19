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
import socket, time, urllib.request
from dosvm import Failure, DOS_PROMPT

# Files put in D:\SBCL before the machine starts (with DOS line endings).
FILES = {
    'T2.TXT': 'one\ntwo\nthree\n',
    'T3.TXT': 'alpha beta\nalpha gamma\n',
    'T4.TXT': 'one\ntwo\n',
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
    return dos(name, ('line', ('emacs ' + file).strip()), ('see', ' -- '), *steps,
               ('key', 'ctrl-x', 'ctrl-c'))


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
        text('pciscan', 'Host bridge', 'Ethernet controller'),
        text('ports', 'keyboard answered FAh', 'LPT1 is at 378h', 'printed 78 characters'),
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
        example('floppy', ('see', 'no disk in A:'), leave=None),
        example('timer', ('see', 'Against the clock chip'), ('key', 'spc'), ('wait', 1)),
        example('blaster-16', ('see', 'DMA channel 5', 150), ('see', 'interrupts handled', 90),
                example='blaster', args='"16"', leave=None, patience=200),
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
        example('flip', ('gfx', 0x13, 8), ('key', 'spc'), ('wait', 1), ('key', 'spc'), ('wait', 1)),
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
        emacs('emacs-dired', '',
              ('key', 'ctrl-x', 'd'), ('see', 'Dired (directory)'), ('key', 'ret'), ('see', 'Directory of D:\\SBCL\\'),
              ('see', 'DEMO.BAT'), ('key', 'f1'), ('see', 'describe-bindings')),
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


AFTER = [
    ('ports', printed),
    ('emacs-edit', file_is('T1.TXT', b'Hello, DOS.\r\n(+ 40 2)\r\n')),
    ('emacs-kill-yank', file_is('T2.TXT', b'two\r\none\r\nthree\r\n')),
    ('emacs-final-newline', file_is('T4.TXT', b'one\r\ntwo\r\nthree\r\n')),
    ('sprite', sprite_saved),
    ('emacs-mouse', mouse_edited),
    ('emacs-search', file_is('T3.TXT', b'delta beta\r\ndelta gamma!\r\n')),
]
