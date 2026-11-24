#!/usr/bin/env python3
"""The display cards QEMU does not have: the editor is started in DOSBox-X
as each kind of machine it can be -- a VGA with a VESA BIOS, a plain VGA,
an EGA, a CGA, a Hercules card, an MDA -- and says what it found and what
it drew.

    tests/dosbox.py                 every machine
    tests/dosbox.py ega hercules    only these

For each machine this checks that the library names the card rightly and
finds its text memory (B0000h on the monochrome cards), that the editor
offers the sizes the card has and no others, and that in each size the
mode line is where it should be, in the text memory, for the columns and
lines the size has.  It also checks, on each, the clipboard the emulator
shares with DOS programs (the text comes back; whether it reaches the
Mac's clipboard needs DOSBox-X's window, which this does not open), a file
read from a Mac folder mounted as a drive, and that an example that needs
a VGA says so where there is none.

Like run.py, it tests what "./run-dos.sh build" last built.  Needs
dosbox-x, mtools and Python 3.  No window is opened.
"""
import os, re, shutil, subprocess, sys, time

here = os.path.dirname(os.path.abspath(__file__))
project = os.path.dirname(here)
tools = os.environ.get('SBCL_DOS_TOOLS') or os.path.dirname(project)
vm = os.path.join(tools, 'vm')
work = os.path.join(vm, 'dosbox-test')

VGA = ['80x25', '80x28', '80x50']
MACHINES = {
    # machine: (the card, text memory, the sizes it must offer, and whether it may offer more)
    'svga_s3': ('VGA', 'B8000', VGA + ['80x60', '132x25', '132x43', '132x50', '132x60'], False),
    'svga_et4000': ('VGA', 'B8000', VGA, True),
    'vgaonly': ('VGA', 'B8000', VGA, False),
    'ega': ('EGA', 'B8000', ['80x25', '80x43'], False),
    'cga': ('CGA', 'B8000', ['80x25'], False),
    'hercules': ('HERCULES MDA', 'B0000', ['80x25'], False),
    'mda': ('MDA', 'B0000', ['80x25'], False),
}

# What the editor is told to do: EMACSRC.LSP is loaded as it starts, and
# puts a key of its own making in front of the keyboard.
SCRIPT = r'''(in-package :doslib-emacs)
(define-command report-display ()
  (with-open-file (out "REPORT.TXT" :direction :output :if-exists :supersede)
    (handler-case
        (flet ((modeline ()
                 (let ((base (+ (text-memory-address) (* 2 *columns* (- *lines* 2)))))
                   (values (coerce (loop for i below *columns*
                                         collect (code-char (doslib:peek (+ base (* 2 i)))))
                                   'string)
                           (doslib:peek (1+ base))))))
          (format out "adapter=~A~%memory=~X~%mono=~A~%" (display-adapter) (text-memory-address)
                  (and (monochrome-display-p) t))
          (format out "file=~A~%" (and (search "display-cells" (format nil "~{~A~%~}" (coerce (buf-lines *buf*) 'list))) t))
          (format out "sizes=~{~{~Dx~D~*~}~^ ~}~%" (screen-sizes))
          (dolist (size (screen-sizes))
            (setf *screen-size* (copy-list size) *display-stale* t)
            (redisplay)
            (multiple-value-bind (text attribute) (modeline)
              (format out "size=~Dx~D got=~Dx~D bios=~Dx~D modeline=~A attribute=~2,'0X~%"
                      (first size) (second size) *columns* *lines* (text-columns) (text-lines)
                      (and (search "SCREEN.LISP" text) t) attribute)))
          (setf (clipboard-text) (format nil "one~%two"))
          (format out "clipboard=~A~%" (and (clipboard-present-p) (equal (clipboard-text) (format nil "one~%two"))))
          (push "killed text" *kill-ring*)
          (clipboard-export)
          (format out "kill=~A~%" (equal (clipboard-text) "killed text"))
          (setf (clipboard-text) "from elsewhere")
          (clipboard-import)
          (format out "yank=~A~%" (equal (first *kill-ring*) "from elsewhere"))
          (when (and (find-package "DOSLIB-EXAMPLES") (not (member (display-adapter) '(:vga :mcga))))
            (format out "refused=~A~%"
                    (and (search "display card"
                                 (with-output-to-string (*standard-output*)
                                   (funcall (find-symbol "RUN" "DOSLIB-EXAMPLES") "fire")))
                         t)))
          (format out "done=T~%"))
      (error (e) (format out "error=~A~%" e))))
  (text-mode)
  (sb-ext:exit))
(bind-key "<f9>" 'report-display)
(setf *unread* (list "<f9>"))
'''


def fill():
    """The DOS files, from the QEMU machine's disk, as "./run-dos.sh dosbox" has them."""
    os.makedirs(work, exist_ok=True)
    environment = dict(os.environ, MTOOLSRC=os.path.join(vm, 'mtoolsrc'))
    subprocess.run(['mcopy', '-s', '-m', '-o', '-Q', 'p:/SBCL', 'p:/DOSLIB', work + '/'],
                   env=environment, check=True, stdout=subprocess.DEVNULL)
    with open(os.path.join(work, 'SBCL', 'EMACSRC.LSP'), 'w', newline='\r\n') as f:
        f.write(SCRIPT)


def run(machine, seconds=240):
    report = os.path.join(work, 'SBCL', 'REPORT.TXT')
    if os.path.exists(report):
        os.remove(report)
    command = ['dosbox-x', '-silent', '-exit', '-fastlaunch', '-nolog', '-time-limit', str(seconds),
               '-set', 'dosbox machine=' + machine, '-set', 'dosbox memsize=512',
               '-set', 'cpu cputype=ppro_slow', '-set', 'cpu core=normal', '-set', 'cpu cycles=max',
               '-set', 'dos lfn=true', '-set', 'dos ver=7.1', '-set', 'dos files=200',
               '-set', 'dos clipboard api=true',
               '-c', 'mount d "%s"' % work, '-c', 'mount p "%s"' % project, '-c', 'd:', '-c', 'cd \\SBCL',
               '-c', 'emacs P:\\emacs\\screen.lisp', '-c', 'exit']
    subprocess.run(command, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if not os.path.exists(report):
        return None
    return open(report, encoding='latin-1').read().splitlines()


def check(machine, lines):
    """The complaints about what the editor reported on MACHINE."""
    card, memory, sizes, more = MACHINES[machine]
    if lines is None:
        return ['the editor wrote no report']
    wrong = []
    values = dict(line.split('=', 1) for line in lines if '=' in line and not line.startswith('size='))
    if 'error' in values:
        return ['an error: ' + values['error']]
    if values.get('done') != 'T':
        wrong.append('the report stops short')
    if values.get('adapter') not in card.split():
        wrong.append('the card is said to be %s, not %s' % (values.get('adapter'), card))
    if values.get('memory') != memory:
        wrong.append('text memory at %s, not %s' % (values.get('memory'), memory))
    if (values.get('mono') == 'T') != (memory == 'B0000'):
        wrong.append('monochrome: %s' % values.get('mono'))
    offered = values.get('sizes', '').split()
    if offered != sizes and not (more and offered[:len(sizes)] == sizes):
        wrong.append('sizes offered: %s, not %s' % (' '.join(offered), ' '.join(sizes)))
    for line in lines:
        match = re.match(r'size=(\S+) got=(\S+) bios=(\S+) modeline=(\S+) attribute=(\S+)', line)
        if match:
            size, got, bios, modeline, attribute = match.groups()
            if not (size == got == bios):
                wrong.append('asked for %s: the editor has %s, the BIOS %s' % (size, got, bios))
            if modeline != 'T':
                wrong.append('%s: no mode line where it should be' % size)
            if memory == 'B0000' and attribute != '70':
                wrong.append('%s: the mode line has attribute %s on a monochrome card' % (size, attribute))
    for name, what in (('file', 'the file from the Mac folder was not read'),
                       ('clipboard', 'text put on the clipboard did not come back'),
                       ('kill', 'a kill did not reach the clipboard'),
                       ('yank', 'the clipboard was not taken for a yank')):
        if values.get(name) != 'T':
            wrong.append(what)
    if card != 'VGA' and values.get('refused') != 'T':
        wrong.append('an example that needs a VGA did not say so')
    return wrong


def main():
    names = sys.argv[1:] or list(MACHINES)
    for name in names:
        if name not in MACHINES:
            sys.exit('dosbox.py: no machine named %s (there are: %s)' % (name, ' '.join(MACHINES)))
    if not shutil.which('dosbox-x'):
        sys.exit('dosbox.py: dosbox-x is not installed')
    fill()
    failed = []
    for name in names:
        started = time.time()
        wrong = check(name, run(name))
        if wrong:
            failed.append(name)
            print('FAIL  %-12s %s' % (name, '; '.join(wrong)), flush=True)
        else:
            print('ok    %-12s %d s' % (name, time.time() - started), flush=True)
    print('\n%d passed, %d failed%s' % (len(names) - len(failed), len(failed),
                                        ': ' + ' '.join(failed) if failed else ''))
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main())
