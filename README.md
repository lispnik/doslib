# doslib

Common Lisp on DOS with SBCL: a library for the BIOS, DOS, I/O ports,
memory and video; example programs that use it; and an Emacs-like editor.

It needs the DOS port of SBCL with ASDF and, for the network examples and
nothing else, the `sb-bsd-sockets` contrib and a packet driver.

## Systems

`doslib.asd` defines four ASDF systems.

| System | Directory | Package | What it is |
|---|---|---|---|
| `doslib` | `src/` | `doslib` | The library |
| `doslib/examples` | `examples/` | `doslib-examples`, plus one package per example | Example programs |
| `doslib/emacs` | `emacs/` | `doslib-emacs`, `doslib-emacs-user` | The editor |
| `doslib/emacs-extras` | `emacs-extras/` | `doslib-emacs` | More for the editor, built on the examples and the network |

Tell ASDF where the project is (the launcher's `SBCLRC` does this), then
load what you want:

```lisp
(push #p"D:/DOSLIB/" asdf:*central-registry*)
(asdf:load-system :doslib/examples)
```

## The library

```lisp
(asdf:load-system :doslib)
(use-package :doslib)
(video-mode #x13)                 ; BIOS video mode 13h: 320x200, 256 colours
(rainbow-palette)
(dotimes (i 200) (plot i i (1+ (mod i 255))))
(finish)                          ; wait for a key, back to text mode
```

| File | Provides |
|---|---|
| `machine.lisp` | `machine-code`, `int86` (software interrupts), `lo`, `hi`, `word16` |
| `ports.lisp` | `inb`, `outb`, `inw`, `outw`, `inl`, `outl`, `cmos` |
| `dos.lisp` | `real-int` (real-mode interrupts through DPMI), `dos-alloc`, `dos-free`, `dos-scratch`, `dos-string`, `linear`, `without-break-checking` (Ctrl-C as a key), `catching-interrupt` (Ctrl-Break) |
| `cpuid.lisp` | `cpuid` |
| `memory.lisp` | `peek`, `peek16`, `peek32` |
| `interrupts.lisp` | `get-vector`, `set-vector`, `code-selector`, `data-selector` |
| `text.lisp` | `text-put`, `text-box`, `save-text-screen`, `get-key`, `text-input`: text mode by direct video memory |
| `critical.lisp` | `catch-critical-errors`, `critical-error`: DOS device errors without "Abort, Retry, Fail?" |
| `dialog.lisp` | `restart-dialog`, `dialog-ask`: choose a restart from a box on the screen |
| `hotkey.lisp` | `install-hotkey`, `remove-hotkeys`, `poll-hotkeys`: keys taken through the BIOS keyboard intercept |
| `video.lisp` | `video-mode`, `with-video-mode`, `set-palette`, `rainbow-palette`, `plot`, `draw-line`, `*vga*`, `vesa-set-mode` |
| `input.lisp` | `ticks`, `wait-tick`, `key-pressed-p`, `wait-key`, `finish` |
| `sound.lisp` | `speaker-on`, `speaker-off`, `speaker-beep`: the PC speaker |
| `mouse.lisp` | `mouse-init`, `mouse-position`, `mouse-cell`, `mouse-pixel`, `mouse-show`, `mouse-hide`: the mouse driver |
| `disk.lisp` | `dos-read-sectors`, `bios-read-sectors`: disks, a sector at a time |
| `pci.lisp` | `pci-read`, `pci-write`, `pci-devices`: PCI configuration space |
| `serial.lisp` | `serial-open`, `serial-read-byte`, `serial-write-string`: serial ports, polled |
| `init.lisp` | `doslib-init`, run at load and whenever a saved core starts |

`doc/reference.md` lists every exported name with its arguments and what
it does; `sbcl --script doc/make-reference.lisp` makes it again from the
documentation strings in the sources.

The instructions Lisp cannot express (INT, IN, OUT, CPUID) are assembled
by hand into malloc'ed memory and called with `alien-funcall`.

### Interrupting a program

Ctrl-Break stops Lisp that is running, even a loop that calls nothing:
the condition `sb-sys:interactive-interrupt` is signalled there, and if
nothing handles it SBCL's debugger opens, with a restart that carries on.
Ctrl, Alt and Shift pressed together do the same, for keyboards that have
no Break key (`(setf (interrupt-chord) nil)` turns that off); so does
Ctrl-C, once DOS notices it, which is when the program next calls DOS.

```lisp
(catching-interrupt (loop))       ; => :INTERRUPTED, when you have had enough
```

`doslib-examples:run` catches the interrupt and puts the screen back in
text mode.  In the editor, Ctrl-Break during an evaluation opens the
editor's debugger on it, and at any other time stops the command that is
running.  This needs the DOS port's runtime of November 2026 or later
(`(interrupts-supported-p)` says whether it is there).

## The examples

```lisp
(asdf:load-system :doslib/examples)
(doslib-examples:list-examples)             ; what there is
(doslib-examples:menu)                      ; choose from a list
(doslib-examples:run "fire")                ; run one
(doslib-examples:run "wget" "http://example.com/")
(doslib-examples:build-executable "life" "life.exe")   ; a stand-alone program
```

Each example is one file with a package of its own,
`doslib-examples.<name>`, that exports `main`.

| Category | Examples |
|---|---|
| `graphics/` | bmpview fire font gcviz lsystem mandel modex paint plasma raycast raytrace sprite stars textfx turtle vesa wire3d |
| `system/` | asm asmdsl calc diskinfo dosinfo fatview intvec ivt mcbwalk memmap pciscan ports printer probes serial sysinfo |
| `lisp/` | bench hotfix life live restarts resume shell snake special windows |
| `sound/` | adlib blaster speaker |
| `network/` | browse files irc netpong ntp remote websrv wget |

`data/` holds files the examples read; `support/` holds the host-side
half of the serial example.

`blaster` wants a Sound Blaster (the launcher's QEMU machine has an
`sb16`); `sprite`, `paint` and `windows` want a mouse driver.

To add one, write a file with a `main` function, register it with
`doslib-examples:define-example`, and list it in `doslib.asd`.

## The editor

```lisp
(asdf:load-system :doslib/emacs)
(doslib-emacs:emacs)              ; or (doslib-emacs:emacs "file"), or (ed "file")
(doslib-emacs:build-emacs)        ; save it as EMACS.EXE
```

`F1` lists the keys. `C-c C-z` opens the REPL, `C-x d` is Dired, and
`C-x C-c` leaves. An error in evaluated code opens a debugger that lists
the restarts; so does Ctrl-Break, if the code will not stop. `M-.` goes to
the definition of the name at the cursor and `M-,` comes back; `C-_`
undoes and `M-_` redoes.

### The extras

`(asdf:load-system :doslib/emacs-extras)` adds these; the `EMACS.EXE` that
the launcher builds has them.

| Command | What it does |
|---|---|
| `M-x run-example` (`C-c x`) | Run an example, with completion on its name, and come back |
| `M-x quickload` (`C-c q`) | Load a system with Quicklisp, with completion; output in `*quickload*` |
| `F12` | The pop-up calculator (`M-x calculator-hotkey` turns it off) |
| `M-x restart-dialog-mode` | Errors in evaluated code offer their restarts in a box |
| `M-x hex-find-file`, `M-x hex-disk-sector` | A hex editor for files; disk sectors, read-only |
| `M-x browse-url` | A text browser for plain HTTP and Gopher: `TAB` to a link, `RET` follows, `b` back |
| `M-x irc` | An IRC client: a buffer per channel, filled while the editor is idle |
| `M-x blocks`, `M-x snake` | Games, in a buffer |
| `M-x live` | Live coding on two displays: the editor moves to the second, and the VGA shows what the code draws, redefined as you type (`M-x live-stop` ends it) |

`M-x live` needs a second display card: QEMU's and UTM's `secondary-vga`
(the launcher's machines have one), or a monochrome card beside the VGA.

A mode of your own can do what these do: an idle hook (`add-idle-hook`)
is called while the editor waits for a key, and properties on the mode's
name colour its lines and hear of its buffers being killed (see
`mode-hook` in `emacs/buffers.lisp`).

What you type at the REPL is read in the package `doslib-emacs-user`
(nickname `emacs-user`), which uses Lisp, `doslib` and everything the
editor exports. Extend the editor from there, or from `EMACSRC.LSP` in the
current directory:

```lisp
(define-command insert-date ()
  "Insert today's date."
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (declare (ignore s m h))
    (insert (format nil "~D-~2,'0D-~2,'0D" year month day))))
(bind-key "C-c d" 'insert-date)
```

## Running it

`./run-dos.sh` starts FreeDOS in QEMU with SBCL, ASDF and this project
installed in `D:\DOSLIB`; `./run-dos.sh utm` does the same for UTM, and
`./run-dos.sh build` compiles everything on the DOS machine. At the DOS
prompt, `demo` lists the examples, `demo plasma` runs one (`demo speaker
scale` passes it an argument), and `emacs` starts the editor. The comments at the top of the script say what it
needs: QEMU, mtools, a FreeDOS image, a packet driver, and SBCL built for
DOS.

## Tests

`tests/run.py` runs the regression tests, on DOS: it boots a copy of the
launcher's disks in QEMU without a window, types at the machine and reads
its screen.

```
./run-dos.sh update && ./run-dos.sh build     # the tests run what was last built
tests/run.py                                  # everything: about twelve minutes
tests/run.py unit fire emacs-edit             # some of it
tests/run.py --list
```

| Kind | What it does |
|---|---|
| `unit` | `tests/unit.lisp`, loaded into Lisp on the DOS machine: the library, function by function |
| `example` | Starts each example, checks what it shows (text, or the video mode and that something was drawn), operates it where that is cheap, and leaves it; the network examples talk to servers the runner starts on this computer |
| `dos` | Programs typed at the DOS prompt: `DEMO`, a saved program, and `EMACS.EXE` driven through its keys, with the files it saved compared afterwards |

The tests are listed in `tests/cases.py`; `tests/run.py --look NAME` runs
an example and prints what it puts on the screen, which is how to start
a test for a new one.  The screen of a test that fails is kept in the
launcher's `vm/test` directory.  It needs QEMU, mtools and Python 3.

## Quicklisp

Quicklisp works on the DOS port, given the `sb-posix` subset that
`make-dos-contrib.sh sb-posix` builds in the SBCL tree. With
`quicklisp.lisp` on the DOS disk (the launcher copies it from
`dl-quicklisp/` if it is there), `ql` at the DOS prompt installs Quicklisp
the first time and starts SBCL with it loaded after that:

```lisp
(ql:quickload "cl-ppcre")
(cl-ppcre:regex-replace-all "(\\w+) on (\\w+)" "Lisp on DOS" "\\2 runs \\1")   ; => "DOS runs Lisp"
```

Libraries are fetched over plain HTTP, so there is no TLS involved; ones
that need foreign libraries or threads will not load.

Screenshots are in `doc/shots/`.
