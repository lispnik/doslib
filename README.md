# doslib

Common Lisp on DOS with SBCL: a library for the BIOS, DOS, I/O ports,
memory and video; example programs that use it; and an Emacs-like editor.

It needs the DOS port of SBCL with ASDF and, for the network examples and
nothing else, the `sb-bsd-sockets` contrib and a packet driver. The port is
the `dos-port` branch of <https://github.com/lispnik/sbcl>;
[Building SBCL for DOS](#building-sbcl-for-dos) below says how to make it.

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
| `dos.lisp` | `real-int`, `real-call` (real-mode interrupts and routines through DPMI), `dos-alloc`, `dos-free`, `dos-scratch`, `dos-string`, `linear`, `without-break-checking` (Ctrl-C as a key), `catching-interrupt` (Ctrl-Break) |
| `cpuid.lisp` | `cpuid` |
| `memory.lisp` | `peek`, `peek16`, `peek32`; `map-physical`, `unmap-physical`, `with-physical-memory`: a device's memory made addressable |
| `interrupts.lisp` | `get-vector`, `set-vector`, `code-selector`, `data-selector` |
| `text.lisp` | `text-put`, `text-box`, `save-text-screen`, `get-key`, `text-input`: text mode by direct video memory |
| `critical.lisp` | `catch-critical-errors`, `critical-error`: DOS device errors without "Abort, Retry, Fail?" |
| `dialog.lisp` | `restart-dialog`, `dialog-ask`: choose a restart from a box on the screen |
| `hotkey.lisp` | `install-hotkey`, `remove-hotkeys`, `poll-hotkeys`: keys taken through the BIOS keyboard intercept |
| `video.lisp` | `video-mode`, `with-video-mode`, `set-palette`, `rainbow-palette`, `plot`, `draw-line`, `*vga*`, `vesa-set-mode`, `vesa-mode-info` |
| `gtext.lisp` | `graphics-text`, `draw-glyphs`, `glyph-row`: text in the graphics modes, in the BIOS's 8 by 8 characters |
| `modex.lisp` | `mode-x`, `mode-x-text`, `vga-planes`, `vga-display-start`, `vga-sequencer`, `vga-crtc`, `vga-graphics`, `vga-attribute`: the VGA's registers, and 320x240 in planes |
| `input.lisp` | `ticks`, `wait-tick`, `key-pressed-p`, `wait-key`, `finish`; `timer-count`, `wait-frame`: time finer than the tick |
| `sound.lisp` | `speaker-on`, `speaker-off`, `speaker-beep`: the PC speaker |
| `mouse.lisp` | `mouse-init`, `mouse-position`, `mouse-cell`, `mouse-pixel`, `mouse-show`, `mouse-hide`: the mouse driver |
| `disk.lisp` | `dos-read-sectors`, `bios-read-sectors`: disks, a sector at a time |
| `irq.lisp` | `with-irq-handler`, `install-irq-handler`, `irq-count`, `irq-read-byte`: handlers for hardware interrupts, made to order |
| `keystate.lisp` | `with-key-state`, `key-down-p`, `keys-down`: which keys are held down, from every scan code the keyboard sends |
| `dma.lisp` | `dma-start`, `dma-stop`, `dma-address`, `dma-count`, `with-dma-memory`: the DMA controllers |
| `blaster.lisp` | `blaster-present-p`, `blaster-write`, `blaster-read`, `blaster-version`, `with-blaster-interrupt`, `blaster-interrupts`: the Sound Blaster's settings, its processor and its interrupt |
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
| `graphics/` | blit bmpview compiled copper dirty fire flip font gcviz latch lfb lsystem mandel mode12 modex paint palette parallax picview plasma raycast raytrace rotozoom scroll solid3d split sprite stars textfont textfx tiles tunnel turtle vesa wire3d |
| `system/` | alarm asm asmdsl calc cdrom diskinfo dosinfo fatview floppy intvec ivt joystick keys mcbwalk memmap pciscan ports printer probes serial speed step sysinfo term timer xms |
| `lisp/` | bench hotfix life live restarts resume shell snake special windows |
| `sound/` | adlib blaster dmascope midi mixer modplay record speaker spectrum tune |
| `network/` | browse files irc netpong ntp remote websrv wget |

`data/` holds files the examples read; `support/` holds the host-side
half of the serial example.

Some show one hardware technique each:

| Technique | Examples |
|---|---|
| Blitting | `blit` (a back buffer), `dirty` (dirty rectangles), `compiled` (sprites as machine code) |
| Mode X | `flip` (page flipping), `scroll` (hardware scrolling), `latch` (copies inside video memory) |
| Other video | `lfb` (the VESA linear framebuffer), `copper` (a palette entry changed on every scanline) |
| Scrolling worlds | `tiles` (a tile map, only the new column drawn), `parallax` (three layers and sprites, redrawn and flipped), `split` (a status bar by the line-compare register) |
| Palette and fonts | `palette` (cycling, fades and cross-fades with no pixel redrawn), `textfont` (glyphs redefined in plane 2, 80 by 50) |
| Planar 16 colours | `mode12` (640x480 through the graphics controller: bit mask, set/reset, write modes, latches, XOR) |
| Demo-scene effects | `rotozoom`, `tunnel`, `solid3d` (filled, shaded and texture-mapped polygons) |
| Picture files | `bmpview`, `picview` (PCX and GIF decoders in Lisp) |
| Sound engines | `mixer` (eight voices mixed in Lisp), `modplay` (ProTracker modules, from a file too), `tune` (AdLib music from the timer interrupt), `midi` (an MPU-401), `spectrum` (an FFT of what is playing) |
| DMA | `blaster` (playing), `dmascope` (the controller's registers, live), `record`, `modplay` (a MOD player), `floppy` (a sector without the BIOS) |
| Interrupts and timing | `timer` (the timer chip at 1000 Hz), `alarm` (the real-time clock's interrupt), `keys` (which keys are held down), `term` (a serial terminal, received by interrupt), `step` (single-stepping machine code) |
| Other hardware and drivers | `joystick` (the game port's timers), `xms` (extended and expanded memory), `cdrom` (MSCDEX, raw sectors, audio tracks), `speed` (the time-stamp counter, and ways to copy memory) |

`blaster`, `dmascope`, `record` and `modplay` want a Sound Blaster (the launcher's QEMU machine has an
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

### Editing Lisp

The editor runs in the same Lisp as the code it edits, and treats that
code as expressions. `C-M-` is Ctrl and Alt together; `ESC` and then the
Ctrl key does the same, for a host that keeps Ctrl-Alt to itself.

| Keys | What they do |
|---|---|
| `C-M-f`, `C-M-b` | Forward and back over an expression |
| `C-M-u`, `C-M-d`, `M-)` | Out of this list to its start, into the next list, out past its end |
| `C-M-a`, `C-M-e` | To the start and the end of the top-level form |
| `C-M-k`, `C-M-<backspace>` | Kill the expression after point, or before it |
| `C-M-SPC`, `C-M-h` | Mark the expression; mark the top-level form |
| `C-M-t` | Swap the expressions either side of point |
| `TAB`, `C-M-q`, `M-q`, `C-M-\` | Indent the line, the expression after point, the top-level form, the region |
| `M-;` | Comment the region out, or take its comments off; without a region, a comment on this line |
| `M-(` | A pair of parentheses, round the region if there is one |
| `M-s`, `M-r` | Take this list's parentheses away; put the expression after point in place of its list |
| `C-c )`, `C-c }` | Move the closing parenthesis out past the next expression, or back in before the last |
| `M-TAB`, `M-/` (and `TAB` in the REPL) | Complete the symbol before point from the running Lisp |
| `SPC` | After a function's or macro's name, its arguments appear in the echo area |
| `C-c C-d d`, `C-c C-d a` | Describe a symbol; list the symbols with some text in their names |
| `C-c RET`, `C-c M-m` | Expand the macro call at point once, or all the way down |
| `C-x C-e`, `C-c C-c`, `C-c C-r`, `C-c C-b` | Evaluate the expression before point, the top-level form, the region, the buffer |
| `C-u C-x C-e` | Evaluate the expression before point and insert the result |
| `C-c C-k` | Save, compile and load the file; what the compiler said is in `*compilation*` |
| `C-M-x` (and `C-c C-c`) | Evaluate the top-level form; a `defvar` gives its variable the value again |
| `C-c I` | The inspector: evaluate an expression and look at the value part by part (`RET` into a part, `l` back, `d` describe, `q` leave) |
| `C-c i` | Go to one of this buffer's definitions by name, with completion |
| `C-c C-t`, `C-c M-d`, `C-c C-u` | Trace or untrace a function; disassemble one; undefine one |
| `C-c M-p`, `C-c M-o` | Put the REPL in this buffer's package; empty the REPL |
| `M-r` in the REPL | Recall the most recent input containing some text |
| `M-x check-parens` | Go to a parenthesis that has no partner |
| `M-x electric-pair-mode` | `(` and `"` bring their partners; Backspace takes an empty pair |

Strings and `#| |#` comments are coloured across the lines they run over.

Code in a file is read in the package of the nearest `(in-package ...)`
above it; `*scratch*` and the REPL share the REPL's package.

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
| The mouse | Click to put the cursor somewhere, drag to select, turn the wheel to move; `M-x mouse-mode` turns it off |
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

## Building SBCL for DOS

The port is a branch of SBCL, not part of this project:
<https://github.com/lispnik/sbcl>, branch `dos-port`. It makes a 32-bit
protected-mode `SBCL.EXE` that runs on FreeDOS under the HX extender's DPMI
host (`HDPMI32.EXE`), with the long-file-name driver DOSLFN loaded, on a
Pentium Pro or later with a floating-point unit.

DOS cannot host the build. The C runtime is cross-compiled on a Unix
machine with Open Watcom, and the steps that must run the new Lisp (the
warm load and saving the core) run on FreeDOS in QEMU. It has been built on
macOS on Apple silicon; the makefile also names the Open Watcom binaries
for Intel macOS and for Linux, which have not been tried.

On the build machine, from a package manager: a host SBCL (as any SBCL
build needs), GNU make, `qemu-system-i386`, `mtools`, `nasm`, `objconv`
and an `i386-elf-gcc` cross-compiler (used only to assemble one file).

Then a directory of downloads, `sbcl-dos-tools`, beside the SBCL tree
(or anywhere, named by `SBCL_DOS_TOOLS`):

| In `sbcl-dos-tools/` | What | From |
|---|---|---|
| `ow/` | An Open Watcom v2 snapshot (`ow-snapshot.tar.xz`), unpacked | <https://github.com/open-watcom/open-watcom-v2> |
| `dl-hx/rt/`, `dl-hx/dev/` | The HX runtime and development kits (`HXRT`, `HXDEV`), unpacked | <https://github.com/Baron-von-Riedesel/HX> |
| `dl-freedos/lite/FD14LITE.img` | The FreeDOS 1.4 "LiteUSB" image | <https://www.freedos.org> |
| `dl-watt32/src/` | Watt-32, the TCP/IP library (for sockets) | <https://github.com/gvanem/Watt-32> |
| `dl-pktdrv/crynwr/` | The Crynwr packet drivers (for sockets, when running) | the Crynwr collection |
| `dl-quicklisp/quicklisp.lisp` | Optional | <https://beta.quicklisp.org/quicklisp.lisp> |

```sh
git clone -b dos-port https://github.com/lispnik/sbcl sbcl-dos-experiment
cd sbcl-dos-experiment
sh tools-for-build/dos-watt32.sh          # the TCP/IP library; skip it to build without sockets
./make-dos.sh                             # the runtime, then the core: src/runtime/sbcl.exe, output/sbcl.core
./make-dos-contrib.sh asdf sb-bsd-sockets sb-posix
```

`make-dos.sh` takes a while: the warm build runs in the emulator. The
scripts' own comments say more (`make-dos.sh`, `make-dos-contrib.sh`,
`src/runtime/Makefile.dos`, `tools-for-build/dos-run.sh`). The names here
are the ones the scripts default to: this project's launcher looks for the
SBCL tree in `sbcl-dos-experiment` next to `sbcl-dos-tools` unless
`SBCL_SRC` says otherwise, and for this project inside `sbcl-dos-tools`.

To run the result without this project, put `sbcl.exe`, `hdpmi32.exe` and
`sbcl.core` in one directory on a FreeDOS disk, with the contribs in a
`CONTRIB` directory beside them, load DOSLFN, and type `sbcl`.

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
