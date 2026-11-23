# The examples, as they look

Each picture is the display of the DOS machine (QEMU) while the example runs,
taken by `tests/shots.py`.  Run one with `demo NAME` at the DOS prompt, or
`(doslib-examples:run "NAME")` in Lisp.

## graphics

| | | |
|---|---|---|
| <img src="blit.png" width="260"><br>**blit**<br>Sprites with transparency, drawn in a back buffer and copied at the retrace. | <img src="bmpview.png" width="260"><br>**bmpview**<br>An image viewer. | <img src="compiled.png" width="260"><br>**compiled**<br>Compiled sprites: a sprite as machine code, raced against a Lisp blitter. |
| <img src="copper.png" width="260"><br>**copper**<br>Copper bars: a palette entry rewritten on every scanline. | <img src="dirty.png" width="260"><br>**dirty**<br>Dirty rectangles: only what changed is redrawn and copied to the screen. | <img src="fire.png" width="260"><br>**fire**<br>The classic fire effect, mode 13h, animated until a key is hit. |
| <img src="flip.png" width="260"><br>**flip**<br>Mode X page flipping: draw out of sight, then change the start address. | <img src="font.png" width="260"><br>**font**<br>Redefining text-mode characters. | <img src="formula.png" width="260"><br>**formula**<br>A formula compiled to machine code while you watch, and plotted. |
| <img src="gcviz.png" width="260"><br>**gcviz**<br>Watching the garbage collector. | <img src="latch.png" width="260"><br>**latch**<br>Latch copies: tiles moved inside video memory four pixels to the byte. | <img src="lfb.png" width="260"><br>**lfb**<br>The VESA linear framebuffer: 640x480 with no bank switching. |
| <img src="lsystem.png" width="260"><br>**lsystem**<br>Plants grown from grammars. | <img src="mandel.png" width="260"><br>**mandel**<br>The Mandelbrot set in mode 13h. | <img src="mode12.png" width="260"><br>**mode12**<br>Mode 12h: 640x480 in 16 colours through the graphics controller. |
| <img src="modex.png" width="260"><br>**modex**<br>Mode X: 320x240 with square pixels and several pages. | <img src="paint.png" width="260"><br>**paint**<br>A paint program for the mouse; saves PAINT.BMP. | <img src="palette.png" width="260"><br>**palette**<br>Palette tricks: colour cycling, a fade and a cross-fade, with no pixel redrawn. |
| <img src="parallax.png" width="260"><br>**parallax**<br>Parallax: three layers at different speeds and sprites, redrawn and page-flipped. | <img src="picview.png" width="260"><br>**picview**<br>A viewer for PCX and GIF pictures, decoded in Lisp. | <img src="plasma.png" width="260"><br>**plasma**<br>A plasma: sine waves written straight into VGA memory. |
| <img src="raycast.png" width="260"><br>**raycast**<br>A textured 3-D maze in the style of early-1990s shooters. | <img src="raytrace.png" width="260"><br>**raytrace**<br>A small ray tracer. | <img src="rotozoom.png" width="260"><br>**rotozoom**<br>A rotozoomer: a texture turned and zoomed, two additions a pixel. |
| <img src="scroll.png" width="260"><br>**scroll**<br>Hardware scrolling: a wide tile map moved by the VGA's start address and panning. | <img src="solid3d.png" width="260"><br>**solid3d**<br>Filled 3-D solids: culling, depth sorting, flat shading, affine texture mapping. | <img src="split.png" width="260"><br>**split**<br>The VGA split screen: a status bar that stays put under a scrolling playfield. |
| <img src="sprite.png" width="260"><br>**sprite**<br>A sprite editor, worked with the mouse. | <img src="stars.png" width="260"><br>**stars**<br>Flying through a starfield. | <img src="textfont.png" width="260"><br>**textfont**<br>Text-mode fonts: glyphs redefined in plane 2, a smooth progress bar, 80 by 50. |
| <img src="textfx.png" width="260"><br>**textfx**<br>Text-mode effects by writing to B800:0000. | <img src="tiles.png" width="260"><br>**tiles**<br>A tile map scrolled every way by the VGA, drawing only the tiles that come into view. | <img src="tunnel.png" width="260"><br>**tunnel**<br>The tunnel effect: tables of each pixel's depth and angle, a moving texture. |
| <img src="turtle.png" width="260"><br>**turtle**<br>Turtle graphics with CLOS. | <img src="vesa.png" width="260"><br>**vesa**<br>640x480 in 256 colours through the VESA BIOS. | <img src="wire3d.png" width="260"><br>**wire3d**<br>Rotating wireframe solids. |

## lisp

| | | |
|---|---|---|
| <img src="bench.png" width="260"><br>**bench**<br>The same portable Lisp on DOS and on the build host. | <img src="faults.png" width="260"><br>**faults**<br>Processor faults as Lisp conditions: caught, reported, and put right. | <img src="hotfix.png" width="260"><br>**hotfix**<br>A game with a bug, fixed from inside its own error while it runs. |
| <img src="life.png" width="260"><br>**life**<br>Conway's Game of Life in text mode. | <img src="live.png" width="260"><br>**live**<br>Change a running program from the listener. | <img src="restarts.png" width="260"><br>**restarts**<br>DOS errors as restarts: Abort, Retry, Fail? done properly. |
| <img src="resume.png" width="260"><br>**resume**<br>Stop a computation in the middle; carry on later from RESUME.EXE. | <img src="shell.png" width="260"><br>**shell**<br>A DOS shell in Lisp. | <img src="snake.png" width="260"><br>**snake**<br>Snake: arrow keys steer, Esc quits. |
| <img src="special.png" width="260"><br>**special**<br>Programs that write programs, at run time, on DOS. | <img src="windows.png" width="260"><br>**windows**<br>A window system in a page of CLOS, with the mouse. |  |

## network

| | | |
|---|---|---|
| <img src="browse.png" width="260"><br>**browse**<br>A text browser for Gopher and plain HTTP, with numbered links. | <img src="files.png" width="260"><br>**files**<br>A file server: the DOS machine's disk in a Web browser. | <img src="irc.png" width="260"><br>**irc**<br>An IRC client: a channel above, a line to type in below. |
| <img src="netpong.png" width="260"><br>**netpong**<br>Pong for two machines over TCP: host it, or join a host. | <img src="remote.png" width="260"><br>**remote**<br>A remote REPL into a running graphics program. | <img src="websrv.png" width="260"><br>**websrv**<br>A web server in Lisp on DOS, using SB-BSD-SOCKETS. |
| <img src="wget.png" width="260"><br>**wget**<br>An HTTP client in Lisp on DOS, using SB-BSD-SOCKETS. |  |  |

## sound

| | | |
|---|---|---|
| <img src="adlib.png" width="260"><br>**adlib**<br>FM synthesis on an AdLib or Sound Blaster card. | <img src="blaster.png" width="260"><br>**blaster**<br>Digital sound on a Sound Blaster: DMA and an interrupt handler. | <img src="dmascope.png" width="260"><br>**dmascope**<br>A scope on the Sound Blaster's DMA buffer: the DMA controller's registers, live. |
| <img src="midi.png" width="260"><br>**midi**<br>MIDI out through an MPU-401: a phrase sent byte by byte, and shown. | <img src="mixer.png" width="260"><br>**mixer**<br>A software mixer: 8 voices with pitch, volume and pan, and what mixing them costs. | <img src="modplay.png" width="260"><br>**modplay**<br>A MOD player: four channels mixed in Lisp into the Sound Blaster's DMA buffer. |
| <img src="record.png" width="260"><br>**record**<br>Recording with a Sound Blaster: DMA from the card to memory, then played back. | <img src="speaker.png" width="260"><br>**speaker**<br>Music on the PC speaker. | <img src="spectrum.png" width="260"><br>**spectrum**<br>A spectrum analyser: a fixed-point FFT in Lisp of what the Sound Blaster plays. |
| <img src="tune.png" width="260"><br>**tune**<br>An AdLib tune in four voices, sequenced from the timer interrupt. |  |  |

## system

| | | |
|---|---|---|
| <img src="alarm.png" width="260"><br>**alarm**<br>The real-time clock's interrupt: 1,024 a second, and an alarm. | <img src="asm.png" width="260"><br>**asm**<br>A small x86 assembler in Lisp. | <img src="asmdsl.png" width="260"><br>**asmdsl**<br>Assembly language with Lisp macros, timed against Lisp. |
| <img src="calc.png" width="260"><br>**calc**<br>A pop-up calculator on F12, over another example. | <img src="cdrom.png" width="260"><br>**cdrom**<br>The CD-ROM: MSCDEX, raw sectors of ISO 9660, and the audio tracks. | <img src="diskinfo.png" width="260"><br>**diskinfo**<br>Reading a disk the hard way. |
| <img src="dosinfo.png" width="260"><br>**dosinfo**<br>Asking DOS directly with INT 21h, bypassing the C library. | <img src="driver.png" width="260"><br>**driver**<br>Two small device drivers: a network card's address, and a display mode with no BIOS. | <img src="fatview.png" width="260"><br>**fatview**<br>A disk sector viewer and FAT explorer. |
| <img src="floppy.png" width="260"><br>**floppy**<br>A floppy's boot sector read with the controller and DMA channel 2, not the BIOS. | <img src="intvec.png" width="260"><br>**intvec**<br>A Lisp function as an interrupt handler. | <img src="ivt.png" width="260"><br>**ivt**<br>The real-mode interrupt vector table. |
| <img src="joystick.png" width="260"><br>**joystick**<br>The joystick: four timers and four switches at port 201h. | <img src="keys.png" width="260"><br>**keys**<br>Which keys are held down, from every scan code the keyboard sends. | <img src="mcbwalk.png" width="260"><br>**mcbwalk**<br>Walk DOS's chain of memory control blocks, the way MEM does. |
| <img src="memmap.png" width="260"><br>**memmap**<br>The DPMI host's view of memory (INT 31h) next to SBCL's own. | <img src="pciscan.png" width="260"><br>**pciscan**<br>Everything the PCI bus will say about itself. | <img src="ports.png" width="260"><br>**ports**<br>Three old PC devices driven through their I/O ports. |
| <img src="printer.png" width="260"><br>**printer**<br>A printer that is not ready, as restarts. | <img src="probes.png" width="260"><br>**probes**<br>The CMOS memory, the real-time clock and the floppy controller. | <img src="serial.png" width="260"><br>**serial**<br>A Lisp listener on COM1. |
| <img src="speed.png" width="260"><br>**speed**<br>The processor's clock rate, and a chart of eight ways to move a megabyte. | <img src="step.png" width="260"><br>**step**<br>Single-stepping machine code: a small debugger. | <img src="sysinfo.png" width="260"><br>**sysinfo**<br>A hardware inventory read from Lisp. |
| <img src="term.png" width="260"><br>**term**<br>A terminal: the serial port by interrupt, and ANSI escape sequences. | <img src="timer.png" width="260"><br>**timer**<br>The timer chip at 1000 interrupts a second: a millisecond clock. | <img src="vop.png" width="260"><br>**vop**<br>Machine instructions in line: three operations added to the compiler, and timed. |
| <img src="xms.png" width="260"><br>**xms**<br>Memory beyond the first megabyte, the DOS way: XMS and EMS. |  |  |

No picture: `ntp`.
