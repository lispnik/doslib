# doslib reference

Every exported name, file by file, with its arguments and what its
documentation string says.  This file is made by `doc/make-reference.lisp`
from the sources: change the documentation string, not this file.

## The library

Package `doslib`.

### `src/machine.lisp`: Machine code made at run time: software interrupts and byte helpers.

**`machine-code`** `&rest bytes` — Function  
Put BYTES in memory as machine code.  Returns its address (a system area pointer), to be called with ALIEN-FUNCALL.  The memory is the process's, not the Lisp image's: see SAVED-CODE.

**`interrupt-stub`** `n` — Function  
Machine code for the function INT86 calls to do interrupt N: it loads EAX, EBX, ECX, EDX and EDI from a block of words, does INT N, and stores them back with the flags.

**`*regs*`** — Variable  
The block of words INT86 passes the registers in.

**`*stubs*`** — Variable  
Interrupt number -> its INTERRUPT-STUB, made when first wanted.

**`int86`** `n &key (ax 0) (bx 0) (cx 0) (dx 0) (di 0)` — Function  
Software interrupt N.  Returns AX BX CX DX DI FLAGS as 32-bit values.

**`lo`** `x` — Function  
The low byte of X: AL of AX.

**`hi`** `x` — Function  
The second byte of X: AH of AX.

**`word16`** `x` — Function  
The low 16 bits of X: AX of EAX.

**`*session*`** — Variable  
Which process of this image we are in: DOSLIB-INIT counts them.

**`saved-code`** — Structure  
Machine code kept as its bytes, so that it survives in a saved core: (saved-code bytes) makes one, and CODE-ADDRESS puts it in memory.

**`code-address`** `saved-code` — Function  
Where SAVED-CODE is in memory, in this process.

### `src/ports.lisp`: I/O ports, 8, 16 and 32 bits wide, and the CMOS memory.

**`*inb*`** — Variable  
Machine code: read a byte from a port.

**`*outb*`** — Variable  
Machine code: write a byte to a port.

**`inb`** `port` — Function  
Read a byte from I/O port PORT.

**`outb`** `port value` — Function  
Write the byte VALUE to I/O port PORT.

**`inw`** `port` — Function  
Read 16 bits from I/O port PORT.

**`outw`** `port value` — Function  
Write the 16 bits of VALUE to I/O port PORT.

**`*inl*`** — Variable  
Machine code: read 32 bits from a port.

**`*outl*`** — Variable  
Machine code: write 32 bits to a port.

**`inl`** `port` — Function  
Read 32 bits from I/O port PORT.

**`outl`** `port value` — Function  
Write the 32 bits of VALUE to I/O port PORT.

**`cmos`** `register` — Function  
Byte REGISTER (0 to 127) of the CMOS memory: the clock, and the BIOS's settings.

**`bcd`** `byte` — Function  
The number that BYTE holds as two decimal digits, as the clock keeps its.

### `src/dos.lisp`: Real-mode interrupts through DPMI, memory below 1MB, Ctrl-C and Ctrl-Break.

**`*rm*`** — Variable  
The DPMI real-mode call structure REAL-INT passes the registers in.

**`real-int`** `n &key (ax 0) (bx 0) (cx 0) (dx 0) (si 0) (di 0) (es 0) (ds 0)` — Function  
Real-mode interrupt N.  Returns AX BX CX DX ES DS FLAGS (16-bit values).

**`real-call`** `segment offset &key (ax 0) (bx 0) (cx 0) (dx 0) (si 0) (di 0) (es 0) (ds 0)` — Function  
Call the real-mode routine at SEGMENT:OFFSET, which returns with a far return.  Returns AX BX CX DX ES DS FLAGS (16-bit values), as REAL-INT does.

**`dos-alloc`** `bytes` — Function  
Memory below 1MB, which real-mode code (BIOS, DOS) can reach.  Returns its segment.  It belongs to this process: a saved core does not keep it.

**`dos-free`** `segment` — Function  
Give back memory that DOS-ALLOC returned.

**`with-dos-memory`** `(segment bytes) &body body` — Macro  
Run BODY with SEGMENT bound to BYTES of DOS memory, freed afterwards.

**`linear`** `segment &optional (offset 0)` — Function  
The address, for PEEK and SAP-REF, of real-mode SEGMENT:OFFSET.

**`dos-scratch`** `` — Function  
The segment of 16K of DOS memory for short-lived use: what is in it lasts until the next thing that uses it (DOS-STRING, the functions in disk.lisp).

**`dos-string`** `string &key (segment (dos-scratch)) (offset 0)` — Function  
Copy STRING into DOS memory with a zero after it, as DOS wants its file names.  Returns the segment and the offset: DS:DX for REAL-INT, say.

**`dos-bytes`** `segment offset count` — Function  
COUNT bytes of memory at SEGMENT:OFFSET, as a vector.

**`break-checking`** `` — Function  
Is DOS's BREAK on?  SETF it to true or false to turn it on or off.

**`(setf break-checking)`** `on` — Function  
Turn DOS's BREAK on or off.

**`without-break-checking`** `&body body` — Macro  
Run BODY with DOS's BREAK off, and put it back as it was afterwards.

**`interrupt`** — Type  
The condition signalled when the user interrupts the program.

**`interrupts-supported-p`** `` — Function  
Can this SBCL be interrupted with Ctrl-Break?  (One built before that was added cannot.)

**`interrupt-chord`** `` — Function  
Does pressing Ctrl, Alt and Shift together interrupt the program?  SETF it to NIL for a program that wants those keys itself.

**`(setf interrupt-chord)`** `on` — Function  
Say whether Ctrl-Alt-Shift interrupts the program.

**`interrupt-count`** `` — Function  
How many times the program has been interrupted from the keyboard.

**`catching-interrupt`** `&body body` — Macro  
Run BODY and return its values -- or, if the user interrupts it, stop it there (its UNWIND-PROTECT forms run) and return :INTERRUPTED.

### `src/cpuid.lisp`: The CPUID instruction.

**`*cpuid*`** — Variable  
Machine code: the CPUID instruction.

**`cpuid`** `leaf` — Function  
The processor's answer to CPUID with EAX = LEAF: EAX, EBX, ECX and EDX.

### `src/memory.lisp`: Reading memory by address, and making a device's memory addressable.

**`peek`** `address` — Function  
The byte at ADDRESS.

**`peek16`** `address` — Function  
The 16 bits at ADDRESS.

**`peek32`** `address` — Function  
The 32 bits at ADDRESS.

**`map-physical`** `physical size` — Function  
Make SIZE bytes of a device's memory, at the physical address PHYSICAL, addressable (DPMI function 0800h).  Returns the address they are at, to use with PEEK or INT-SAP, or NIL if the DPMI host would not.

**`unmap-physical`** `address` — Function  
Take away a mapping that MAP-PHYSICAL made at ADDRESS (DPMI function 0801h).

**`with-physical-memory`** `(address physical size) &body body` — Macro  
Run BODY with ADDRESS bound to where SIZE bytes of device memory at the physical address PHYSICAL have been mapped (MAP-PHYSICAL), or to NIL if the DPMI host would not; the mapping is taken away afterwards.

### `src/interrupts.lisp`: Protected-mode interrupt vectors, through DPMI.

**`code-selector`** `` — Function  
The selector our code runs in: what an interrupt vector must name.

**`data-selector`** `` — Function  
The selector of our (flat) data segment, for handlers that must load DS.

**`bytes32`** `x` — Function  
X as four bytes, low first: for putting addresses into machine code.

**`get-vector`** `n` — Function  
The handler of protected-mode interrupt N: its selector and offset (DPMI 0204h).

**`set-vector`** `n selector offset` — Function  
Point protected-mode interrupt N at SELECTOR:OFFSET (DPMI 0205h).

### `src/hotkey.lisp`: Hot keys: keys taken straight from the keyboard interrupt.

**`hotkeys-installed-p`** `` — Function  
Is any hot key installed?

**`install-hotkey`** `scan-code function` — Function  
Make the key with SCAN-CODE (F12 is #x58, F11 #x57, Scroll Lock #x46) call FUNCTION, with no arguments, the next time the program polls.

**`remove-hotkeys`** `` — Function  
Give every hot key back to the keyboard, and interrupt 15h to its old handler.

**`remove-hotkey`** `scan-code` — Function  
Give the key with SCAN-CODE back to the keyboard.

**`hotkey-pending-p`** `scan-code` — Function  
Has the hot key been pressed, and not yet been acted on?

**`clear-pending-hotkeys`** `` — Function  
Forget hot keys that have been pressed but not yet acted on.

**`poll-hotkeys`** `` — Function  
Call the function of any hot key pressed since the last poll.  Cheap when none was.

### `src/video.lisp`: VGA: video modes, the palette, pixels and lines in mode 13h; VESA modes.

**`*vga*`** — Variable  
Video memory in the graphics modes: a byte a pixel in mode 13h.

**`video-mode`** `mode` — Function  
Set BIOS video mode MODE: 3 is 80x25 text, #x13 is 320x200 in 256 colours.

**`current-video-mode`** `` — Function  
The BIOS's video mode: 3 is 80x25 text, #x13 is 320x200 in 256 colours.

**`with-video-mode`** `(mode) &body body` — Macro  
Run BODY in video mode MODE, and go back to 80x25 text however it ends.

**`wait-retrace`** `` — Function  
Wait for the start of the next vertical retrace: the moment to change the palette or the picture without it showing.

**`vesa-set-mode`** `mode &key linear keep` — Function  
Set a VESA BIOS mode (#x101 is 640x480 in 256 colours).  True if the BIOS did.  With LINEAR the mode is set for its linear framebuffer (see VESA-MODE-INFO) and not for the 64K window at A0000h that VESA-SET-BANK moves; with KEEP video memory is not cleared, and the picture stays.

**`vesa-mode-info`** `mode` — Function  
What the VESA BIOS says of MODE (INT 10h, AX=4F01h), as a property list, or NIL if it knows no such mode.  :WIDTH and :HEIGHT are in pixels, :BITS the bits in a pixel, :LINE-BYTES the bytes from one row to the next; :FRAMEBUFFER is the physical address of the mode's linear framebuffer, for MAP-PHYSICAL, or NIL if it has none; :GRANULARITY is how many kilobytes one step of VESA-SET-BANK moves the window; :ATTRIBUTES is the mode's attribute word (bit 7: a linear framebuffer, bit 4: graphics).

**`vesa-set-bank`** `bank &optional (window 0)` — Function  
Move the 64K window at A0000h to BANK of a VESA mode's video memory.

**`set-palette`** `index r g b` — Function  
Make colour INDEX the mixture R, G, B: each from 0 to 63.

**`rainbow-palette`** `&optional (first 1) (last 255)` — Function  
Make colours FIRST to LAST a rainbow, and colour 0 black.

**`plot`** `x y colour` — Function  
Set the pixel at X, Y in mode 13h; off the screen, nothing happens.

**`draw-line`** `x0 y0 x1 y1 colour` — Function  
Draw a line in mode 13h (Bresenham's algorithm).

### `src/gtext.lisp`: Text in the graphics modes: the BIOS's 8 by 8 characters, as pixels.

**`glyph-row`** `character row` — Function  
Row ROW (0 at the top to 7) of CHARACTER, a character or its code, in the BIOS's 8 by 8 font: a byte, the leftmost pixel in bit 7.  The ROM has the first 128 characters; one above that is given the pattern 128 below it.

**`glyph-rows`** `character` — Function  
The eight rows of CHARACTER in the BIOS's 8 by 8 font, top first, as a vector of bytes: see GLYPH-ROW.

**`draw-glyphs`** `memory offset row-bytes string colour &optional (background 0)` — Function  
Write STRING in the BIOS's 8 by 8 characters into memory that has a byte for each pixel.  MEMORY is a system area pointer, OFFSET where in it the top left pixel of the first character goes, and ROW-BYTES the distance from one row of pixels to the next: 320 in mode 13h.  The pixels of the letters are set to COLOUR and the rest of each character's square to BACKGROUND; if that is NIL they are left as they were.  Nothing is clipped.

**`graphics-text`** `x y string colour &optional (background 0)` — Function  
Write STRING in mode 13h with the top left corner of its first character at X, Y: each character is 8 by 8 pixels, the letters in COLOUR on BACKGROUND, or over the picture as it is if BACKGROUND is NIL.  Characters that would not be wholly on the screen are left out.

### `src/modex.lisp`: The VGA's registers, and "Mode X": 320 by 240 in 256 colours, in planes.

**`vga-sequencer`** `index &optional value` — Function  
The VGA sequencer's register INDEX (ports 3C4h and 3C5h): set it to VALUE, or, with no VALUE, return what it holds.  Register 2 is the map mask (see VGA-PLANES), 4 the memory mode.

**`vga-crtc`** `index &optional value` — Function  
The CRT controller's register INDEX (ports 3D4h and 3D5h): set it to VALUE, or, with no VALUE, return what it holds.  Registers 0 to 7 cannot be written while bit 7 of register 11h is set.

**`vga-graphics`** `index &optional value` — Function  
The graphics controller's register INDEX (ports 3CEh and 3CFh): set it to VALUE, or, with no VALUE, return what it holds.  Register 5 is the mode: its low two bits are the write mode.

**`vga-attribute`** `index &optional value` — Function  
The attribute controller's register INDEX (port 3C0h; read at 3C1h): set it to VALUE, or, with no VALUE, return what it holds.  The display stays on.  Register 13h is the horizontal pixel panning.

**`vga-planes`** `mask` — Function  
Say which of the four planes of video memory a write goes to: bit 0 of MASK for plane 0, and so on to bit 3.  (The sequencer's map mask, register 2.)  In Mode X, 1, 2, 4 or 8 is one pixel of the four at an address, and 15 all four at once.

**`vga-display-start`** `address` — Function  
Have the display begin at ADDRESS of video memory (CRT controller registers 0Ch and 0Dh), from the next vertical retrace: how pages are flipped and pictures scrolled.  In Mode X one address is four pixels, and a row is 80 addresses.  If a retrace is going on, its end is waited for, so that the VGA does not take half of the old address and half of the new.

**`mode-x`** `` — Function  
Set Mode X: 320 by 240 in 256 colours, with square pixels and video memory in four planes of 64K.  The pixel X, Y is in plane X mod 4, at address 80 Y + X / 4 from *VGA*; VGA-PLANES says which planes are written. All of video memory is cleared.  The BIOS still believes it is in mode 13h; (video-mode 3) goes back to text.

**`mode-x-text`** `x y string colour &optional (background 0) (start 0) (row-bytes 80)` — Function  
Write STRING in Mode X, in the BIOS's 8 by 8 characters, with the top left corner of its first character at X (a multiple of 4), Y: the letters in COLOUR on BACKGROUND, or over the picture as it is if BACKGROUND is NIL. START is the address in video memory of the page to write on, and ROW-BYTES the addresses from one row to the next, if it is not the usual 80.  Nothing is clipped, and the map mask is left naming plane 3.

### `src/input.lisp`: The BIOS timer tick and keyboard.

**`ticks`** `` — Function  
The BIOS's count of timer ticks: 18.2 a second, zero at midnight.

**`wait-tick`** `&optional (n 1)` — Function  
Wait for N ticks of the BIOS timer: 18.2 to the second.

**`timer-count`** `` — Function  
The time in the timer chip's clocks, +TIMER-HZ+ (1,193,182) to the second: the BIOS's count of ticks, 65,536 clocks each, and how far the timer has got through this one.  For timing what is over in less than a tick, and for pacing: see WAIT-FRAME.

**`wait-frame`** `&optional retrace` — Function  
Wait until a frame's time has passed since WAIT-FRAME last returned: 16,000 of TIMER-COUNT's clocks, a little less than a frame at 70 a second. Then, if RETRACE, wait for the vertical retrace as well.  This paces an animation where WAIT-RETRACE alone does not: some emulators (QEMU is one) report a retrace whenever they are asked.

**`key-pressed-p`** `` — Function  
Is a key waiting to be read?

**`wait-key`** `` — Function  
Wait for a key.  Returns the BIOS's word for it: the scan code in the high byte, the character in the low.  GET-KEY gives it a name.

**`finish`** `` — Function  
Wait for a key, then go back to 80x25 text: how a picture ends.

### `src/text.lisp`: Text mode: characters and colours written straight into video memory,

**`*text-screen*`** — Variable  
80x25 cells: a character byte, then a colour byte.

**`text-put`** `x y string &optional (attribute 7)` — Function  
Write STRING at column X, row Y, in colour ATTRIBUTE (background * 16 + foreground).

**`text-fill`** `x y width height &optional (attribute 7) (char #\ )` — Function  
Fill a rectangle with CHAR in colour ATTRIBUTE.

**`text-box`** `x y width height &key (attribute 7) title (title-attribute attribute) shadow` — Function  
A filled box with a double-line border, an optional TITLE and SHADOW.

**`text-cursor`** `x y` — Function  
Put the hardware cursor at X, Y.

**`hide-text-cursor`** `` — Function  
Make the hardware cursor invisible.

**`show-text-cursor`** `` — Function  
Make the hardware cursor the usual underline.

**`save-text-screen`** `` — Function  
The whole text screen and the cursor, for RESTORE-TEXT-SCREEN.

**`restore-text-screen`** `saved` — Function  
Put back the screen and cursor that SAVE-TEXT-SCREEN returned.

**`get-key`** `` — Function  
Wait for a key.  Returns its character -- or, for a key that has none, a keyword such as :UP, :F1 or :DELETE (or the scan code, if it has no name).

**`text-input`** `x y width &key (attribute 7) (initial "")` — Function  
Let the user type a line at X, Y in a field WIDTH wide.  Enter returns the string; Esc returns NIL.

### `src/critical.lisp`: DOS critical errors: "Abort, Retry, Fail?" answered by the program.

**`catch-critical-errors`** `` — Function  
From now on DOS device errors fail quietly, and (critical-error) tells of them.  Calls nest: the handler stays until RELEASE-CRITICAL-ERRORS has been called as often as this has.

**`release-critical-errors`** `&optional all` — Function  
Undo one CATCH-CRITICAL-ERRORS -- or, with ALL, every one.  When none is left, interrupt 24h goes back to its previous handler.

**`with-critical-errors-caught`** `&body body` — Macro  
Run BODY with DOS device errors failing quietly, as CATCH-CRITICAL-ERRORS.

**`critical-error`** `` — Function  
If a device has failed since this was last asked, the description of the failure (and its code); otherwise NIL.  Asking clears it.

### `src/dialog.lisp`: A restart dialog: Common Lisp's answer to "Abort, Retry, Fail?".

**`text-wrap`** `text width` — Function  
TEXT as a list of lines no longer than WIDTH, broken at spaces.  A paragraph that starts with spaces keeps them, on every line it becomes; a word too long for any line is cut where the line ends.

**`dialog-ask`** `prompt` — Function  
Ask for a line of text inside the dialog that is showing (or at the bottom of the screen); for the :INTERACTIVE function of a restart. Returns the string typed, or "" if the user pressed Esc.

**`restart-dialog`** `condition &key (restarts (compute-restarts condition)) through (title "Error") eval-package` — Function  
Show CONDITION and RESTARTS; invoke the one the user chooses.  THROUGH names the last restart to offer: the program's outermost one, so that those of whatever is running the program are left out.  With EVAL-PACKAGE, the user may also type Lisp forms, evaluated in that package.

### `src/sound.lisp`: The PC speaker.

**`+timer-hz+`** — Constant  
The frequency of the clock that drives the timer chip.

**`speaker-divisor`** `divisor` — Function  
Start a tone of 1193182/DIVISOR Hz (DIVISOR from 1 to 65535).

**`speaker-on`** `frequency` — Function  
Start a tone of FREQUENCY Hz (19 to 20000 or so).  It sounds until SPEAKER-OFF.

**`speaker-off`** `` — Function  
Silence the speaker.

**`speaker-beep`** `&optional (frequency 880) (ticks 4)` — Function  
Sound FREQUENCY Hz for TICKS timer ticks (18.2 to the second).

### `src/mouse.lisp`: The mouse, through the mouse driver (interrupt 33h).

**`mouse-driver-p`** `` — Function  
Is there a handler on interrupt 33h?  (Calling it without one would crash.)

**`mouse-init`** `` — Function  
Reset the mouse driver.  Returns the number of buttons, or NIL if there is no driver or no mouse.  The pointer starts hidden, in the middle.

**`mouse-show`** `` — Function  
Show the driver's pointer.  (Calls to MOUSE-HIDE nest: each needs a MOUSE-SHOW.)

**`mouse-hide`** `` — Function  
Take the driver's pointer off the screen.

**`with-mouse-hidden`** `&body body` — Macro  
Run BODY with the pointer off the screen: for drawing where it might be.

**`mouse-position`** `` — Function  
Where the mouse is and what is pressed.  Returns X, Y (the driver's units), the buttons -- bit 0 is the left one, bit 1 the right, bit 2 the middle -- and how far the wheel has turned since this was last asked (negative is away from you; always 0 unless MOUSE-WHEEL-P has said there is one).

**`mouse-wheel-p`** `` — Function  
Does the mouse have a wheel that the driver reports (CTMOUSE's wheel interface)?  Asking turns the reporting on.

**`mouse-cell`** `` — Function  
The character cell the mouse is in: column, row, the buttons and the wheel.

**`mouse-pixel`** `` — Function  
The pixel the mouse is at in a graphics mode: x, y, the buttons and the wheel.

**`mouse-move`** `x y` — Function  
Put the mouse at X, Y (the driver's units).

**`mouse-range`** `x0 y0 x1 y1` — Function  
Keep the mouse inside the rectangle from X0, Y0 to X1, Y1.

**`mouse-presses`** `&optional (button 0)` — Function  
How often BUTTON (0 left, 1 right, 2 middle) has been pressed since this was last asked, and where the last press was: count, x, y.

**`mouse-releases`** `&optional (button 0)` — Function  
As MOUSE-PRESSES, for the button being let go.

**`mouse-motion`** `` — Function  
How far the mouse has moved since this was last asked, in mickeys (the mouse's own units, whatever the screen): dx and dy, negative for left and up.

### `src/disk.lisp`: Reading disks a sector at a time: through DOS, and through the BIOS.

**`disk-error`** — Condition  
A sector could not be read: DOS-READ-SECTORS and BIOS-READ-SECTORS signal it.

**`disk-error-drive`** `condition` — Function  
The drive of a disk-error condition.

**`disk-error-sector`** `condition` — Function  
The sector of a disk-error condition.

**`disk-error-code`** `condition` — Function  
The code of a disk-error condition.

**`disk-error-reason`** `condition` — Function  
The reason of a disk-error condition.

**`current-drive`** `` — Function  
The number of DOS's current drive: 0 is A:, 2 is C:.

**`drive-number`** `drive` — Function  
DRIVE as a number (0 is A:), given a number, a letter, a string such as "c:", or NIL for the current drive.

**`drive-letter`** `drive` — Function  
The letter of DRIVE (as DRIVE-NUMBER takes it): a character.

**`drive-read-method`** `drive` — Function  
Which of DOS's three ways reads DRIVE: :SMALL, :LARGE or :FAT32.  Signals DISK-ERROR if there is no such drive, or none of them works.

**`dos-read-sectors`** `drive sector &key (count 1) (bytes-per-sector 512)` — Function  
COUNT sectors of a DOS drive (a number, 0 for A:, or a letter, or NIL for the current one), starting at SECTOR of its volume, as a vector of bytes.

**`bios-read-sectors`** `drive sector &optional (count 1)` — Function  
COUNT 512-byte sectors of a whole disk, through the BIOS (interrupt 13h, function 42h): DRIVE is the BIOS's number, #x80 for the first hard disk, and SECTOR counts from the very start of the disk.

### `src/dma.lisp`: The DMA controllers: memory handed to a device, or filled by one,

**`dma-start`** `channel address length &key (direction :to-device) (repeat t)` — Function  
Have DMA channel CHANNEL move LENGTH bytes at ADDRESS: to its device (DIRECTION :TO-DEVICE, playing) or from it into memory (:TO-MEMORY, recording, reading a disk).  With REPEAT it starts again at ADDRESS each time it has moved them all; without, it stops there.

**`dma-stop`** `channel` — Function  
Mask DMA channel CHANNEL: it answers its device no more.

**`dma-address`** `channel` — Function  
The channel's address register as it is now: the low 16 bits of where it will move the next byte (for channels 5 to 7, the next word, counted in words).

**`dma-count`** `channel` — Function  
The channel's count register as it is now: the bytes (for channels 5 to 7, the words) it has still to move, less one.  FFFFh: it has moved them all.

**`dma-fit`** `address length` — Function  
The first address at or after ADDRESS where LENGTH bytes (65536 at most) lie without a 64K boundary among them.  Memory of twice LENGTH has room.

**`with-dma-memory`** `(address length) &body body` — Macro  
Run BODY with ADDRESS bound to the address of LENGTH bytes of DOS memory that a DMA channel can be pointed at, freed afterwards.

### `src/irq.lisp`: Handlers for hardware interrupts, built to order.

**`irq-hook`** — Structure  
A handler made by INSTALL-IRQ-HANDLER: what it is on, and what was there before.

**`irq-vector`** `irq` — Function  
The interrupt vector of hardware interrupt request IRQ (0 to 15).

**`irq-handler-bytes`** `irq memory &key read-ports write-ports buffer chain` — Function  
The machine code of a handler for IRQ, as a list of bytes: write each (port value) of WRITE-PORTS, read each of READ-PORTS, read port BUFFER if given and put the byte in the ring at MEMORY + 16, add one to the four bytes at MEMORY, and then either tell the interrupt controller and return, or (CHAIN, as INSTALL-IRQ-HANDLER describes it) jump to the handler whose address is at MEMORY + 6.

**`install-irq-handler`** `irq &key read-ports write-ports buffer chain (unmask (not chain))` — Function  
Put a handler on hardware interrupt request IRQ (0 to 15) that writes each (port value) of WRITE-PORTS and then reads each of READ-PORTS -- whatever tells the device it has been heard -- and counts.  With BUFFER, a port, it also reads that port and keeps the byte for IRQ-READ-BYTE (the last 256 of them).  Without CHAIN it then tells the interrupt controller the interrupt is done; with CHAIN true it passes every interrupt on to the handler that was there before, and with CHAIN a number only when a 16-bit sum of that number overflows.  With UNMASK (the default unless chaining) the interrupt controller is told to let the interrupt through.  Returns a hook, for IRQ-COUNT and REMOVE-IRQ-HANDLER.

**`remove-irq-handler`** `hook` — Function  
Take HOOK's handler out: the interrupt controller's mask bit and the vector go back to what they were when it was installed.

**`remove-irq-handlers`** `` — Function  
Take out every handler INSTALL-IRQ-HANDLER has put in: done when Lisp exits.

**`irq-count`** `hook` — Function  
How many interrupts HOOK's handler has seen since it was installed.  SETF it to start again.

**`(setf irq-count)`** `count hook` — Function  
Set the number of interrupts HOOK's handler has counted.

**`irq-read-byte`** `hook` — Function  
The next byte the handler of HOOK (installed with :BUFFER) has kept, or NIL when there is none waiting.  More than 255 left unread are lost.

**`with-irq-handler`** `(hook irq &rest options) &body body` — Macro  
Run BODY with a handler on hardware interrupt request IRQ, made as INSTALL-IRQ-HANDLER makes it from OPTIONS, and HOOK bound to its hook: (irq-count hook) is the number of interrupts so far.  The handler is taken out again however BODY ends.

### `src/keystate.lisp`: Which keys are held down.

**`+key-escape+`** — Constant  
The scan code of Esc.

**`+key-enter+`** — Constant  
The scan code of Enter.

**`+key-control+`** — Constant  
The scan code of (left) Ctrl.

**`+key-left-shift+`** — Constant  
The scan code of the left Shift key.

**`+key-right-shift+`** — Constant  
The scan code of the right Shift key.

**`+key-alt+`** — Constant  
The scan code of (left) Alt.

**`+key-space+`** — Constant  
The scan code of the space bar.

**`+key-up+`** — Constant  
The scan code of the up arrow (and 8 on the number pad).

**`+key-left+`** — Constant  
The scan code of the left arrow (and 4 on the number pad).

**`+key-right+`** — Constant  
The scan code of the right arrow (and 6 on the number pad).

**`+key-down+`** — Constant  
The scan code of the down arrow (and 2 on the number pad).

**`key-state-start`** `` — Function  
Begin keeping track of which keys are held down.  Returns true, or NIL if it was already being done.

**`key-state-stop`** `` — Function  
Stop keeping track of the keys, and empty the BIOS's keyboard buffer.

**`poll-key-state`** `` — Function  
Take in the scan codes that have arrived since last time.  KEY-DOWN-P and KEYS-DOWN do this themselves.  Returns the number of presses and releases seen since KEY-STATE-START.

**`key-down-p`** `scan-code` — Function  
Is the key with SCAN-CODE (1 to 127: +KEY-LEFT+, +KEY-SPACE+ ...) held down now?  Inside WITH-KEY-STATE only.  The keys the AT keyboard added answer to the same codes as the ones they copy: either Ctrl is 29.

**`keys-down`** `` — Function  
The scan codes of all the keys held down now, in order.

**`with-key-state`** `&body body` — Macro  
Run BODY with the keyboard's presses and releases tracked, so that KEY-DOWN-P and KEYS-DOWN work; the keyboard interrupt is put back however BODY ends.

### `src/blaster.lisp`: The Sound Blaster: where the card is, its processor (the "DSP"), and

**`*blaster-port*`** — Variable  
The Sound Blaster's first port: BLASTER-SETTINGS sets it.

**`*blaster-irq*`** — Variable  
The Sound Blaster's interrupt request line: BLASTER-SETTINGS sets it.

**`*blaster-dma*`** — Variable  
The Sound Blaster's DMA channel for 8-bit sound: BLASTER-SETTINGS sets it.

**`*blaster-dma16*`** — Variable  
A Sound Blaster 16's DMA channel for 16-bit sound: BLASTER-SETTINGS sets it.

**`blaster-settings`** `&optional (text (posix-getenv "BLASTER"))` — Function  
Take the card's port, interrupt and DMA channels from TEXT, which is the BLASTER environment variable unless given: "A220 I5 D1 H5" is port 220h, interrupt 5, and DMA channels 1 (8 bits) and 5 (16 bits).  What it does not say, or all of it if it is not set, is as in that example.  Sets *BLASTER-PORT*, *BLASTER-IRQ*, *BLASTER-DMA* and *BLASTER-DMA16*, and returns the four.

**`blaster-write`** `byte` — Function  
Send BYTE, a command or part of one, to the card's processor, when it is ready for it.

**`blaster-read`** `` — Function  
The next byte of the card's processor's answer, when there is one.

**`blaster-reset`** `` — Function  
Reset the card's processor: whatever it was playing or recording stops. True if a card answered at *BLASTER-PORT*.

**`blaster-version`** `` — Function  
The version of the card's processor, as two values: 2 and up plays without a gap (auto-initialized DMA), 3 is a Sound Blaster Pro, 4 a Sound Blaster 16.

**`blaster-present-p`** `` — Function  
Is there a Sound Blaster this library can use?  Reads the settings (BLASTER-SETTINGS) and resets the card: true if the interrupt is from 2 to 7, the 8-bit DMA channel is 0, 1 or 3, and a card answered.

**`blaster-interrupts`** `` — Function  
How many times the card has interrupted since WITH-BLASTER-INTERRUPT began: once for each block of samples played or recorded.

**`with-blaster-interrupt`** `() &body body` — Macro  
Run BODY with the card's interrupts counted: BLASTER-INTERRUPTS, which starts at 0, says how many there have been.  A handler in machine code is put on the card's interrupt (*BLASTER-IRQ*, from 2 to 7) and the interrupt controller is told to let it through; however BODY ends, the controller's mask and the vector are put back as they were, and the card is reset. Stopping what the card was doing (and its DMA channel) first is for BODY to do, with UNWIND-PROTECT.

### `src/pci.lisp`: PCI configuration space, by the ports CF8h and CFCh.

**`pci-read`** `bus device function register` — Function  
The 32 bits at REGISTER (a multiple of 4) of a device's configuration space.

**`pci-write`** `bus device function register value` — Function  
Write the 32 bits of VALUE at REGISTER of a device's configuration space.

**`pci-byte`** `bus device function register` — Function  
The byte at REGISTER of a device's configuration space.

**`pci-word`** `bus device function register` — Function  
The 16 bits at REGISTER (even) of a device's configuration space.

**`pci-present-p`** `` — Function  
Does the machine have PCI, as far as the configuration ports can tell?

**`pci-devices`** `&key (buses 256)` — Function  
Every PCI function there is, as lists: bus, device, function, vendor id, device id, and the 24-bit class code (class, subclass, interface).

### `src/serial.lisp`: Serial ports, by polling the UART: no interrupts.

**`serial-port`** `n` — Function  
The base address of COMn (1 to 4) as the BIOS found it, or NIL if there is none.

**`serial-open`** `&key (port 1016) (baud 115200)` — Function  
Set PORT up for BAUD, 8 data bits, no parity, 1 stop bit.  Returns PORT.

**`serial-byte-ready-p`** `port` — Function  
Has a byte arrived?

**`serial-read-byte`** `port &optional (wait t)` — Function  
The next byte received; with WAIT false, NIL if none has arrived.

**`serial-write-byte`** `port byte` — Function  
Send BYTE, waiting until the port can take it.

**`serial-write-string`** `port string` — Function  
Send STRING, with a carriage return before every newline.

**`serial-read-line`** `port` — Function  
Bytes received up to a carriage return or a line feed, as a string.

### `src/init.lisp`: Setting the library up, at load time and when a saved core starts.

**`doslib-init`** `` — Function  
Set the library up in this process.  It runs when the library is loaded and whenever a core saved with the library in it starts; a program saved with its own :TOPLEVEL function calls it first.

## The examples

Package `doslib-examples`.

### `examples/package.lisp`: The examples: a registry, so that they can be listed and run by name.

**`define-example`** `name function &key (category :other) (description "")` — Function  
Make the example NAME known: FUNCTION (a symbol) runs it.

**`run`** `name &rest arguments` — Function  
Run the example NAME (a string or symbol), passing ARGUMENTS to its MAIN. Ctrl-Break (or Ctrl-Alt-Shift) stops it: the screen goes back to text, the speaker is silenced, and RUN returns.

**`run-arguments`** `&optional (arguments (command-line-arguments))` — Function  
Run the example named by the first of ARGUMENTS, passing it the rest: by default, what follows --end-toplevel-options on the command line, which is how DEMO.BAT passes on what was typed after DEMO.  An argument that is a whole number is passed as a number, anything else as a string.

**`list-examples`** `&optional (stream *standard-output*)` — Function  
Print the examples, by category.

**`example-names`** `` — Function  
The names of all the examples, in alphabetical order.

**`list-names`** `&optional (stream *standard-output*)` — Function  
Print just the names of the examples, by category: it fits on one screen.

**`menu`** `` — Function  
Run the examples asked for by name, until an empty line or q. ? lists their names again, and ?? lists them with descriptions.

**`data-file`** `name` — Function  
The native name of a file in the examples' data directory.

**`build-executable`** `name file` — Function  
Save the example NAME as a stand-alone program in FILE.  This ends the Lisp session.

**`save-core`** `&optional (file "doslib.cor")` — Function  
Save a core with the examples already loaded, so that they start quickly: sbcl --core doslib.cor.  This ends the Lisp session.

## The editor

Package `doslib-emacs`.

### `emacs/buffers.lisp`: Buffers: a vector of lines, a point, a mark

**`buf`** — Structure  

**`win`** — Structure  

**`mode-hook`** `mode name` — Function  

**`buffer-precious-p`** `b` — Function  
Would killing B lose work?

**`*buffers*`** — Variable  

**`*buf*`** — Variable  
The current buffer.

**`*windows*`** — Variable  

**`*window*`** — Variable  
The selected window.

**`*commands*`** — Variable  
Every function defined with DEFINE-COMMAND.

**`*message*`** — Variable  

**`*last-message*`** — Variable  
The most recent echo-area message, for programs to look at.

**`*kill-ring*`** — Variable  

**`*this-command*`** — Variable  

**`*last-command*`** — Variable  

**`*unread*`** — Variable  

**`*prefix-argument*`** — Variable  
The number typed with C-u or M-digits before this command, or NIL.

**`*defining-macro*`** — Variable  

**`*macro-keys*`** — Variable  
The keys of the keyboard macro being defined, newest first.

**`*macro-mark*`** — Variable  

**`*last-macro*`** — Variable  
The keys of the last keyboard macro defined.

**`*repl-package*`** — Variable  
The package the REPL and the evaluation commands read and evaluate in.

**`quit-command`** — Condition  

**`*inhibit-read-only*`** — Variable  
True while a read-only buffer may be changed.

**`define-command`** `name lambda-list &body body` — Macro  
Define a function that can be bound to a key or run with M-x.

**`message`** `control &rest arguments` — Function  
Show a line in the echo area.

**`one-line`** `string` — Function  

**`editor-error`** `control &rest arguments` — Function  

**`make-buf`** `name` — Function  

**`find-buf`** `name` — Function  

**`nlines`** `&optional (b *buf*)` — Function  

**`line-text`** `i &optional (b *buf*)` — Function  

**`current-line`** `` — Function  
The text of the line point is on.

**`point-line`** `` — Function  

**`point-col`** `` — Function  

**`goto`** `line col` — Function  
Move point, staying inside the buffer.

**`split-lines`** `string` — Function  

**`vector-insert`** `vector index item` — Function  

**`vector-delete`** `vector start end` — Function  

**`insert-at`** `line col string &key (b *buf*) (undo t)` — Function  
Insert STRING at LINE, COL; returns the position after it.

**`text-between`** `l1 c1 l2 c2 &optional (b *buf*)` — Function  

**`delete-between`** `l1 c1 l2 c2 &key (b *buf*) (undo t)` — Function  
Delete the text between two positions and return it.

**`insert`** `string` — Function  
Insert STRING at point.

**`buffer-string`** `&optional (b *buf*)` — Function  

### `emacs/screen.lisp`: The screen

**`*screen*`** — Variable  

**`*flush-screen*`** — Variable  

**`*place-cursor*`** — Variable  

**`*leave-display*`** — Variable  

**`reset-display`** `` — Function  

**`*attrs*`** — Variable  

**`put-string`** `x y string attribute &optional (width (length string))` — Function  

**`set-cursor`** `x y` — Function  

**`whitespace-p`** `c` — Function  

**`delimiter-p`** `c` — Function  

**`*highlight-words*`** — Variable  

**`lisp-attributes`** `text` — Function  
Fill *ATTRS* with a colour for each character of a line of Lisp.

**`region-bounds`** `&optional (b *buf*)` — Function  
The mark and point in order: L1 C1 L2 C2.

**`mode-name`** `mode` — Function  

**`visual-col`** `text col` — Function  
The screen column that character COL of TEXT lands in (tabs go to every 8th).

**`draw-window`** `w` — Function  

**`draw-echo`** `&optional (text () prompt)` — Function  

**`redisplay`** `` — Function  

### `emacs/keyboard.lisp`: The keyboard: BIOS codes -> key names like "a", "C-x", "M-f", "<up>"

**`extended-key`** `scan shifted` — Function  

**`*idle-hooks*`** — Variable  
Functions to call while waiting for a key at the top level.

**`add-idle-hook`** `function` — Function  

**`remove-idle-hook`** `function` — Function  

**`read-raw-key`** `` — Function  

**`read-key`** `` — Function  
The next key, by name.  ESC followed by a key is Meta.

### `emacs/keymaps.lisp`: Keymaps

**`*global-map*`** — Variable  

**`mode-map`** `mode` — Function  

**`split-keys`** `string` — Function  

**`bind-key`** `keys command &optional (map *global-map*)` — Function  
Make the key sequence KEYS ("C-x C-f", "M-q", "<f5>") run COMMAND. MAP may be a keymap or the name of a mode, as in (bind-key "C-c t" 'foo 'lisp-mode).

**`read-command`** `` — Function  
Read keys until they name a command.  Returns it (or NIL) and the keys typed.

### `emacs/minibuffer.lisp`: The minibuffer

**`read-string`** `prompt &key (initial "") complete` — Function  
Read a line in the echo area.  COMPLETE, if given, is called on TAB with the text so far and returns the completed text and a list of candidates.

**`complete-from`** `candidates` — Function  
A completion function over a list of strings.

**`complete-file`** `text` — Function  

**`ask-y-or-n`** `question` — Function  

### `emacs/movement.lisp`: Movement

**`forward-char`** `` — Command  
Move point right one character.

**`backward-char`** `` — Command  
Move point left one character.

**`move-lines`** `n` — Function  

**`next-line`** `` — Command  
Move point down one line.

**`previous-line`** `` — Command  
Move point up one line.

**`beginning-of-line`** `` — Command  
Move point to the start of the line.

**`end-of-line`** `` — Command  
Move point to the end of the line.

**`beginning-of-buffer`** `` — Command  
Move point to the start of the buffer.

**`end-of-buffer`** `` — Command  
Move point to the end of the buffer.

**`scroll-up`** `` — Command  
Move forward one screenful.

**`scroll-down`** `` — Command  
Move back one screenful.

**`recenter`** `` — Command  
Put the line with point in the middle of the window.

**`char-at`** `line col` — Function  

**`at-end-p`** `` — Function  

**`at-start-p`** `` — Function  

**`forward-word`** `` — Command  
Move point to the end of the next word.

**`backward-word`** `` — Command  
Move point to the start of the previous word.

**`goto-line`** `` — Command  
Go to a line: the one given as a prefix argument (C-u 42 M-g), or asked for.

### `emacs/editing.lisp`: Editing

**`self-insert`** `key` — Function  

**`newline`** `` — Command  
Insert a line break.

**`tab-to-tab-stop`** `` — Command  
Insert spaces up to the next multiple of 8 columns.

**`quoted-insert`** `` — Command  
Insert the next key literally: C-q TAB inserts a tab character.

**`open-line`** `` — Command  
Insert a line break after point.

**`delete-char`** `` — Command  
Delete the character after point.

**`delete-backward-char`** `` — Command  
Delete the character before point.

**`kill-new`** `text &optional before` — Function  

**`kill-between`** `l1 c1 l2 c2 &optional before` — Function  

**`kill-line`** `` — Command  
Kill to the end of the line; at the end of a line, kill the line break.

**`kill-word`** `` — Command  
Kill to the end of the next word.

**`backward-kill-word`** `` — Command  
Kill to the start of the previous word.

**`set-mark`** `` — Command  
Put the mark where point is.

**`exchange-point-and-mark`** `` — Command  
Swap point and the mark.

**`mark-whole-buffer`** `` — Command  
Put point at the start and the mark at the end.

**`kill-region`** `` — Command  
Kill the text between point and the mark.

**`copy-region`** `` — Command  
Save the text between point and the mark without killing it.

**`yank`** `` — Command  
Insert the most recently killed text.

**`revert-changes`** `from` — Function  
Undo the newest group of changes on the list FROM (a place holding an undo or redo list); returns the records of what that did, for the other list.

**`*changes*`** — Variable  

**`undo`** `` — Command  
Undo the last change; repeat to undo more.

**`redo`** `` — Command  
Redo the change that the last undo took back.

**`keyboard-quit`** `` — Command  
Cancel.

### `emacs/search.lisp`: Search

**`find-text`** `text line col direction` — Function  
The next occurrence of TEXT starting at or after LINE, COL -- or, going backward, the last one starting at or before it: line and column, or NIL.

**`isearch`** `direction` — Function  

**`isearch-forward`** `` — Command  
Search forward as you type.

**`isearch-backward`** `` — Command  
Search backward as you type.

**`replace-string`** `` — Command  
Replace every later occurrence of one string with another.

### `emacs/files.lisp`: Files and buffers

**`select-buffer`** `b` — Function  

**`mode-for-file`** `name` — Function  

**`default-crlf`** `name` — Function  
Should a new file called NAME have DOS line endings?  Not Lisp source: ASDF reads source files as they are, and a carriage return inside a string that runs over several lines would become part of the string.

**`find-file-buffer`** `name` — Function  

**`visit`** `name` — Function  
Show the file NAME in this window -- or, if it is a directory, its listing.

**`find-file`** `` — Command  
Visit a file in its own buffer; a directory is opened in Dired.

**`write-buffer`** `b name` — Function  

**`name-buffer`** `b file` — Function  

**`save-buffer`** `` — Command  
Save the buffer in its file.

**`write-file`** `` — Command  
Save the buffer in a file you name.  It keeps the line endings it has.

**`toggle-line-endings`** `` — Command  
Switch the buffer between DOS (CRLF) and Unix (LF) line endings; saving writes them.

**`load-file`** `` — Command  
Load a file of Lisp code.

**`switch-to-buffer`** `` — Command  
Show another buffer in this window.

**`kill-buffer`** `` — Command  
Get rid of the current buffer.

**`show-in-help`** `name text` — Function  
Put TEXT in the buffer NAME and show it.

**`list-buffers`** `` — Command  
Show all the buffers.

### `emacs/windows.lisp`: Windows

**`layout-windows`** `` — Function  

**`split-window`** `` — Command  
Split the window in two.

**`delete-other-windows`** `` — Command  
Make this window fill the screen.

**`other-window`** `` — Command  
Select the next window.

**`switch-to-repl`** `` — Command  
Show the Lisp REPL in another window and go there.

### `emacs/help.lisp`: Help

**`first-line`** `string` — Function  

**`command-doc`** `command` — Function  

**`describe-key`** `` — Command  
Say what a key does.

**`describe-bindings`** `` — Command  
List every key and what it does.

**`describe-function`** `` — Command  
Show the documentation of a function.

**`execute-command`** `` — Command  
Run a command by name.

**`exit-editor`** `` — Command  
Leave the editor.

### `emacs/macros.lisp`: Keyboard macros: the keys typed between F3 and F4, played back by F4

**`start-macro`** `` — Command  
Start recording a keyboard macro.

**`end-macro`** `` — Command  
Finish recording a keyboard macro.

**`call-macro`** `` — Command  
Play back the last keyboard macro.

**`end-or-call-macro`** `` — Command  
Finish recording a keyboard macro, or play the last one back.

### `emacs/lisp.lisp`: Lisp: parentheses, indentation, evaluation

**`escaped-p`** `line col` — Function  

**`safe-start`** `line` — Function  
A line at or before LINE that starts outside any string, comment or list: the REPL prompt, or the nearest line with a parenthesis in the first column.

**`scan-parens`** `from to-line to-col function` — Function  
Call FUNCTION with true (open) or false (close), the line and the column for every parenthesis that is code -- not in a string, a comment or a character -- from the start of line FROM up to TO-LINE, TO-COL.  Returns true if that position is inside a string.

**`find-open-paren`** `line col` — Function  
The position of the parenthesis that the one just before LINE, COL closes.

**`find-close-paren`** `line col` — Function  
The position after the parenthesis that closes the one at LINE, COL.

**`matching-paren-position`** `` — Function  

**`sexp-start-before-point`** `` — Function  
Where the expression that ends at point begins.

**`*body-forms*`** — Variable  

**`body-form-p`** `word` — Function  

**`indent-under`** `text col` — Function  

**`lisp-indentation`** `target` — Function  
The column line TARGET should start in, or NIL to leave it alone.

**`indent-line`** `` — Command  
Indent the current line as Lisp code.

**`newline-and-indent`** `` — Command  
Insert a line break and indent the new line.

**`*minibuffer-stream*`** — Variable  

**`*editor-restart*`** — Variable  
The restart that abandons the current evaluation.

**`*debugged*`** — Variable  
The condition the debugger was last entered for, in this evaluation.

**`editor-debugger`** `condition &optional hook` — Function  

**`eval-captured`** `form` — Function  
Evaluate FORM.  Returns its values as a list, what it printed, and the condition if the evaluation was abandoned.  Errors enter the debugger.

**`read-forms`** `text` — Function  
All the forms in TEXT, then :OK, :INCOMPLETE, or an error.

**`printed`** `values` — Function  

**`eval-and-show`** `text` — Function  
Evaluate the forms in TEXT; show the result in the echo area, and anything longer in the buffer *output*.

**`eval-last-sexp`** `` — Command  
Evaluate the Lisp expression before point.

**`eval-defun`** `` — Command  
Evaluate the top-level form around point.

**`eval-buffer`** `` — Command  
Evaluate the whole buffer.

**`eval-expression`** `` — Command  
Read a Lisp expression in the minibuffer and evaluate it.

**`name-at-point`** `` — Function  
The symbol's name that point is in or just after, as it is written; or NIL.

**`find-definition`** `` — Command  
Go to the definition of a function, macro, variable or command: by default the one whose name point is on.  M-, comes back.

**`pop-definition`** `` — Command  
Go back to where M-. was last typed.

### `emacs/repl.lisp`: The REPL buffer

**`repl-buffer`** `` — Function  

**`repl-emit`** `string` — Function  

**`repl-prompt`** `` — Function  

**`repl-prompt-line`** `&optional (b *buf*)` — Function  

**`repl-input`** `` — Function  

**`repl-return`** `` — Command  
Evaluate the input at the prompt, if it is complete; otherwise start a new line.

**`repl-history`** `step` — Function  

**`repl-previous-input`** `` — Command  
Recall the previous input.

**`repl-next-input`** `` — Command  
Recall the next input.

**`repl-beginning-of-line`** `` — Command  
Move to the start of the input, or of the line.

### `emacs/dired.lisp`: Dired: a directory as a buffer.  Each line is a file; keys act on it.

**`dentry`** — Structure  

**`*dired-name-column*`** — Variable  

**`cwd-directory`** `` — Function  

**`directory-string`** `text` — Function  
TEXT as the name of a directory: absolute, backslashes, one at the end.

**`parent-directory`** `dir` — Function  

**`directory-name-p`** `name` — Function  

**`read-directory`** `dir` — Function  
The files in DIR as a list of DENTRYs.

**`dired-sorted`** `entries key` — Function  

**`dired-line`** `entry` — Function  

**`dired-render`** `b` — Function  
Write the buffer's entries out as its text.

**`dired-read`** `b &optional keep` — Function  
Read the buffer's directory again; put point on the file named KEEP if it is there.

**`dired-buffer`** `dir` — Function  

**`dired-entry`** `` — Function  

**`dired-path`** `entry` — Function  

**`dired-real-entry`** `` — Function  

**`dired-attributes`** `b i n` — Function  

**`dired-change-directory`** `dir &optional keep` — Function  

**`check-unix`** `ok errno what name` — Function  

**`dired-target`** `prompt` — Function  

**`file-name-of`** `path` — Function  

**`dired`** `` — Command  
Edit a directory: list its files, and open, copy, rename or delete them.

**`dired-next-line`** `` — Command  
Move to the next file.

**`dired-previous-line`** `` — Command  
Move to the previous file.

**`dired-find-file`** `` — Command  
Open the file on this line, or go into the directory.

**`dired-find-file-other-window`** `` — Command  
Open the file on this line in another window.

**`dired-up-directory`** `` — Command  
Go to the directory that contains this one.

**`dired-refresh`** `` — Command  
Read the directory again.

**`dired-flag-deletion`** `` — Command  
Flag this file for deletion (x deletes the flagged files).

**`dired-unflag`** `` — Command  
Take the deletion flag off this file.

**`dired-unflag-all`** `` — Command  
Take the deletion flag off every file.

**`dired-delete-entry`** `entry` — Function  

**`dired-execute`** `` — Command  
Delete the files flagged for deletion.

**`dired-delete`** `` — Command  
Delete the file on this line (an empty directory too).

**`dired-rename`** `` — Command  
Rename the file on this line, or move it by giving a path.

**`dired-copy`** `` — Command  
Copy the file on this line.

**`dired-create-directory`** `` — Command  
Make a new directory.

**`dired-load-file`** `` — Command  
Load the file on this line into Lisp: source, or a compiled fasl.

**`dired-sort`** `` — Command  
Change the order of the listing: by name, by date (newest first), by size (largest first).

**`dired-quit`** `` — Command  
Leave Dired.

**`dired-help`** `` — Command  
Say what the Dired keys are.

### `emacs/main.lisp`: The command loop

**`read-prefix`** `` — Function  
Read a numeric prefix argument, if one is being typed: C-u (4), C-u C-u (16), C-u 12, M-1 M-2.  Returns the number or NIL; the key after it is left to be read again.

**`run-command`** `command keys` — Function  
Run COMMAND -- as many times as the prefix argument says, unless it is a command that takes the argument itself (its TAKES-PREFIX property).

**`editor-interrupted`** `condition hook` — Function  
The debugger hook while the editor runs.  Ctrl-Break (or Ctrl-Alt-Shift) outside an evaluation stops the command that is running, as C-g would if it could be typed, and the editor carries on.  (Inside an evaluation the hook is EDITOR-DEBUGGER, which shows the interrupted code's restarts.)

**`emacs`** `&optional file` — Function  
Run the editor until C-x C-c.

**`build-emacs`** `&optional (name "emacs.exe")` — Function  
Save the editor as a stand-alone program: EMACS [file].
