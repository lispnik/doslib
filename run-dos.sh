#!/bin/sh
# Run SBCL for DOS interactively: FreeDOS 1.4 in a QEMU window, with SBCL,
# ASDF, the DOSLIB project (library, examples, editor) and networking.
#
#   ./run-dos.sh            start the machine (sets it up the first time)
#   ./run-dos.sh setup      rebuild the disks from scratch (erases D:)
#   ./run-dos.sh update     copy the current sbcl.exe, core, contribs and
#                           the doslib sources onto D: again, keeping your
#                           other files
#   ./run-dos.sh build      put ASDF into SBCL.CORE, compile doslib and make
#                           EMACS.EXE and DOSLIB.COR, by running
#                           the machine unattended (some minutes); needed
#                           after setup, and after update to refresh EMACS.EXE
#   ./run-dos.sh put FILE.. copy files from here into D:\SBCL
#   ./run-dos.sh get FILE.. copy files from D:\SBCL to the current directory
#   ./run-dos.sh ls         list D:\SBCL
#
#   ./run-dos.sh utm        make vm/SBCL-DOS.utm, the same machine for the UTM
#                           app (https://mac.getutm.app), and open it there
#   ./run-dos.sh utm update | build | put FILE.. | get FILE.. | ls | setup
#   ./run-dos.sh utm cdrom DISC.ISO   put a disc in the UTM machine's drive
#   ./run-dos.sh macbox     make the same machine for 86Box, as MacBox
#                           (https://github.com/Moonif/MacBox) keeps them:
#                           ~/MacBox/SBCL on FreeDOS, a Pentium II with a VGA
#                           and, beside it, a Hercules monochrome card -- the
#                           two-monitor PC that the editor's M-x live is for.
#                           Then start it from MacBox.  The first time, its
#                           BIOS stops at "CMOS checksum error": press F1.
#   ./run-dos.sh macbox update | build | put FILE.. | get FILE.. | ls | setup
#                           the commands above, for the UTM machine's disks
#                           (the UTM machine has its own copies of the disks)
#
# (Everything but starting it needs the machine to be shut down.)
#
# On the DOS machine:
#   D:\SBCL     sbcl.exe, sbcl.core, CONTRIB\ (ASDF, sb-bsd-sockets), SBCLRC
#               (the init file: loads ASDF and tells it about D:\DOSLIB),
#               EMACS.EXE, DEMO.BAT, DOSLIB.COR (a core with the examples
#               loaded, for DEMO), QL.BAT (Quicklisp), the packet driver,
#               WATTCP.CFG
#   D:\DOSLIB   the doslib project: doslib.asd, src\, examples\, emacs\
# It boots into D:\SBCL with LBACACHE (a disk cache: the 26 MB Lisp core is
# read from disk once, not at every start), DOSLFN (long file names),
# HDPMI32 (the DPMI host), a packet driver for the emulated network card
# and the CTMOUSE mouse driver loaded.
# Ports on this computer are forwarded to the DOS machine:
#   127.0.0.1:8098 -> 8080   (the websrv example)   change with WEB_PORT=...
#   127.0.0.1:2323 -> 2323   (the remote example)   change with REPL_PORT=...
#
# In the QEMU window: Ctrl-Alt-G releases the mouse, Option is Alt.
# To switch the machine off, type FDAPM POWEROFF at the DOS prompt (or just
# close the window once your files are saved).
#
# Needs qemu-system-i386 and mtools, and two directories besides this one:
#   SBCL_DOS_TOOLS  (default: the directory above this one) with
#                   dl-freedos/lite/FD14LITE.img, the FreeDOS 1.4 "LiteUSB"
#                   image, dl-pktdrv/crynwr, the Crynwr packet drivers, and
#                   (optionally) dl-quicklisp/quicklisp.lisp from
#                   https://beta.quicklisp.org/quicklisp.lisp;
#                   the machines' disks are kept in its vm/ directory
#   SBCL_SRC        (default: sbcl-dos-experiment next to SBCL_DOS_TOOLS)
#                   the SBCL source tree, built for DOS with make-dos.sh
#                   and make-dos-contrib.sh asdf sb-bsd-sockets
# Extra QEMU options go in RUN_DOS_QEMU_OPTIONS.
# Every machine has a CD-ROM drive, E: in DOS.  RUN_DOS_CDROM=disc.iso puts
# a disc in QEMU's, "./run-dos.sh utm cdrom disc.iso" in the UTM machine's,
# and 86Box's Media menu in its own.
set -e
here=`cd "\`dirname "$0"\`" && pwd`
tools=${SBCL_DOS_TOOLS:-`cd "$here/.." && pwd`}
src=${SBCL_SRC:-`cd "$tools/../sbcl-dos-experiment" && pwd`}
vm=$tools/vm
boot=$vm/boot.img
data=$vm/data.img
disk_mb=256
web_port=${WEB_PORT:-8098}
repl_port=${REPL_PORT:-2323}
mkdir -p "$vm"
# "utm" as the first word: work on the UTM machine's disks instead
utm=
bundle=$vm/SBCL-DOS.utm
if [ "$1" = utm ]; then
    utm=yes; shift
    mkdir -p "$bundle/Data"
    boot=$bundle/Data/boot.img
    data=$bundle/Data/data.img
fi
# "macbox" as the first word: work on the 86Box machine's disks
macbox=
macdir=${MACBOX_DIR:-$HOME/MacBox}/SBCL\ on\ FreeDOS
macdir=`eval echo "$macdir"`
if [ "$1" = macbox ]; then
    macbox=yes; shift
    mkdir -p "$macdir/disks"
    boot=$macdir/disks/boot.img
    data=$macdir/disks/data.img
fi
MTOOLSRC=$vm/mtoolsrc; MTOOLS_SKIP_CHECK=1; export MTOOLSRC MTOOLS_SKIP_CHECK
# The DOS machine's clock runs in UTC, and file times on its disks should
# agree with it: otherwise a source file copied in looks hours older than
# it is, and ASDF does not compile it again.
TZ=UTC; export TZ
printf 'drive c: file="%s" partition=1\ndrive d: file="%s" partition=1\n' "$boot" "$data" > "$MTOOLSRC"

running() {
    pgrep -f "$data" > /dev/null 2>&1 && return 0
    [ -n "$macbox" ] && pgrep -f "86Box.*SBCL on FreeDOS" > /dev/null 2>&1
}
need_stopped() {
    if running; then echo "run-dos.sh: shut the DOS machine down first" >&2; exit 1; fi
}
# copy a text file with DOS line endings
put_text() {    # source, name on D:\SBCL
    sed 's/$/\r/' "$1" > "$vm/crlf.tmp"
    mcopy -o "$vm/crlf.tmp" "d:/SBCL/$2"
    rm -f "$vm/crlf.tmp"
}

make_boot() {
    cp "$tools/dl-freedos/lite/FD14LITE.img" "$boot"
    # BUILD.BAT, when there is one, is an unattended job (see "build" below):
    # run it, remove it, switch off.
    printf '%s\r\n' '@echo off' 'SET DOSDIR=\FREEDOS' 'SET PATH=\FREEDOS\BIN;D:\SBCL' \
        'SET HOME=D:\HOME' 'SET WATTCP.CFG=D:\SBCL' \
        'LBACACHE BUF 40000 > NUL' \
        'SHSUCDX /D:SBCLCD01 /QQ > NUL' \
        'DOSLFN > NUL' 'D:' 'CD \SBCL' 'HDPMI32 -r > NUL' 'PCNTPK INT=0x60 > NUL' \
        'IF EXIST CTMOUSE.EXE CTMOUSE > NUL' \
        'IF EXIST BUILD.BAT GOTO build' 'CLS' \
        'ECHO SBCL for DOS.  Try:   sbcl    emacs    demo    demo plasma    type readme.txt' \
        'GOTO end' ':build' 'CALL BUILD.BAT' 'DEL BUILD.BAT' 'C:' 'FDAPM POWEROFF' ':end' \
        > "$vm/fdauto.bat"
    mcopy -o "$vm/fdauto.bat" c:/FDAUTO.BAT
    # A CD-ROM drive, when the machine has one, is E: -- UDVD2 is the driver
    # for the drive, SHSUCDX (above, and before DOSLFN, which then gives the
    # disc's long names) makes it a drive letter.  Without a drive neither
    # stays in memory.
    mtype c:/FDCONFIG.SYS | tr -d '\r' \
        | awk '{print} /HIMEMX/ {print "DEVICE=\\FREEDOS\\BIN\\UDVD2.SYS /D:SBCLCD01"}' \
        | sed 's/$/\r/' > "$vm/fdconfig.sys"
    mcopy -o "$vm/fdconfig.sys" c:/FDCONFIG.SYS
    mdel c:/SETUP.BAT 2>/dev/null || true
}
make_data() {
    cyl=`expr $disk_mb \* 2048 / 1008`
    rm -f "$data"
    dd if=/dev/zero of="$data" bs=1048576 count=0 seek=$disk_mb 2>/dev/null
    mpartition -I -t $cyl -h 16 -s 63 d: 2>/dev/null
    mpartition -c -a -t $cyl -h 16 -s 63 d: 2>/dev/null
    mformat d:
    mmd d:/SBCL d:/SBCL/CONTRIB d:/HOME
}
# The names the demos had on D:\SBCL before they became the doslib project
old_demos="asm bench bmpview diskinfo dosinfo doslib emacs fire font gcviz intvec ivt life lifeexe
           live lsystem mandel mcbwalk memmap modex ntp pciscan plasma ports raycast raytrace remote
           serial shell snake special stars sysinfo textfx turtle vesa websrv wget wire3d"
copy_files() {
    echo "copying SBCL and the doslib project onto D:"
    mmd -D s d:/SBCL d:/SBCL/CONTRIB d:/HOME 2>/dev/null || true
    mcopy -o "$src/src/runtime/sbcl.exe" "$src/src/runtime/hdpmi32.exe" "$src/output/sbcl.core" d:/SBCL/
    [ -f "$src/obj/sbcl-home/contrib/asdf.fasl" ] \
        || { echo "run-dos.sh: ASDF is not built: run ./make-dos-contrib.sh asdf sb-bsd-sockets in $src" >&2; exit 1; }
    mcopy -o "$src"/obj/sbcl-home/contrib/* d:/SBCL/CONTRIB/
    mcopy -o "$tools/dl-pktdrv/crynwr/DRIVERS/CRYNWR/PCNTPK.COM" d:/SBCL/
    printf 'my_ip = dhcp\r\n' > "$vm/wattcp.cfg"; mcopy -o "$vm/wattcp.cfg" d:/SBCL/WATTCP.CFG
    # The mouse driver: FreeDOS's CTMOUSE, from the package on its install image
    if command -v unzip > /dev/null && mcopy -n -o c:/packages/base/ctmouse.zip "$vm/ctmouse.zip" 2>/dev/null; then
        unzip -o -q -j "$vm/ctmouse.zip" BIN/CTMOUSE.EXE -d "$vm" && mcopy -o "$vm/CTMOUSE.EXE" d:/SBCL/
        rm -f "$vm/ctmouse.zip" "$vm/CTMOUSE.EXE"
    fi
    for name in $old_demos; do
        mdel "d:/SBCL/`echo $name | tr a-z A-Z`.LSP" 2>/dev/null || true
    done
    mdel d:/SBCL/PICTURE.BMP 2>/dev/null || true

    # The project, afresh (so nothing stale is left, compiled files
    # included: "build" makes those again).  Documentation gets DOS line
    # endings.  Lisp source does not: ASDF reads it as it is, and a carriage
    # return inside a string would be part of the string.
    mdeltree d:/DOSLIB 2>/dev/null || true
    stage=$vm/stage; rm -rf "$stage"; mkdir -p "$stage"
    (cd "$here" && find . -type f ! -path './doc/*' ! -path './.git/*' ! -name '*.fasl' ! -name '.*' \
                        ! -name run-dos.sh) | while read -r f; do
        mkdir -p "$stage/DOSLIB/`dirname "$f"`"
        case "$f" in
            *.md|*.txt) sed 's/$/\r/' "$here/$f" > "$stage/DOSLIB/$f" ;;
            *) cp "$here/$f" "$stage/DOSLIB/$f" ;;
        esac
    done
    mcopy -s -o -Q "$stage/DOSLIB" d:/
    rm -rf "$stage"

    # SBCL's init file
    cat > "$vm/text.tmp" <<'EOF'
;;; SBCLRC -- read by SBCL when it starts (it lives next to SBCL.CORE).
;; ("build" saves SBCL.CORE again with ASDF already in it, so this costs
;; nothing after that.)
(require :asdf)
;; Compiled files go next to their sources, not into a cache directory:
;; DOS paths are short.
(asdf:disable-output-translations)
;; Projects ASDF should know about: add yours here.
(pushnew #p"D:/DOSLIB/" asdf:*central-registry* :test #'equal)
EOF
    put_text "$vm/text.tmp" SBCLRC
    cat > "$vm/text.tmp" <<'EOF'
@echo off
REM DEMO        list the doslib examples and choose one
REM DEMO name   run one, for example DEMO PLASMA
REM DOSLIB.COR is a core with the examples loaded already ("build" makes it);
REM without it they are loaded through ASDF, which takes longer.
SET DEMOSBCL=sbcl --noinform --eval "(asdf:load-system :doslib/examples)"
IF EXIST D:\SBCL\DOSLIB.COR SET DEMOSBCL=sbcl --core D:\SBCL\DOSLIB.COR --noinform
IF "%1"=="" GOTO menu
%DEMOSBCL% --eval "(doslib-examples:run-arguments)" --quit --end-toplevel-options %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO end
:menu
%DEMOSBCL% --eval "(doslib-examples:menu)" --quit
:end
SET DEMOSBCL=
EOF
    put_text "$vm/text.tmp" DEMO.BAT
    # Quicklisp: QL.BAT installs it the first time (over the network, into
    # D:\HOME\quicklisp) and starts SBCL with it loaded after that.
    if [ -f "$tools/dl-quicklisp/quicklisp.lisp" ]; then
        mcopy -o "$tools/dl-quicklisp/quicklisp.lisp" d:/SBCL/
    fi
    cat > "$vm/text.tmp" <<'EOF'
@echo off
REM QL   start SBCL with Quicklisp; the first time, install Quicklisp
IF EXIST D:\HOME\quicklisp\setup.lisp GOTO have
IF NOT EXIST D:\SBCL\quicklisp.lisp GOTO missing
sbcl --noinform --load D:\SBCL\quicklisp.lisp --eval "(quicklisp-quickstart:install)"
GOTO end
:missing
ECHO D:\SBCL\quicklisp.lisp is not there: get it from https://beta.quicklisp.org/quicklisp.lisp
GOTO end
:have
sbcl --noinform --load D:\HOME\quicklisp\setup.lisp
:end
EOF
    put_text "$vm/text.tmp" QL.BAT
    cat > "$vm/text.tmp" <<EOF
SBCL for DOS
============
sbcl                the Lisp prompt, with ASDF loaded; (sb-ext:exit) leaves
demo                list the examples and choose one
demo plasma         run one example (any key usually ends it)
demo speaker scale  ... with arguments for it
Ctrl-Break          stops Lisp that is running; so does Ctrl-Alt-Shift, for
                    keyboards without a Break key
emacs [file]        the editor: F1 lists keys, C-c C-z is the REPL, C-x d is
                    Dired, C-x C-c leaves

The doslib project is in D:\\DOSLIB (doslib.asd).  At the Lisp prompt:
  (asdf:load-system :doslib)             the library
  (asdf:load-system :doslib/examples)    then (doslib-examples:menu)
                                         or   (doslib-examples:run "fire")
  (asdf:load-system :doslib/emacs)       then (doslib-emacs:emacs)

Network examples (the packet driver is loaded at boot):
  (doslib-examples:run "wget" "http://example.com/")
  (doslib-examples:run "websrv")   host: open http://127.0.0.1:$web_port/
  (doslib-examples:run "remote")   host: nc 127.0.0.1 $repl_port
  (doslib-examples:run "ntp")

Quicklisp:  ql   installs it the first time (it needs the network), and
after that starts SBCL with it loaded:  (ql:quickload "alexandria")

Your own systems: put them in a directory, add it to D:\\SBCL\\SBCLRC.

FDAPM POWEROFF switches the machine off.
EOF
    put_text "$vm/text.tmp" README.TXT; rm -f "$vm/text.tmp"
}

# Compile the project and make EMACS.EXE, on the DOS machine itself: boot
# it without a window to run BUILD.BAT, which FDAUTO.BAT looks for.
build() {
    cat > "$vm/text.tmp" <<'EOF'
@echo off
SET SBCL_DOS_MERGE_STDERR=1
REM SBCL.CORE again, with ASDF (which SBCLRC has just loaded) inside it
sbcl --noinform --non-interactive --load D:\SBCL\MKCORE.LSP > BUILD.LOG
sbcl --noinform --non-interactive --eval "(asdf:load-system :doslib/examples)" >> BUILD.LOG
IF EXIST DOSLIB.COR DEL DOSLIB.COR
sbcl --noinform --non-interactive --eval "(asdf:load-system :doslib/examples)" --eval "(doslib-examples:save-core)" >> BUILD.LOG
IF EXIST EMACS.EXE DEL EMACS.EXE
sbcl --noinform --non-interactive --eval "(asdf:load-system :doslib/emacs-extras)" --eval "(doslib-emacs:build-emacs)" >> BUILD.LOG
EOF
    put_text "$vm/text.tmp" BUILD.BAT
    # (the running Lisp has read its core into memory, so it can write over the file)
    printf '(sb-ext:enable-debugger)\n(sb-ext:save-lisp-and-die "D:/SBCL/sbcl.core")\n' > "$vm/text.tmp"
    put_text "$vm/text.tmp" MKCORE.LSP; rm -f "$vm/text.tmp"
    # (and the old results, so that a build that dies half way is not taken for a good one)
    mdel d:/SBCL/BUILD.LOG d:/SBCL/DOSLIB.COR d:/SBCL/EMACS.EXE 2>/dev/null || true
    echo "building doslib on the DOS machine (this takes some minutes)"
    # (with the network card: the packet driver in FDAUTO.BAT waits for one)
    qemu-system-i386 -m 512 -cpu pentium3 -display none -no-reboot -nic user,model=pcnet \
        -drive file="$boot",format=raw,if=ide,index=0 \
        -drive file="$data",format=raw,if=ide,index=1 > /dev/null 2>&1 &
    qemu=$!
    waited=0
    while kill -0 $qemu 2>/dev/null; do
        if [ $waited -ge ${BUILD_TIMEOUT:-3600} ]; then
            echo "run-dos.sh: the build did not finish; stopping it" >&2
            kill -9 $qemu 2>/dev/null; break
        fi
        sleep 2; waited=`expr $waited + 2`
    done
    wait $qemu 2>/dev/null || true
    mdel d:/SBCL/BUILD.BAT d:/SBCL/MKCORE.LSP 2>/dev/null || true
    mtype d:/SBCL/BUILD.LOG 2>/dev/null | tr -d '\r' > "$vm/build.log" || true
    if mdir d:/SBCL/EMACS.EXE > /dev/null 2>&1 && mdir d:/SBCL/DOSLIB.COR > /dev/null 2>&1 && ! grep -q 'Unhandled\|debugger invoked\|COMPILE-FILE-ERROR' "$vm/build.log"; then
        echo "built: the examples are compiled, DOSLIB.COR and EMACS.EXE are made (log: $vm/build.log)"
    else
        tail -30 "$vm/build.log"
        echo "run-dos.sh: the build failed; the whole log is $vm/build.log" >&2
        exit 1
    fi
}

# The UTM machine's CD-ROM drive.  UTM gives every IDE drive a bus of its
# own, and a PC has two, which the disks take: so the drive is put second
# on the second bus with QEMU's own arguments (see the description below),
# and its disc is a file in the bundle, Data/cdrom.iso, that
# "./run-dos.sh utm cdrom DISC.ISO" replaces.  To begin with it is a disc
# with a note on it.
make_utm_disc() {
    [ -f "$bundle/Data/cdrom.iso" ] && return 0
    mkdir -p "$vm/disc.tmp"
    printf 'The disc of the SBCL on FreeDOS machine.\r\nPut another in:  ./run-dos.sh utm cdrom DISC.ISO\r\n' \
        > "$vm/disc.tmp/README.TXT"
    hdiutil makehybrid -quiet -iso -joliet -default-volume-name NODISC -o "$bundle/Data/cdrom.iso" "$vm/disc.tmp"
    rm -f "$vm/disc.tmp/README.TXT"; rmdir "$vm/disc.tmp"
}

# The description of the machine that UTM reads.  Written once, so that the
# machine keeps its identity in UTM.
make_utm_config() {
    make_utm_disc
    [ -f "$bundle/config.plist" ] && return
    cat > "$bundle/config.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Backend</key><string>QEMU</string>
  <key>ConfigurationVersion</key><integer>4</integer>
  <key>Information</key>
  <dict>
    <key>Name</key><string>SBCL on FreeDOS</string>
    <key>UUID</key><string>`uuidgen`</string>
    <key>Icon</key><string>msdos</string>
    <key>IconCustom</key><false/>
    <key>Notes</key><string>SBCL for DOS: type sbcl, emacs, or type readme.txt.  Host port $web_port goes to the web server demo (8080), $repl_port to the remote REPL demo (2323).  Made by run-dos.sh.</string>
  </dict>
  <key>System</key>
  <dict>
    <key>Architecture</key><string>i386</string>
    <key>Target</key><string>pc</string>
    <key>CPU</key><string>pentium3</string>
    <key>CPUCount</key><integer>1</integer>
    <key>CPUFlagsAdd</key><array/>
    <key>CPUFlagsRemove</key><array/>
    <key>ForceMulticore</key><false/>
    <key>JITCacheSize</key><integer>0</integer>
    <key>MemorySize</key><integer>512</integer>
  </dict>
  <key>QEMU</key>
  <dict>
    <key>AdditionalArguments</key>
    <array>
      <string>-drive</string>
      <string>if=none,id=sbclcd,media=cdrom,readonly=on,format=raw,file=$bundle/Data/cdrom.iso</string>
      <string>-device</string>
      <string>ide-cd,bus=ide.1,unit=1,drive=sbclcd</string>
    </array>
    <key>BalloonDevice</key><false/>
    <key>DebugLog</key><false/>
    <key>Hypervisor</key><false/>
    <key>PS2Controller</key><true/>
    <key>RNGDevice</key><false/>
    <key>RTCLocalTime</key><false/>
    <key>TPMDevice</key><false/>
    <key>TSO</key><false/>
    <key>UEFIBoot</key><false/>
  </dict>
  <key>Input</key>
  <dict>
    <key>MaximumUsbShare</key><integer>3</integer>
    <key>UsbBusSupport</key><string>Disabled</string>
    <key>UsbSharing</key><false/>
  </dict>
  <key>Sharing</key>
  <dict>
    <key>ClipboardSharing</key><false/>
    <key>DirectoryShareMode</key><string>None</string>
    <key>DirectoryShareReadOnly</key><false/>
  </dict>
  <key>Display</key>
  <array>
    <dict>
      <key>Hardware</key><string>VGA</string>
      <key>DynamicResolution</key><false/>
      <key>NativeResolution</key><false/>
      <key>DownscalingFilter</key><string>Linear</string>
      <key>UpscalingFilter</key><string>Nearest</string>
    </dict>
    <dict>
      <key>Hardware</key><string>secondary-vga</string>
      <key>DynamicResolution</key><false/>
      <key>NativeResolution</key><false/>
      <key>DownscalingFilter</key><string>Linear</string>
      <key>UpscalingFilter</key><string>Nearest</string>
    </dict>
  </array>
  <key>Drive</key>
  <array>
    <dict>
      <key>Identifier</key><string>`uuidgen`</string>
      <key>ImageName</key><string>boot.img</string>
      <key>ImageType</key><string>Disk</string>
      <key>Interface</key><string>IDE</string>
      <key>InterfaceVersion</key><integer>1</integer>
      <key>ReadOnly</key><false/>
    </dict>
    <dict>
      <key>Identifier</key><string>`uuidgen`</string>
      <key>ImageName</key><string>data.img</string>
      <key>ImageType</key><string>Disk</string>
      <key>Interface</key><string>IDE</string>
      <key>InterfaceVersion</key><integer>1</integer>
      <key>ReadOnly</key><false/>
    </dict>
  </array>
  <key>Network</key>
  <array>
    <dict>
      <key>Hardware</key><string>pcnet</string>
      <key>Mode</key><string>Emulated</string>
      <key>MacAddress</key><string>52:54:00:5B:C1:D0</string>
      <key>IsolateFromHost</key><false/>
      <key>PortForward</key>
      <array>
        <dict>
          <key>Protocol</key><string>TCP</string>
          <key>HostAddress</key><string>127.0.0.1</string>
          <key>HostPort</key><integer>$web_port</integer>
          <key>GuestPort</key><integer>8080</integer>
        </dict>
        <dict>
          <key>Protocol</key><string>TCP</string>
          <key>HostAddress</key><string>127.0.0.1</string>
          <key>HostPort</key><integer>$repl_port</integer>
          <key>GuestPort</key><integer>2323</integer>
        </dict>
      </array>
    </dict>
  </array>
  <key>Serial</key><array/>
  <key>Sound</key>
  <array>
    <dict>
      <key>Hardware</key><string>adlib</string>
      <key>Identifier</key><string>`uuidgen`</string>
    </dict>
    <dict>
      <key>Hardware</key><string>pcspk</string>
      <key>Identifier</key><string>`uuidgen`</string>
    </dict>
    <dict>
      <key>Hardware</key><string>sb16</string>
      <key>Identifier</key><string>`uuidgen`</string>
    </dict>
  </array>
</dict>
</plist>
EOF
}

# 86Box's description of the machine.  86Box rewrites this file with the
# settings made in its own window, so it is written only if there is none.
make_macbox_config() {
    [ -f "$macdir/86box.cfg" ] && return 0
    cat > "$macdir/86box.cfg" <<EOF
[Machine]
machine = p2bls
cpu_family = pentium2_deschutes
cpu_speed = 400000000
cpu_multi = 4
cpu_use_dynarec = 1
mem_size = 524288
time_sync = utc

[Input devices]
keyboard_type = keyboard_ps2
mouse_type = ps2

[Video]
gfxcard = cl_gd5446_pci
gfxcard_2 = hercules

[Sound]
sndcard = sb16

[Network]
net_01_card = pcnetpci
net_01_net_type = slirp
net_01_link = 0

[Hard disks]
hdd_01_fn = disks/boot.img
hdd_01_ide_channel = 0:0
hdd_01_parameters = 63, 16, 65, 0, ide
hdd_02_fn = disks/data.img
hdd_02_ide_channel = 0:1
hdd_02_parameters = 63, 16, $((disk_mb * 2048 / 1008)), 0, ide

[Floppy and CD-ROM drives]
fdd_01_type = 35_2hd
fdd_02_type = none
cdrom_01_parameters = 1, atapi
cdrom_01_ide_channel = 1:0
cdrom_01_speed = 24
EOF
}

# MacBox shows the machines listed in its Config file (JSON), not the
# folders it finds, and writes that file itself: so add ours only while it
# is not running.
register_macbox() {
    list="$(dirname "$macdir")/Config"
    if grep -q -F "SBCL on FreeDOS" "$list" 2>/dev/null; then return; fi
    if pgrep -x MacBox > /dev/null 2>&1; then
        echo "run-dos.sh: quit MacBox and run this again, to add the machine to its list" >&2
        return
    fi
    python3 - "$list" "$macdir" <<'EOF'
import json, os, sys
path, machine = sys.argv[1], sys.argv[2]
try:
    machines = json.load(open(path))
except (OSError, ValueError):
    machines = []
machines.append({'name': 'SBCL on FreeDOS', 'path': machine,
                 'description': 'FreeDOS with SBCL, doslib and the editor. VGA plus a monochrome card.'})
json.dump(machines, open(path, 'w'))
EOF
}

command=${1:-run}
if [ -n "$utm" ] && [ "$command" = run ]; then command=utm-open; fi
if [ -n "$macbox" ] && [ "$command" = run ]; then command=macbox-open; fi
case "$command" in
    macbox-open)
        need_stopped
        if [ ! -f "$boot" ] || [ ! -f "$data" ]; then make_boot; make_data; copy_files; build; fi
        make_macbox_config
        register_macbox
        echo "the machine is in $macdir; start \"SBCL on FreeDOS\" from MacBox"
        echo "(the first time, press F1 when its BIOS reports a CMOS checksum error)"
        open -a MacBox ;;
    utm-open)
        if [ ! -f "$boot" ] || [ ! -f "$data" ]; then make_boot; make_data; copy_files; build; fi
        make_utm_config
        echo "opening $bundle in UTM; press its play button to start the machine"
        open "$bundle" ;;
    cdrom)
        [ -n "$utm" ] || { echo "run-dos.sh: cdrom is for the UTM machine (QEMU: RUN_DOS_CDROM=DISC.ISO; 86Box: its Media menu)" >&2; exit 2; }
        [ -f "$2" ] || { echo "usage: $0 utm cdrom DISC.ISO" >&2; exit 2; }
        if [ "`utmctl status 'SBCL on FreeDOS' 2>/dev/null`" = started ]; then
            echo "run-dos.sh: shut the DOS machine down first" >&2; exit 1
        fi
        cp "$2" "$bundle/Data/cdrom.iso"
        echo "$2 is the disc in the UTM machine's drive, E:" ;;
    setup)
        need_stopped; make_boot; make_data; copy_files
        echo "now run:  $0 ${utm:+utm }${macbox:+macbox }build" ;;
    build)
        need_stopped; [ -f "$data" ] || { echo "run-dos.sh: nothing set up yet" >&2; exit 1; }
        build ;;
    update)
        need_stopped; [ -f "$data" ] || { echo "run-dos.sh: nothing set up yet" >&2; exit 1; }
        make_boot; copy_files ;;
    put)
        need_stopped; shift
        for f in "$@"; do mcopy -o "$f" d:/SBCL/; done ;;
    get)
        need_stopped; shift
        for f in "$@"; do mcopy -n -o "d:/SBCL/$f" .; done ;;
    ls)
        need_stopped; mdir d:/SBCL ;;
    run)
        if running; then echo "run-dos.sh: the DOS machine is already running" >&2; exit 1; fi
        if [ ! -f "$boot" ] || [ ! -f "$data" ]; then make_boot; make_data; copy_files; build; fi
        # The PC speaker, an AdLib card and a Sound Blaster 16, heard through the Mac (RUN_DOS_SOUND=0: neither)
        sound="-audiodev coreaudio,id=snd -machine pcspk-audiodev=snd -device adlib,audiodev=snd -device sb16,audiodev=snd"
        if [ "${RUN_DOS_SOUND:-1}" = 0 ]; then sound=; fi
        # A second display card, for the editor's M-x live (QEMU's own window
        # shows one display at a time: its View menu switches between them)
        exec qemu-system-i386 -m 512 -cpu pentium3 -name "SBCL on FreeDOS" -device secondary-vga \
            -drive file="$boot",format=raw,if=ide,index=0 \
            -drive file="$data",format=raw,if=ide,index=1 \
            -nic user,model=pcnet,hostfwd=tcp:127.0.0.1:$web_port-:8080,hostfwd=tcp:127.0.0.1:$repl_port-:2323 \
            ${RUN_DOS_CDROM:+-cdrom "$RUN_DOS_CDROM"} $sound $RUN_DOS_QEMU_OPTIONS ;;
    *)
        sed -n "2,60p" "$0" | sed 's/^# \{0,1\}//'; exit 2 ;;
esac
