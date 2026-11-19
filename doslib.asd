;;;; DOSLIB: Common Lisp on DOS with SBCL.
;;;;
;;;;   doslib            the library: BIOS, DOS, ports, memory, video
;;;;   doslib/examples   programs that use it; (doslib-examples:menu) lists them
;;;;   doslib/emacs      an Emacs-like editor with a REPL, a debugger and Dired
;;;;   doslib/emacs-extras   more for the editor: modes and commands that use
;;;;                     the examples and the network

(defsystem "doslib"
  :description "BIOS, DOS, port and memory access for SBCL on DOS."
  :version "0.2.0"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "machine")
               (:file "ports")
               (:file "dos")
               (:file "cpuid")
               (:file "memory")
               (:file "interrupts")
               (:file "hotkey")
               (:file "video")
               (:file "input")
               (:file "text")
               (:file "critical")
               (:file "dialog")
               (:file "sound")
               (:file "mouse")
               (:file "disk")
               (:file "pci")
               (:file "serial")
               (:file "init")))

(defsystem "doslib/examples"
  :description "Example programs for DOSLIB: graphics, system information, Lisp, networking."
  :version "0.2.0"
  :depends-on ("doslib" "sb-bsd-sockets")
  :pathname "examples"
  :components ((:file "package")
               (:module "graphics"
                :depends-on ("package")
                :components ((:file "bmpview")
                             (:file "fire")
                             (:file "font")
                             (:file "gcviz")
                             (:file "lsystem")
                             (:file "mandel")
                             (:file "modex")
                             (:file "paint")
                             (:file "plasma")
                             (:file "raycast")
                             (:file "raytrace")
                             (:file "sprite")
                             (:file "stars")
                             (:file "textfx")
                             (:file "turtle")
                             (:file "vesa")
                             (:file "wire3d")))
               (:module "system"
                :depends-on ("package")
                :components ((:file "asm")
                             (:file "asmdsl")
                             (:file "calc")
                             (:file "fatview")
                             (:file "printer")
                             (:file "probes")
                             (:file "diskinfo")
                             (:file "dosinfo")
                             (:file "intvec")
                             (:file "ivt")
                             (:file "mcbwalk")
                             (:file "memmap")
                             (:file "pciscan")
                             (:file "ports")
                             (:file "serial")
                             (:file "sysinfo")))
               (:module "lisp"
                :depends-on ("package")
                :components ((:file "bench")
                             (:file "hotfix")
                             (:file "life")
                             (:file "live")
                             (:file "restarts")
                             (:file "resume")
                             (:file "shell")
                             (:file "snake")
                             (:file "special")
                             (:file "windows")))
               (:module "sound"
                :depends-on ("package")
                :components ((:file "adlib")     ; and the notation both use
                             (:file "blaster")
                             (:file "speaker" :depends-on ("adlib"))))
               (:module "network"
                :depends-on ("package")
                :components ((:file "browse")
                             (:file "files")
                             (:file "irc")
                             (:file "netpong")
                             (:file "ntp")
                             (:file "remote")
                             (:file "websrv")
                             (:file "wget")))))

(defsystem "doslib/emacs"
  :description "An Emacs-like text editor for DOS, written in and extended with Common Lisp."
  :version "0.2.0"
  :depends-on ("doslib")
  :pathname "emacs"
  :serial t
  :components ((:file "package")
               (:file "buffers")
               (:file "screen")
               (:file "keyboard")
               (:file "keymaps")
               (:file "minibuffer")
               (:file "movement")
               (:file "editing")
               (:file "search")
               (:file "lisp")
               (:file "repl")
               (:file "files")
               (:file "dired")
               (:file "windows")
               (:file "help")
               (:file "macros")
               (:file "keys")
               (:file "main")))

(defsystem "doslib/emacs-extras"
  :description "More for the editor: commands and modes built on the examples and the network."
  :version "0.2.0"
  :depends-on ("doslib/emacs" "doslib/examples")
  :pathname "emacs-extras"
  :components ((:file "commands")
               (:file "hex")
               (:file "web")
               (:file "irc")
               (:file "games")
               (:file "live")))
