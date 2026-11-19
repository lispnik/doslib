;;;; Reading memory by address, and making a device's memory addressable.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; Memory: the DPMI client's address space is flat and starts at zero, so
;;; the first megabyte (BIOS data, video memory, ROM) is just there.
(defun peek (address)
  "The byte at ADDRESS."
  (sap-ref-8 (int-sap address) 0))
(defun peek16 (address)
  "The 16 bits at ADDRESS."
  (sap-ref-16 (int-sap address) 0))
(defun peek32 (address)
  "The 32 bits at ADDRESS."
  (sap-ref-32 (int-sap address) 0))

;;; A device's memory -- a VESA linear framebuffer, a PCI card's registers --
;;; is at a physical address, high in the machine's address space, and a
;;; physical address is not one a protected-mode program can use: its
;;; addresses go through the page tables of the DPMI host.  DPMI function
;;; 0800h (INT 31h, BX:CX = physical address, SI:DI = size) has the host map
;;; the memory and returns the address it is at -- which here, where the
;;; address space is flat and starts at 0, is an address like any other.
;;; INT86 does not carry SI, so the call has a stub of its own:
;;; unsigned stub(unsigned physical, unsigned size), 0 if the host would not.
(defvar *map-physical-code*
  (saved-code '(#x53 #x56 #x57                  ; push ebx / esi / edi
                #x8B #x4C #x24 #x10             ; mov ecx,[esp+16]   the address
                #x89 #xCB #xC1 #xEB #x10        ; mov ebx,ecx / shr ebx,16
                #x8B #x7C #x24 #x14             ; mov edi,[esp+20]   the size
                #x89 #xFE #xC1 #xEE #x10        ; mov esi,edi / shr esi,16
                #xB8 #x00 #x08 #x00 #x00        ; mov eax,0800h
                #xCD #x31                       ; int 31h
                #x72 10                         ; jc failed
                #xC1 #xE3 #x10 #x66 #x89 #xCB   ; shl ebx,16 / mov bx,cx
                #x89 #xD8 #xEB #x02             ; mov eax,ebx / jmp done
                #x31 #xC0                       ; failed: xor eax,eax
                #x5F #x5E #x5B #xC3)))          ; done: pop edi / esi / ebx / ret
(defun map-physical (physical size)
  "Make SIZE bytes of a device's memory, at the physical address PHYSICAL,
addressable (DPMI function 0800h).  Returns the address they are at, to use
with PEEK or INT-SAP, or NIL if the DPMI host would not."
  (let ((address (alien-funcall (sap-alien (code-address *map-physical-code*)
                                           (function unsigned unsigned unsigned))
                                physical size)))
    (unless (zerop address) address)))
(defun unmap-physical (address)
  "Take away a mapping that MAP-PHYSICAL made at ADDRESS (DPMI function 0801h)."
  (int86 #x31 :ax #x0801 :bx (ldb (byte 16 16) address) :cx (ldb (byte 16 0) address))
  (values))
(defmacro with-physical-memory ((address physical size) &body body)
  "Run BODY with ADDRESS bound to where SIZE bytes of device memory at the
physical address PHYSICAL have been mapped (MAP-PHYSICAL), or to NIL if the
DPMI host would not; the mapping is taken away afterwards."
  `(let ((,address (map-physical ,physical ,size)))
     (unwind-protect (progn ,@body)
       (when ,address (unmap-physical ,address)))))
