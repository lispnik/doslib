#!/usr/bin/env python3
"""Make the sample pictures for the "picview" example: examples/data/sample.pcx and sample.gif.

    python3 examples/support/make-pictures.py            the two samples
    python3 examples/support/make-pictures.py IN.BMP OUT.PCX OUT.GIF ...
                                                         an 8-bit BMP file as PCX and/or GIF

Nothing but Python's standard library.  The picture is drawn here, a pixel at a time, to make a
decoder's mistakes easy to see: a white frame one pixel wide at the very edge, four ramps of 64
shades, a chequer-board of single pixels, diagonal stripes through all 256 colours, a disc, and
the name of the format in big letters.  The GIF is interlaced; the PCX is not.

Part of DOSLIB: Common Lisp on DOS with SBCL.
"""
import os, struct, sys

W, H = 320, 200
FONT = {  # 5 by 7, a row a string
    'P': 'XXXX.X...XX...XXXXX.X....X....X....', 'C': '.XXX.X...XX....X....X....X...X.XXX.',
    'X': 'X...XX...X.X.X...X...X.X.X...XX...X', 'G': '.XXX.X...XX....X.XXXX...XX...X.XXXX',
    'I': 'XXXXX..X....X....X....X....X..XXXXX', 'F': 'XXXXXX....X....XXXX.X....X....X....',
    'D': 'XXXX.X...XX...XX...XX...XX...XXXXX.', 'O': '.XXX.X...XX...XX...XX...XX...X.XXX.',
    'S': '.XXXXX....X.....XXX.....X....XXXXX.', 'L': 'X....X....X....X....X....X....XXXXX',
    'B': 'XXXX.X...XX...XXXXX.X...XX...XXXXX.'}

def palette():
    """0-63 reds, 64-127 greens, 128-191 blues, 192-255 greys: 0 is black and 255 white."""
    p = bytearray()
    for i in range(256):
        v = (i % 64) * 255 // 63
        p += bytes([(v, 0, 0), (0, v, 0), (0, 0, v), (v, v, v)][i // 64])
    return bytes(p)

def draw(label):
    pix = bytearray(W * H)
    def put(x, y, c):
        if 0 <= x < W and 0 <= y < H: pix[x + W * y] = c
    def text(x, y, string, size, c):
        for n, ch in enumerate(string):
            for i, bit in enumerate(FONT[ch]):
                if bit == 'X':
                    for dy in range(size):
                        for dx in range(size):
                            put(x + (6 * n + i % 5) * size + dx, y + (i // 5) * size + dy, c)
    for y in range(176):                        # four ramps, dark at the left
        for x in range(W):
            pix[x + W * y] = 64 * (y // 44) + x * 64 // W
    for y in range(176, H):
        for x in range(W):                      # a chequer-board, then stripes of every colour
            pix[x + W * y] = (255 * ((x + y) & 1)) if x < 64 else (x + y) & 255
    for y in range(H):                          # a disc with a ring round it
        for x in range(W):
            d = (x - 250) ** 2 + (y - 80) ** 2
            if d < 40 * 40: pix[x + W * y] = 192 + (x - 210 + y - 40) * 63 // 160
            elif 46 * 46 <= d < 48 * 48: pix[x + W * y] = 255
    for i in range(160):                        # two diagonals across the ramps
        put(i * 2, i, 255); put(i * 2 + 1, i, 255); put(319 - i * 2, i, 0); put(318 - i * 2, i, 0)
    text(22, 22, label, 8, 0); text(20, 20, label, 8, 255)
    text(21, 101, 'DOSLIB', 3, 0); text(20, 100, 'DOSLIB', 3, 255)
    for x in range(W): pix[x] = pix[x + W * (H - 1)] = 255
    for y in range(H): pix[W * y] = pix[W * y + W - 1] = 255
    return bytes(pix)

def pcx(w, h, pix, pal):
    """Version 5, 8 bits, one plane, each row run-length encoded; the palette after the picture."""
    row_bytes = w + (w & 1)
    head = struct.pack('<BBBBHHHHHH48sBBHHHH54s', 10, 5, 1, 8, 0, 0, w - 1, h - 1, 72, 72,
                       bytes(48), 0, 1, row_bytes, 1, 0, 0, bytes(54))
    out = bytearray(head)
    for y in range(h):
        row = pix[w * y:w * y + w] + bytes(row_bytes - w)
        i = 0
        while i < row_bytes:
            n = 1
            while i + n < row_bytes and n < 63 and row[i + n] == row[i]: n += 1
            if n > 1 or row[i] >= 0xC0: out.append(0xC0 | n)
            out.append(row[i])
            i += n
    return bytes(out) + b'\x0c' + pal

def gif(w, h, pix, pal, interlace=False):
    """GIF87a: a global palette of 256, one image, LZW with codes of 9 to 12 bits."""
    if interlace:
        passes = ((0, 8), (4, 8), (2, 4), (1, 2))
        rows = [y for start, step in passes for y in range(start, h, step)]
        pix = b''.join(pix[w * y:w * y + w] for y in rows)
    clear, bits, acc, out = 256, 0, 0, bytearray()
    def emit(code, size):
        nonlocal bits, acc
        acc |= code << bits; bits += size
        while bits >= 8: out.append(acc & 255); acc >>= 8; bits -= 8
    table, size, following = {}, 9, clear + 2
    emit(clear, size)
    prefix = pix[0]
    for byte in pix[1:]:
        if (prefix, byte) in table:
            prefix = table[prefix, byte]
            continue
        emit(prefix, size)
        table[prefix, byte] = following; following += 1
        if following > (1 << size): size += 1
        if following == 4096:                   # the table is full: start again
            emit(clear, size); table, size, following = {}, 9, clear + 2
        prefix = byte
    emit(prefix, size)
    if following + 1 > (1 << size) and size < 12: size += 1     # as the reader will
    emit(clear + 1, size)
    if bits: out.append(acc & 255)
    data = b''.join(bytes([len(out[i:i + 255])]) + out[i:i + 255] for i in range(0, len(out), 255))
    return (b'GIF87a' + struct.pack('<HHBBB', w, h, 0xF7, 0, 0) + pal
            + b',' + struct.pack('<HHHHB', 0, 0, w, h, 0x40 if interlace else 0)
            + b'\x08' + data + b'\x00;')

def read_bmp(name):
    b = open(name, 'rb').read()
    start, = struct.unpack_from('<I', b, 10)
    size, w, h, planes, depth, compression = struct.unpack_from('<IiiHHI', b, 14)
    assert b[:2] == b'BM' and depth == 8 and compression == 0 and h > 0, 'not a plain 8-bit BMP'
    pal = bytearray(768)
    for i in range((start - 14 - size) // 4):
        pal[3 * i:3 * i + 3] = b[14 + size + 4 * i:14 + size + 4 * i + 3][::-1]
    stride = (w + 3) & ~3
    pix = b''.join(b[start + stride * y:start + stride * y + w] for y in range(h - 1, -1, -1))
    return w, h, pix, bytes(pal)

if __name__ == '__main__':
    if len(sys.argv) > 2:
        picture = read_bmp(sys.argv[1])
        for name in sys.argv[2:]:
            open(name, 'wb').write(pcx(*picture) if name.lower().endswith('.pcx')
                                   else gif(*picture, interlace='lace' in name.lower()))
    else:
        data = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'data')
        open(os.path.join(data, 'sample.pcx'), 'wb').write(pcx(W, H, draw('PCX'), palette()))
        open(os.path.join(data, 'sample.gif'), 'wb').write(
            gif(W, H, draw('GIF'), palette(), interlace=True))
