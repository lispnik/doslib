#!/usr/bin/env python3
"""Write a small ProTracker module from scratch: examples/data/song.mod.

    python3 examples/support/make-mod.py                 write examples/data/song.mod
    python3 examples/support/make-mod.py FILE.MOD        write it somewhere else
    python3 examples/support/make-mod.py --describe [FILE.MOD]
    python3 examples/support/make-mod.py --at SAMPLES [--rate 22050] [FILE.MOD]

The "modplay" example plays module files, and no module that somebody else
wrote can be shipped with DOSLIB, so this makes one: six instruments
computed here (two looped single waves, a pad whose loop starts part way in
and which is tuned a little sharp, three drums) and five patterns that use
every effect the player knows -- arpeggio (0), slides (1, 2, 3), vibrato (4),
3 and 4 with a volume slide (5, 6), sample offset (9), volume slide (A),
position jump (B), volume (C), pattern break (D), speed and tempo (F).

The file is a plain "M.K." module, as ProTracker on the Amiga wrote them:

    0     20 bytes   the title
    20    31 times 30 bytes, one for each sample:
              22 bytes   its name
              2          its length in words (two bytes), big end first
              1          finetune, 0 to 15 (8 to 15 mean -8 to -1 eighths of a semitone)
              1          volume, 0 to 64
              2          where its loop starts, in words
              2          how long the loop is, in words (1: no loop)
    950   1 byte     the song's length: how many of the orders are used
    951   1 byte     127 (once a restart position)
    952   128 bytes  the orders: which pattern to play first, second, ...
    1080  4 bytes    "M.K."
    1084  the patterns, as many as the highest number among the orders, plus one:
          64 rows of 4 channels of 4 bytes  SSSSPPPP PPPPPPPP SSSSEEEE XXXXXXXX
          (S sample number, upper and lower half; P period; E effect; X its parameter)
    then  the samples' data one after another, signed bytes

--describe reads a module (this one unless another is named) with a parser
written here, independently of the Lisp one, and prints what it finds in the
same words as
    (doslib-examples.modplay:describe-mod "SONG.MOD")       or   DEMO MODPLAY SONG.MOD INFO
does on DOS: the two outputs should be the same to the letter.  That
includes a checksum of each sample's data and of the patterns, the first rows,
and how long the song takes to play through once, which needs the speed,
tempo, jump and break effects followed the same way on both sides.

--at SAMPLES says which row the player is mixing when it has made that many
samples of sound (what "modplay" prints when it ends), to check that it
moves through the song at the right speed.

Part of DOSLIB: Common Lisp on DOS with SBCL.
"""

import math
import os
import struct
import sys
from fractions import Fraction

PERIODS = [856, 808, 762, 720, 678, 640, 604, 570, 538, 508, 480, 453,   # C-1 to B-1
           428, 404, 381, 360, 339, 320, 302, 285, 269, 254, 240, 226,   # C-2 to B-2
           214, 202, 190, 180, 170, 160, 151, 143, 135, 127, 120, 113]   # C-3 to B-3
NAMES = ['C-', 'C#', 'D-', 'D#', 'E-', 'F-', 'F#', 'G-', 'G#', 'A-', 'A#', 'B-']


def period(note):
    """The period of a note written like 'C-2' or 'F#1'."""
    return PERIODS[12 * (int(note[2]) - 1) + NAMES.index(note[:2])]


# ---- the instruments: (name, volume, finetune, loop start, loop length, data)

def clip(value):
    return max(-128, min(127, int(round(value))))


def instruments():
    seed = [20261]

    def noise():
        seed[0] = (seed[0] * 1103515245 + 12345) & 0x7FFFFFFF
        return ((seed[0] >> 16) & 255) - 128

    def make(length, function):
        return [clip(function(i)) for i in range(length)]

    # a pulse wave, a quarter of it high: one wave, played round and round
    lead = make(32, lambda i: 96 if i < 8 else -32)
    # a saw with its corner rounded, for the bass
    bass = make(64, lambda i: 110 * (1 - i / 32.0) if i else 0)
    # a pad: 512 samples swelling in, then eight sine waves of 64 with a
    # slow second partial, which is the loop -- so the loop does not start at 0
    pad = make(1024, lambda i: min(1.0, i / 512.0) * (80 * math.sin(2 * math.pi * i / 64)
                                                      + 30 * math.sin(2 * math.pi * i / 32)))
    # the drums: a sine falling in pitch, and noise dying away slowly and fast
    kick = make(1800, lambda i: 125 * math.exp(-i / 450.0)
                * math.sin(55 * (1 - math.exp(-i / 320.0))))
    snare = make(2600, lambda i: math.exp(-i / 520.0) * (0.75 * noise()
                                                         + 45 * math.sin(i / 5.5)))
    hat = make(700, lambda i: noise() * math.exp(-i / 110.0))
    return [('pulse lead', 44, 0, 0, 32, lead),
            ('round saw bass', 64, 0, 0, 64, bass),
            ('sine pad (sharp)', 36, 3, 512, 512, pad),
            ('kick', 64, 0, 0, 0, kick),
            ('snare (flat)', 52, 14, 0, 0, snare),
            ('hat', 36, 0, 0, 0, hat)]


LEAD, BASS, PAD, KICK, SNARE, HAT = 1, 2, 3, 4, 5, 6

# ---- the patterns: {(row, channel): (note, sample, effect, parameter)}

CHORDS = [('A-1', 'A-2', 0x37), ('F-1', 'F-2', 0x47), ('C-2', 'C-3', 0x47), ('G-1', 'G-2', 0x47)]


def patterns():
    result = [dict() for _ in range(5)]

    def cell(pattern, row, channel, note=None, sample=0, effect=0, parameter=0):
        assert (row, channel) not in result[pattern], (pattern, row, channel)
        result[pattern][(row, channel)] = (note, sample, effect, parameter)

    def drums(pattern, rows, first=None):
        """Channel 0: kick and snare by turns; the first row may carry an effect."""
        for row in rows:
            effect, parameter = first if (first and row == rows[0]) else (0, 0)
            cell(pattern, row, 0, 'C-2', SNARE if row % 16 == 8 else KICK, effect, parameter)

    def bass(pattern, bars=4):
        """Channel 2: a bar to each chord, every note fading (A)."""
        for bar in range(bars):
            root = PERIODS.index(period(CHORDS[bar][0]))
            for row, up in ((0, 0), (4, 0), (6, 12), (8, 0), (12, 0), (14, 7)):
                index = root + up
                cell(pattern, 16 * bar + row, 2, NAMES[index % 12] + str(index // 12 + 1), BASS)
                cell(pattern, 16 * bar + row + 1, 2, None, 0, 0xA, 0x06)

    # pattern 0: speed 6 (F06) and tempo 125 (F7D); drums, bass, chords as arpeggios (0)
    drums(0, list(range(0, 64, 8)), (0xF, 6))
    for row in range(0, 64, 4):
        if row == 0:
            cell(0, row, 1, 'C-3', HAT, 0xF, 125)
        else:
            cell(0, row, 1, 'C-3', HAT, 0xC, 40 if row % 8 == 4 else 20)     # volume (C)
    bass(0)
    for bar, (_, note, arpeggio) in enumerate(CHORDS):
        cell(0, 16 * bar, 3, note, LEAD, 0, arpeggio)
        for row in range(1, 16):
            cell(0, 16 * bar + row, 3, None, 0, 0, arpeggio)

    # pattern 1: a tune over the same, with the slides and the vibrato
    drums(1, list(range(0, 64, 8)))
    for row in range(2, 64, 4):
        cell(1, row, 1, 'C-3', HAT, 0x9, 1)                 # from 256 samples in (9)
    bass(1)
    tune = [(0, 'E-3', 0xC, 48), (4, 'D-3'), (6, 'C-3'), (8, 'A-2'),
            (10, None, 0xA, 0x04), (11, None, 0xA, 0x04),
            (12, 'C-3', 0xC, 40), (14, None, 0x1, 8), (15, None, 0x1, 8),      # slide up
            (16, 'C-3', 0xC, 48), (20, 'A-2'), (22, 'F-2'), (24, 'A-2'),
            (26, 'C-3', 0x3, 12), (27, None, 0x3, 0), (28, None, 0x3, 0),      # slide to C-3
            (29, None, 0x4, 0x46), (30, None, 0x4, 0), (31, None, 0x6, 0x02),  # vibrato
            (32, 'G-3', 0xC, 48), (36, 'E-3'), (38, 'C-3'), (40, 'E-3'),
            (42, None, 0x2, 6), (43, None, 0x2, 6),                            # slide down
            (44, 'D-3'), (46, None, 0xA, 0x08),
            (48, 'D-3', 0xC, 48), (50, 'B-2'), (52, 'G-2'), (54, 'B-2'),
            (56, 'D-3', 0xC, 40), (58, 'G-3', 0x3, 16), (59, None, 0x5, 0x03),
            (60, None, 0x5, 0x03), (61, None, 0xA, 0x03), (62, None, 0xA, 0x03),
            (63, None, 0xA, 0x03)]
    for row, note, *effect in tune:
        cell(1, row, 3, note, LEAD if note else 0, *(effect or (0, 0)))

    # pattern 2: faster (speed 4, tempo 150); 32 rows of drums and bass, then a
    # break (D16) to row 16 of whatever comes next
    drums(2, list(range(0, 32, 4)), (0xF, 4))
    for row in range(0, 32, 2):
        if row == 0:
            cell(2, row, 1, 'C-3', HAT, 0xF, 150)
        else:
            cell(2, row, 1, 'C-3', HAT, 0xC, 36 if row % 4 == 2 else 16)
    bass(2, 2)
    cell(2, 31, 3, None, 0, 0xD, 0x16)

    # pattern 3: back to speed 6 and tempo 125; the pad (looped from half way,
    # and sharp by three eighths of a semitone) under a slow tune, swelling and fading
    drums(3, list(range(0, 64, 16)), (0xF, 6))
    cell(3, 0, 1, 'A-2', PAD, 0xF, 125)
    for row, note in ((16, 'F-2'), (32, 'C-3'), (48, 'G-2')):
        cell(3, row, 1, note, PAD, 0xC, 8)
    for bar in range(4):
        for row in range(1, 8):
            cell(3, 16 * bar + row, 1, None, 0, 0xA, 0x40)      # louder
        for row in range(12, 16):
            cell(3, 16 * bar + row, 1, None, 0, 0xA, 0x05)      # softer
    bass(3)
    for row, note in ((0, 'E-3'), (8, 'C-3'), (16, 'A-2'), (24, 'C-3'),
                      (32, 'E-3'), (40, 'G-3'), (48, 'D-3'), (56, 'B-2')):
        cell(3, row, 3, note, LEAD, 0xC, 36)
        for more in range(2, 8):
            cell(3, row + more, 3, None, 0, 0x4, 0x35 if more == 2 else 0)

    # pattern 4: sixteen rows at speed 3, a roll getting louder and the bass
    # sliding up; then a jump (B01) back to the second order
    for row in range(16):
        cell(4, row, 0, 'C-2', SNARE, 0xC, 16 + 3 * row)
        if row == 0:
            cell(4, row, 2, 'A-1', BASS, 0xF, 3)
        else:
            cell(4, row, 2, None, 0, 0x1, 12)
    cell(4, 15, 1, None, 0, 0xB, 1)
    return result


ORDERS = [0, 1, 2, 1, 3, 4]
TITLE = 'doslib demo song'


def build():
    """The module, as bytes."""
    header = bytearray(1084)
    header[0:len(TITLE)] = TITLE.encode('ascii')
    data = bytearray()
    made = instruments()
    for number in range(31):
        at = 20 + 30 * number
        struct.pack_into('>H', header, at + 28, 1)              # "no loop" is a loop of one word
        if number < len(made):
            name, volume, finetune, loop_start, loop_length, samples = made[number]
            assert len(samples) % 2 == 0 and len(name) <= 22
            header[at:at + len(name)] = name.encode('ascii')
            struct.pack_into('>HBBH', header, at + 22, len(samples) // 2, finetune, volume,
                             loop_start // 2)
            if loop_length:
                struct.pack_into('>H', header, at + 28, loop_length // 2)
            data += bytes(value & 255 for value in samples)
    header[950] = len(ORDERS)
    header[951] = 127
    header[952:952 + len(ORDERS)] = bytes(ORDERS)
    header[1080:1084] = b'M.K.'
    cells = bytearray()
    for pattern in patterns():
        block = bytearray(1024)
        for (row, channel), (note, sample, effect, parameter) in pattern.items():
            p = period(note) if note else 0
            block[16 * row + 4 * channel:16 * row + 4 * channel + 4] = bytes(
                [(sample & 0xF0) | (p >> 8), p & 255, ((sample & 15) << 4) | effect, parameter])
        cells += block
    return bytes(header + cells + data)


# ---- reading one back: a parser of its own, to compare the Lisp one with

def text(raw):
    raw = raw.split(b'\0')[0]
    return ''.join(chr(c) if 31 < c < 127 else ' ' for c in raw).rstrip(' ')


def parse(raw):
    if len(raw) < 1084 or raw[1080:1084] not in (b'M.K.', b'M!K!', b'FLT4', b'4CHN'):
        sys.exit('not a four-channel ProTracker module (no M.K. at byte 1080)')
    length = max(1, min(128, raw[950]))
    count = max(raw[952:1080]) + 1
    at = min(len(raw), 1084 + 1024 * count)
    samples = []
    for number in range(31):
        name = raw[20 + 30 * number:42 + 30 * number]
        words, finetune, volume, start, loop = struct.unpack_from('>HBBHH', raw, 42 + 30 * number)
        size = min(2 * words, len(raw) - at)
        start, loop = 2 * start, 2 * loop
        if loop <= 2 or start >= size:
            loop = 0
        loop = min(loop, size - start)
        samples.append((text(name), raw[at:at + size], finetune & 15, min(64, volume), start, loop))
        at += size
    cells = raw[1084:1084 + 1024 * count].ljust(1024 * count, b'\0')
    return text(raw[:20]), list(raw[952:952 + length]), count, cells, samples


def cell_at(cells, pattern, row, channel):
    b0, b1, b2, b3 = cells[1024 * pattern + 16 * row + 4 * channel:][:4]
    return ((b0 & 15) << 8) | b1, (b0 & 0xF0) | (b2 >> 4), b2 & 15, b3


def walk(orders, cells, rate=None, stop=None):
    """Go through the song as a player does, a tick at a time, following F, B
    and D.  Without STOP: until the song goes back (a jump to an order not after
    this one, or the end of the orders); returns rows, ticks and the time.
    With STOP, a number of samples at RATE: returns the order and row being
    played when that many have been made, and the rows begun by then."""
    speed, tempo, order, row = 6, 125, 0, 0
    rows = ticks = samples = remainder = 0
    time = Fraction(0)
    while True:
        next_order, next_row = -1, 0
        for channel in range(4):                    # the first tick of a row
            _, _, effect, x = cell_at(cells, orders[order], row, channel)
            if effect == 0xB:
                next_order, next_row = x, 0
            elif effect == 0xD:
                if next_order < 0:
                    next_order = order + 1
                next_row = 10 * (x >> 4) + (x & 15)
                if next_row > 63:
                    next_row = 0
            elif effect == 0xF and x:
                if x < 32:
                    speed = x
                else:
                    tempo = x
        rows += 1
        for _ in range(speed):
            ticks += 1
            time += Fraction(5, 2 * tempo)
            if stop is not None:
                total = remainder + 5 * rate
                samples += max(1, total // (2 * tempo))
                remainder = total % (2 * tempo)
                if samples >= stop:
                    return order, row, rows
        back = False
        if next_order >= 0:
            back = next_order <= order
            order, row = next_order, next_row
        else:
            row += 1
            if row >= 64:
                order, row = order + 1, 0
        if order >= len(orders):
            order, back = 0, True
        if back and stop is None:
            return rows, ticks, time


def checksum(data):
    return sum((i + 1) * byte for i, byte in enumerate(data)) % 65521


def describe(raw):
    title, orders, count, cells, samples = parse(raw)
    print('Title "%s", %d bytes' % (title, len(raw)))
    print('%d orders: %s' % (len(orders), ' '.join(map(str, orders))))
    print('%d patterns, checksum %d' % (count, checksum(cells)))
    for number, (name, data, finetune, volume, start, loop) in enumerate(samples, 1):
        if data:
            print('Sample %2d "%s": %d bytes, tune %d, volume %d, loop %d+%d, sum %d'
                  % (number, name, len(data), finetune, volume, start, loop, checksum(data)))
    for pattern in range(min(3, count)):
        for row in range(4):
            print('Pattern %d row %d:' % (pattern, row)
                  + ' |'.join(' %3d %02d %X%02X' % cell_at(cells, pattern, row, channel)
                              for channel in range(4)))
    rows, ticks, time = walk(orders, cells)
    print('Once through: %d rows, %d ticks, %d ms' % (rows, ticks, int(time * 1000)))


def main(arguments):
    rate, at, mode, files = 22050, None, 'write', []
    while arguments:
        argument = arguments.pop(0)
        if argument == '--describe':
            mode = 'describe'
        elif argument == '--at':
            mode, at = 'at', int(arguments.pop(0))
        elif argument == '--rate':
            rate = int(arguments.pop(0))
        else:
            files.append(argument)
    file = files[0] if files else os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                               '..', 'data', 'song.mod')
    if mode == 'write':
        raw = build()
        with open(file, 'wb') as out:
            out.write(raw)
        print('%s: %d bytes' % (os.path.normpath(file), len(raw)))
        return
    with open(file, 'rb') as stream:
        raw = stream.read()
    if mode == 'describe':
        describe(raw)
    else:
        _, orders, _, cells, _ = parse(raw)
        order, row, rows = walk(orders, cells, rate, at)
        print('%d samples at %d a second: order %d (pattern %d), row %d; %d rows begun'
              % (at, rate, order, orders[order], row, rows))


if __name__ == '__main__':
    main(sys.argv[1:])
