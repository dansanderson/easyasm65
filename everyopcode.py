#!/usr/bin/env python3

import os
import re

DOC_DIR = '/Applications/Acme/docs/cputypes'
DOC_FILES = ('cpu 65ce02.txt', 'cpu m65.txt')

PREAMBLE = '''
!cpu m65
!to "everyopcode.prg",cbm
*=$1600

imm8 = 7
imm16 = 257
zp = $fc
abs16 = $d020
o8 = 13

'''


# 24                  25 andq zp          26 rolq zp          27
# 00  brk             01  ora (zp, x)     02+ cle             03+ see
RE_INSTR_LINE = re.compile(r'[0-9a-f][0-9a-f].(.{17})[0-9a-f][0-9a-f].(.{17})[0-9a-f][0-9a-f].(.{17})[0-9a-f][0-9a-f](.(.+))?')


def get_instructions_from_file(fname):
    instructions = []
    with open(fname) as infh:
        for line in infh:
            m = RE_INSTR_LINE.match(line)
            if m is None:
                continue
            instructions.extend([m.group(1), m.group(2), m.group(3)])
            if m.group(5):
                instructions.append(m.group(5))
    return [i.strip() for i in instructions if i.strip()]


next_id_num = 0
with open('everyopcode.s', 'w') as outfh:
    instrs = []
    for fname in DOC_FILES:
        instrs.extend(get_instructions_from_file(os.path.join(DOC_DIR, fname)))
    instrs.sort()

    outfh.write(PREAMBLE)
    for instr in instrs:
        if instr == 'nop':
            # Acme will error on NOP for M65
            continue
        if 'rel8' in instr:
            unique_id = 'l' + str(next_id_num)
            outfh.write(unique_id)
            outfh.write(':\n')
            next_id_num += 1
            instr = instr.replace('rel8', unique_id)
        if 'rel16' in instr:
            unique_id = 'l' + str(next_id_num)
            outfh.write(unique_id)
            outfh.write(':\n')
            next_id_num += 1
            instr = instr.replace('rel16', unique_id)
            if not instr.startswith('bsr'):
                # bsr not lbsr
                outfh.write('l')  # lbra, not bra, correct?
        # Acme supports both "sta (o8, s), y" and "sta (o8, sp), y"
        # EasyAsm supports the latter
        instr = instr.replace(', s)', ', sp)')

        outfh.write(instr)
        outfh.write('\n')
