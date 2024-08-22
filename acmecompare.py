#!/usr/bin/env python3
'''
Usage:
    python3 acmecompare.py <sourcefile> <easyasm-made-prg>
'''

import os
import re
import subprocess
import sys
import tempfile


RE_SRC_LINE = re.compile(r'(.{6})(.{6})(.{20})(.*)')
FIRST_BYTE_POS = 21


def process_rpt(rpt_txt):
    rpt_lines = rpt_txt.split('\n')
    line_data_triples = []
    for line in rpt_lines:
        m = RE_SRC_LINE.match(line)
        line_data = (None, None, None, line)
        if m:
            line_data = (
                m.group(1).strip(),
                m.group(2).strip(),
                m.group(3).strip(),
                m.group(4).strip())
        line_data_triples.append(line_data)
    return line_data_triples


def report_differences(acme_dat, ea_dat, rpt_triples):
    # This assumes both EasyAsm and Acme are compiling a "runnable" with a
    # BASIC preamble. The Acme report truncates the data part of lines with a
    # lot of data, so skip the preamble manually.

    # Match BASIC preamble without source file.
    if len(acme_dat) <= FIRST_BYTE_POS:
        print(f'Expected Acme to assemble at least {FIRST_BYTE_POS} bytes')
        return
    if len(ea_dat) <= FIRST_BYTE_POS:
        print(f'Expected EasyAsm to assemble at least {FIRST_BYTE_POS} bytes')
        return
    if acme_dat[:FIRST_BYTE_POS] != ea_dat[:FIRST_BYTE_POS]:
        print(f'First {FIRST_BYTE_POS} bytes differ, aborting')
        print('Acme: ' + ' '.join(f'{x:02x}'
                                  for x in acme_dat[:FIRST_BYTE_POS]))
        print('Easy: ' + ' '.join(f'{x:02x}'
                                  for x in ea_dat[:FIRST_BYTE_POS]))
        print('      ' + ' '.join(
            ('^^' if x != y else '  ')
            for (x, y) in zip(
                acme_dat[:FIRST_BYTE_POS], ea_dat[:FIRST_BYTE_POS])))
        return

    # Locate first source file line with address $2014.
    line_i = 0
    while (line_i < len(rpt_triples) and rpt_triples[line_i][1] != '2014'):
        line_i += 1
    line_dat_pos = 0
    acme_pos = FIRST_BYTE_POS
    ea_pos = FIRST_BYTE_POS
    show_line = True
    while (
            line_i < len(rpt_triples) and
            acme_pos < len(acme_dat) and
            ea_pos < len(ea_dat)):
        if show_line and rpt_triples[line_i][2].endswith('...'):
            print(f'## source out of sync starting line {rpt_triples[line_i][0]}')
            show_line = False
        if acme_dat[acme_pos] != ea_dat[ea_pos]:
            print(
                f'Pos:{acme_pos:5d} Acme:{acme_dat[acme_pos]:02x} '
                f'EA:{ea_dat[ea_pos]:02x} ', end='')
            if show_line:
                print(
                    f'Line:{rpt_triples[line_i][0]:5s} '
                    f'PC:{rpt_triples[line_i][1]} '
                    f'"{rpt_triples[line_i][2]}" '
                    f'{rpt_triples[line_i][3]}')
                #print(f'## src byte: "{rpt_triples[line_i][2][line_dat_pos:line_dat_pos+2]}"')
            else:
                print()
        elif show_line:
            assert f'{acme_dat[acme_pos]:02x}' == rpt_triples[line_i][2][line_dat_pos:line_dat_pos+2].lower()

        acme_pos += 1
        ea_pos += 1
        line_dat_pos += 2
        if line_dat_pos >= len(rpt_triples[line_i][2]):
            line_dat_pos = 0
            line_i += 1
            while line_i < len(rpt_triples) and not rpt_triples[line_i][2]:
                line_i += 1


def main(args):
    if len(args) != 2:
        print('Usage: python3 acmecompare.py <sourcefile> <easyasm-made-prg>')
        return 1
    source_fname = args[0]
    ea_prg_fname = args[1]
    if not os.path.isfile(source_fname):
        print(f'{source_fname} does not exist')
        return 1
    if not os.path.isfile(ea_prg_fname):
        print(f'{ea_prg_fname} does not exist')
        return 1
    tdir = tempfile.gettempdir()
    acme_prg_fname = os.path.join(tdir, 't-acme.prg')
    acme_rpt_fname = os.path.join(tdir, 't-acme.rpt')
    subprocess.run([
        'acme', '-o', acme_prg_fname, '-r', acme_rpt_fname,
        '-f', 'cbm', source_fname],
        check=True)
    acme_dat = open(acme_prg_fname, 'rb').read()
    ea_dat = open(ea_prg_fname, 'rb').read()
    rpt_txt = open(acme_rpt_fname).read()
    rpt_triples = process_rpt(rpt_txt)
    report_differences(acme_dat, ea_dat, rpt_triples)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
