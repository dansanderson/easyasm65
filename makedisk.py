#!/usr/bin/env python3
"""
Creates a disk image from a configuration file and a set of data files.

Usage:
  python3 makedisk.py files.json

To install dependencies:
  python3 -m pip install d64

The JSON configuration file has the following fields, all required:

* disk_filename : The filename of the disk image to create
* disk_name : The CBDOS disk name
* disk_id : The CBDOS disk ID (two characters)
* files : The list of files, in the order they appear in the disk directory
* text_style : Processes all .txt files in one of several ways. (See below.)

Each file can have the following fields:

* name : The CBDOS file name
* file_path : The path to the data file
* text_style : Processes this .txt file in one of several ways. (See below.)
*    Overrides global setting.

Disk and file names are converted to uppercase PETSCII.

If file_path is provided, name is optional. If name is omitted, it is derived
from the file_path, as the base filename without the filename extension.

The file type (PRG, SEQ) and data are derived from the filename extension and
file contents, as follows:

* .prg: Data is the file contents verbatim. Type is PRG.
* .seq: Data is the file contents verbatim. Type is SEQ.
* .txt: File is ASCII, is converted to PETSCII. Type is SEQ.
* .bas: File is a petcat BASIC 65 listing, is converted to PRG via petcat. Type
  is PRG.

To specify an empty file (such as a directory entry with no data), omit
file_path. The name is required in this case. Only in this case, the name does
not need to be unique across all names: a new file record is created. This
allows empty files to decorate directory listings, such as for dividers.

"text_style" is an optional property that describes how text files are
processed. As a global property, it applies to all text files. As a property of
a file, it overrides the global setting. The default text style is "plain",
which converts the text to PETSCII and makes no other changes.

The text styles "topic" and "topic-single" process the text file using a
lightweight markup language (intended as a very small subset of Markdown):

* The file begins with clear-screen and lowercase codes.
* Lines indented with two spaces are colored like BASIC code.
* Inline text surrounded by backticks is colored like BASIC code.
* Inline text surrounded by single underscores is underlined.
* The file is preceded with CLR (clear screen) and SWLC (switch to lowercase)
  codes.
* Each line is validated to not exceed 80 columns after formatting is applied.
  The file is expected to be hand-wrapped for greater layout control.

In addition, the "topic-single" style validates that the resulting text file
fits in 22 rows, such that it can be TYPE'd.

TODO:
* Markup: tables? auto-wrap paragraphs? headings?
"""

import codecs
import d64  # type: ignore
import json
import pathlib
import os
import shutil
import subprocess
import sys
import tempfile


VALID_TEXT_STYLES = ('plain', 'topic', 'topic-single')

#    92  PETSCII: pound              ASCII: \
#    94  PETSCII: up arrow           ASCII: ^
#    95  PETSCII: back arrow         ASCII: _
#    96  PETSCII: graphic horiz bar  ASCII: `
#   123  PETSCII: graphic plus       ASCII: {
#   124  PETSCII: half checker       ASCII: |
#   125  PETSCII: graphic vert bar   ASCII: }
#   126  PETSCII: pi                 ASCII: ~
_petscii_encode_table = dict(
    (str(bytes([c]), encoding='iso-8859-1'), bytes([c])) for c in range(128))
# Flip letter casing
for c in range(65, 91):
    luc = c + 97-65  # lower upper case
    uuc = c + 193-65  # upper upper case
    _petscii_encode_table[str(bytes([c]), encoding='iso-8859-1')] = \
        bytes([uuc])
    _petscii_encode_table[str(bytes([luc]), encoding='iso-8859-1')] = \
        bytes([c])
    _petscii_encode_table[str(bytes([uuc]), encoding='iso-8859-1')] = \
        bytes([c])
# Swap LF with CR
_petscii_encode_table[str(bytes([10]), encoding='iso-8859-1')] = bytes([13])
_petscii_decode_table = dict(
    (v, k) for (k, v) in _petscii_encode_table.items())


def petscii_encode(text):
    return b''.join(_petscii_encode_table[x] for x in text), len(text)


def petscii_decode(text):
    return ''.join(_petscii_decode_table[x] for x in text), len(text)


def fail(msg):
    sys.stderr.write(msg + '\n')
    sys.exit(1)


def ascii_to_petscii(data, spec, fdata):
    data_asc = data.decode('ascii')

    text_style = spec.get('text_style')
    text_style_global = (
        fdata.text_style if hasattr(fdata, 'text_style') else None)
    if text_style is None and text_style_global is not None:
        text_style = text_style_global
    if text_style is None:
        text_style = 'plain'

    if text_style == 'topic-single' or text_style == 'topic':
        data_asc = data_asc.strip()

        # TODO:
        # - convert common non-ASCII to ASCII before converting to PETSCII
        #     (smart quotes, m-dashes; stuff I tend to use)
        # - validate line length, ignoring formatting chars
        # - if topic-single, validate screen height
        # - colorize indented code samples
        # - recognize reverse, underline, blink, other coloring?
        # - style title

        data_bytes = b'\x93\x0e' + data_asc.encode('petscii')

    elif text_style == 'plain':
        data_bytes = data_asc.encode('petscii')

    else:
        fail(f'Unrecognized text_style: {text_style}')

    return data_bytes


def petcat_to_prg(data, spec, fdata):
    petcat_pth = shutil.which('petcat')
    if not petcat_pth:
        fail('Could not find petcat, required to convert .bas files')
    fh, baspath = tempfile.mkstemp(suffix='.bas')
    with open(baspath, 'wb') as outfh:
        outfh.write(data)
    fh, prgpath = tempfile.mkstemp(suffix='.prg')
    subprocess.run(
        [petcat_pth, '-w65', '-o', prgpath, '--', baspath], check=True)
    with open(prgpath, 'rb') as fh:
        prgdata = fh.read()
    os.remove(baspath)
    os.remove(prgpath)
    return prgdata


FILE_FORMATS = {
    '.prg': {'type': 'PRG'},
    '.seq': {'type': 'SEQ'},
    '.txt': {'type': 'SEQ', 'converter': ascii_to_petscii},
    '.bas': {'type': 'PRG', 'converter': petcat_to_prg},
}


class FileData:
    def __init__(
            self, disk_filename, disk_name_pt, disk_id_pt, text_style,
            files):
        self.disk_filename = disk_filename
        self.disk_name_pt = disk_name_pt
        self.disk_id_pt = disk_id_pt
        self.text_style = text_style
        self.files = files

    @classmethod
    def from_file(cls, filename):
        if not os.path.exists(filename):
            fail(f'{filename}: File does not exist')
        if not os.path.isfile(filename):
            fail(f'{filename}: Is not a file')
        with open(filename) as fh:
            try:
                data = json.load(fh)
            except json.JSONDecodeError as e:
                fail(f'{filename}: Invalid JSON: {e}')

        def get_or_fail(field):
            if field not in data:
                fail(f'{filename}: Missing field: {field}')
            return data[field]

        files = get_or_fail('files')

        for i, fspec in enumerate(files):
            if 'name' not in fspec and 'file_path' not in fspec:
                fail(
                    f'{filename}: File {i} must have either '
                    f'"name" or "file_path"')
            if 'file_path' in fspec and not any(
                    fspec['file_path'].endswith(ext)
                    for ext in FILE_FORMATS):
                fpath = fspec['file_path']
                fail(f'{filename}: File type of {fpath} not recognized')
            if (
                    'text_style' in fspec and
                    fspec['text_style'] not in VALID_TEXT_STYLES):
                fail(
                    f'{filename}: {fpath}: text_style must be one of '
                    f'{VALID_TEXT_STYLES}')

        text_style = data.get('text_style')
        if text_style is not None and text_style not in VALID_TEXT_STYLES:
            fail(f'{filename}: text_style must be one of {VALID_TEXT_STYLES}')

        return cls(
            disk_filename=get_or_fail('disk_filename'),
            disk_name_pt=get_or_fail('disk_name').encode('petscii'),
            disk_id_pt=get_or_fail('disk_id').encode('petscii'),
            text_style=data.get('text_style'),
            files=files)


def main(args):
    if len(args) != 1:
        fail('Usage: makedisk.py files.json')

    codecs.register(
        lambda x: codecs.CodecInfo(
            petscii_encode, petscii_decode, name='petscii'))

    fdata = FileData.from_file(args[0])
    d64.DiskImage.create(
        type_name='d81',
        filepath=pathlib.Path(fdata.disk_filename),
        disk_name=fdata.disk_name_pt,
        disk_id=fdata.disk_id_pt)
    with d64.DiskImage(
            filepath=pathlib.Path(fdata.disk_filename), mode='w') as disk:
        for fspec in fdata.files:
            basename, ext = os.path.splitext(
                os.path.basename(fspec.get('file_path', '')))

            name = fspec.get('name')
            if name is None:
                name = basename
            name_pt = name.encode('petscii')

            ftype = 'SEQ'
            data = b''
            if ext:
                ftype = FILE_FORMATS[ext]['type']
                with open(fspec['file_path'], 'rb') as content_fh:
                    data = content_fh.read()
                    if 'converter' in FILE_FORMATS[ext]:
                        data = FILE_FORMATS[ext]['converter'](
                            data, fspec, fdata)

            if 'file_path' in fspec:
                # Find the path on the disk to avoid creating a duplicate.
                dos_path = disk.path(name_pt)
            else:
                # Use the DOSPath constructor directly to allow for duplicate
                # names on the disk.
                dos_path = d64.DOSPath(disk, name=name_pt)
            with dos_path.open(mode='w', ftype=ftype) as fh:
                fh.write(data)

    with d64.DiskImage(
            filepath=pathlib.Path(fdata.disk_filename), mode='r') as disk:
        for line in disk.directory():
            print(line)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
