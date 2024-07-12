ACME = acme
PYTHON3 = python3

DISK_FILES = easyasm.prg \
	easyasm-e.prg \
	typeme.txt \
	autoboot.bas

%.prg: %.asm
	${ACME} -f cbm -o $@ -l $@.lst $<

.PHONY: all clean

all: easyasm.d81

clean:
	rm *.prg *.d81 *.lst

easyasm.d81: ${DISK_FILES} files.json makedisk.py
	${PYTHON3} makedisk.py files.json
