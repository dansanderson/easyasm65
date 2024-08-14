ACME = acme
PYTHON3 = python3

DISK_FILES = easyasm.prg \
	easyasm-e.prg \
	typeme.txt \
	autoboot.bas \
	testdata/border.txt \
	testdata/bordercycle.txt \
	testdata/everyopcode.txt \
	testdata/everyoperator.txt

TEST_SUITES = \
	test_suite_1.asm \
	test_suite_2.asm \
	test_suite_3.asm \
	test_suite_4.asm \
	test_suite_5.asm \
	test_suite_6.asm \
	test_suite_7.asm

%.prg: %.asm
	${ACME} -f cbm -o $@ -l $@.lst -r $@.rpt $<

.PHONY: all clean

all: easyasm.d81

clean:
	rm *.prg *.d81 *.lst

easyasm.prg: easyasm.asm test_common.asm ${TEST_SUITES}

easyasm.d81: ${DISK_FILES} files.json makedisk.py
	${PYTHON3} makedisk.py files.json
