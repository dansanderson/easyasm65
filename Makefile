ACME = acme
PYTHON3 = python3
TEST_SUITE ?= 0  # no tests by default

DISK_FILES = easyasm.prg \
	easyasm-e.prg \
	typeme.txt \
	autoboot.bas \
	examples/border.txt \
	examples/bordercycle.txt \
	examples/everyopcode.txt \
	examples/everyoperator.txt \
	examples/rfk.txt

TEST_SUITES = \
	test_suite_1.asm \
	test_suite_2.asm \
	test_suite_3.asm \
	test_suite_4.asm \
	test_suite_5.asm \
	test_suite_6.asm \
	test_suite_7.asm
NUM_OF_SUITES = 7

%.prg: %.asm
	${ACME} -DTEST_SUITE=${TEST_SUITE} -f cbm -o $@ -l $@.lst -r $@.rpt $<

.PHONY: all clean test all_tests

all: easyasm.d81

clean:
	rm *.prg *.d81 *.lst

easyasm.prg: easyasm.asm test_common.asm ${TEST_SUITES}

easyasm.d81: ${DISK_FILES} files.json makedisk.py
	${PYTHON3} makedisk.py files.json

test:
	@echo Would run test suite "${TEST_SUITE}"
	@echo ok done

# This rule depends on "mega65_ftp" and an Ethernet connection.
# Enable Ethernet file transfers on the MEGA65: Shift + Pound
all_tests:
	@for i in $$(seq 1 ${NUM_OF_SUITES}); do \
	    echo; \
		echo ===== Test suite $$i; \
		TEST_SUITE=$$i $(MAKE) -B easyasm.d81 && mega65_ftp -e -c 'put easyasm.d81' -c 'exit'; \
		read -p "Press Enter to continue... "; \
	done
