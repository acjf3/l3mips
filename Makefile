#######################################
# Makefile for the L3 mips simulator ##
#######################################

# generating the L3 source list
# /!\ inclusion order matters /!\
#######################################
L3SRCDIR=src/l3
L3SRCBASE+=mips-print.spec
ifdef CAP
L3SRCBASE+=cheri/tlb-types.spec
L3SRCBASE+=mips-types.spec
L3SRCBASE+=mips-log.spec
L3SRCBASE+=mips-base.spec
L3SRCBASE+=mips-pic.spec
L3SRCBASE+=mips-uart.spec
ifeq ($(CAP), 128)
L3SRCBASE+=cheri/cap128-types.spec
else ifeq ($(CAP), c128)
L3SRCBASE+=cheri/cap128-compressed-types.spec
else
L3SRCBASE+=cheri/cap256-types.spec
endif
L3SRCBASE+=cheri/state.spec
L3SRCBASE+=cheri/exception.spec
L3SRCBASE+=tlb/base.spec
L3SRCBASE+=cheri/tlb-translate.spec
L3SRCBASE+=tlb/instructions.spec
L3SRCBASE+=mips-encode-utils.spec
ifdef CACHE
L3SRCBASE+=cheri/memory-caches.spec
else
L3SRCBASE+=cheri/memory.spec
endif
L3SRCBASE+=cheri/memory-sml-helpers.spec
L3SRCBASE+=cheri/memaccess.spec
L3SRCBASE+=mips-sml.spec
L3SRCBASE+=cheri/instructions.spec
L3SRCBASE+=mips-instructions.spec
L3SRCBASE+=cheri/decode.spec
L3SRCBASE+=mips-decode.spec
L3SRCBASE+=cheri/encode.spec
L3SRCBASE+=mips-encode.spec
L3SRCBASE+=cheri/next.spec
L3SRCBASE+=cheri/init.spec
else
L3SRCBASE+=tlb/types.spec
L3SRCBASE+=mips-types.spec
L3SRCBASE+=mips-log.spec
L3SRCBASE+=mips-base.spec
L3SRCBASE+=mips-pic.spec
L3SRCBASE+=mips-uart.spec
L3SRCBASE+=mips-exception.spec
L3SRCBASE+=tlb/base.spec
L3SRCBASE+=tlb/translate.spec
L3SRCBASE+=tlb/instructions.spec
L3SRCBASE+=mips-encode-utils.spec
ifdef CACHE
L3SRCBASE+=mips-memory-caches.spec
else
L3SRCBASE+=mips-memory.spec
endif
L3SRCBASE+=mips-memaccess.spec
L3SRCBASE+=mips-sml.spec
L3SRCBASE+=cp2-null/instructions.spec
L3SRCBASE+=mips-instructions.spec
L3SRCBASE+=cp2-null/decode.spec
L3SRCBASE+=mips-decode.spec
L3SRCBASE+=cp2-null/encode.spec
L3SRCBASE+=mips-encode.spec
L3SRCBASE+=mips-next.spec
L3SRCBASE+=cp2-null/init.spec
endif
L3SRCBASE+=mips-init.spec
L3SRC=$(patsubst %, $(L3SRCDIR)/%, $(L3SRCBASE))

# sml lib sources
#######################################
HOLSRCDIR=src/hol
SMLSRCDIR=src/sml
SMLLIBDIR=src/sml/lib
SMLLIBSRC=Runtime.sig Runtime.sml\
          IntExtra.sig IntExtra.sml\
          Nat.sig Nat.sml\
          L3.sig L3.sml\
          Bitstring.sig Bitstring.sml\
          BitsN.sig BitsN.sml\
          FP64.sig FP64.sml\
          Ptree.sig Ptree.sml\
          MutableMap.sig MutableMap.sml
SMLLIB=$(patsubst %, $(SMLLIBDIR)/%, $(SMLLIBSRC))

# generating the sml source list
#######################################
SMLSRCBASE+=mips.sig
SMLSRCBASE+=mips.sml
SMLSRCBASE+=run.sml
SMLSRCBASE+=l3mips.mlb
MLBFILE=l3mips.mlb
SMLSRC=$(patsubst %, $(SMLSRCDIR)/%, $(SMLSRCBASE))

# make targets
#######################################
ifdef CAP
ifdef CACHE
	SIM ?= l3mips-cheri$(CAP)-caches
else
	SIM ?= l3mips-cheri$(CAP)
endif
else
ifdef CACHE
	SIM ?= l3mips-caches
else
	SIM ?= l3mips
endif
endif

# memory subsystem params
L1SIZE ?= 16384
L1WAYS ?= 1
L1LINESIZE ?= 32
L2SIZE ?= 65536
L2WAYS ?= 2
L2LINESIZE ?= 64

SIM_PROFILE ?= l3mips_prof

M4_CHERI_FILES = $(basename $(wildcard ${L3SRCDIR}/cheri/*.spec.m4))
M4_FILES = $(basename $(wildcard ${L3SRCDIR}/*.spec.m4))

all: $(SIM)

hol: ${L3SRC}
	echo 'HolExport.spec ("${L3SRC}", "${HOLSRCDIR}/cheri")' | l3

count: ${L3SRC}
	@wc -l ${L3SRC}

$(L3SRCDIR)/cheri/memory-caches.spec: $(L3SRCDIR)/cheri/memory-caches.spec.m4
	m4 -I ${L3SRCDIR}/cheri/ -D CAP=$(CAP) -D L2SIZE=$(L2SIZE) -D L2WAYS=$(L2WAYS) -D L2LINESIZE=$(L2LINESIZE) -D L1SIZE=$(L1SIZE) -D L1WAYS=$(L1WAYS) -D L1LINESIZE=$(L1LINESIZE) $^ > $@

${L3SRCDIR}/%.spec: ${L3SRCDIR}/%.spec.m4
	m4 -I ${L3SRCDIR}/cheri/ -D CAP=$(CAP) $^ > $@

${SMLSRCDIR}/mips.sig ${SMLSRCDIR}/mips.sml: ${L3SRC}
	echo 'SMLExport.spec ("${L3SRC}", "${SMLSRCDIR}/mips")' | l3

${SIM}: ${SMLLIB} ${SMLSRC}
	mlton -inline 1000 -default-type intinf -verbose 1 -output ${SIM} ${SMLSRCDIR}/$(MLBFILE)

${SIM_PROFILE}: ${SMLLIB} ${SMLSRC}
	mlton -profile time -inline 1000 -default-type intinf -verbose 1 -output ./${SIM_PROFILE} ${SMLSRCDIR}/$(MLBFILE)

clean:
	rm -f ${SMLSRCDIR}/mips.sig ${SMLSRCDIR}/mips.sml
	rm -f $(M4_CHERI_FILES)
	rm -f $(M4_FILES)
