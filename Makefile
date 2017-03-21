#######################################
# Makefile for the L3 mips simulator ##
#######################################

# generating the L3 source list
# /!\ inclusion order matters /!\
#######################################
L3SRCDIR=src/l3

ifdef CAP
L3SRCBASE+=cheri/tlb-types.spec
else
L3SRCBASE+=tlb/types.spec
endif
L3SRCBASE+=mips-types.spec
L3SRCBASE+=mips-log.spec
L3SRCBASE+=mips-base.spec
ifndef SIMPLEMEM
L3SRCBASE+=mips-pic.spec
L3SRCBASE+=mips-uart.spec
endif
ifdef CAP
L3SRCBASE+=cheri/cap-params.spec
L3SRCBASE+=cheri/cap-common.spec
ifeq ($(CAP), c128posits1)
L3SRCBASE+=cheri/cap128-compressed-posits-1.spec
else ifeq ($(CAP), c128c1)
L3SRCBASE+=cheri/cap128-compressed-candidate-1.spec
else ifeq ($(CAP), c128c3)
L3SRCBASE+=cheri/cap128-compressed-candidate-3.spec
  ifdef FAST_REP_CHECK
  L3SRCBASE+=cheri/cap128-compressed-candidate-3-fast-representable-check.spec
  else
  L3SRCBASE+=cheri/cap128-compressed-candidate-3-representable-check.spec
  endif
else ifeq ($(CAP), p64)
L3SRCBASE+=cheri/cap-precise-base-256.spec cheri/cap64-precise.spec
else ifeq ($(CAP), p128)
L3SRCBASE+=cheri/cap-precise-base-256.spec cheri/cap128-precise.spec
else
L3SRCBASE+=cheri/cap-precise-base-256.spec cheri/cap256-precise.spec
endif
L3SRCBASE+=cheri/state.spec
L3SRCBASE+=cheri/exception.spec
else
L3SRCBASE+=mips-exception.spec
endif
L3SRCBASE+=tlb/base.spec
ifdef NOTRANSLATE
ifdef CAP
L3SRCBASE+=cheri/tlb-notranslate.spec
else
L3SRCBASE+=tlb/notranslate.spec
endif
else
ifdef CAP
L3SRCBASE+=cheri/tlb-translate.spec
else
L3SRCBASE+=tlb/translate.spec
endif
endif
L3SRCBASE+=tlb/instructions.spec
L3SRCBASE+=mips-encode-utils.spec
L3SRCBASE+=mips-cache-sizes.spec
ifdef CAP
ifdef SIMPLEMEM
L3SRCBASE+=cheri/memaccess-simple.spec
else
ifdef CACHE
L3SRCBASE+=cheri/cache-sizes.spec
L3SRCBASE+=cheri/memory-caches.spec
else
L3SRCBASE+=cheri/memory.spec
endif
L3SRCBASE+=cheri/memory-sml-helpers.spec
L3SRCBASE+=cheri/memaccess.spec
endif
else
ifdef SIMPLEMEM
L3SRCBASE+=mips-memaccess-simple.spec
else
ifdef CACHE
L3SRCBASE+=mips-memory-caches.spec
else
L3SRCBASE+=mips-memory.spec
endif
L3SRCBASE+=mips-memaccess.spec
endif
endif
L3SRCBASE+=mips-sml.spec
ifdef FPU
L3SRCBASE+=fpu/fpu-log.spec
L3SRCBASE+=fpu/state.spec
L3SRCBASE+=fpu/instructions.spec
else
L3SRCBASE+=cp1-null/instructions.spec
endif
ifdef CAP
L3SRCBASE+=cheri/instructions.spec
else
L3SRCBASE+=cp2-null/instructions.spec
endif
L3SRCBASE+=mips-instructions.spec
ifdef FPU
L3SRCBASE+=fpu/decode.spec
else
L3SRCBASE+=cp1-null/decode.spec
endif
ifdef CAP
L3SRCBASE+=cheri/decode.spec
else
L3SRCBASE+=cp2-null/decode.spec
endif
L3SRCBASE+=mips-decode.spec
ifdef FPU
L3SRCBASE+=fpu/encode.spec
else
L3SRCBASE+=cp1-null/encode.spec
endif
ifdef CAP
L3SRCBASE+=cheri/encode.spec
else
L3SRCBASE+=cp2-null/encode.spec
endif
L3SRCBASE+=mips-encode.spec
ifdef FPU
L3SRCBASE+=fpu/init.spec
else
L3SRCBASE+=cp1-null/init.spec
endif
ifdef CAP
L3SRCBASE+=cheri/next.spec
L3SRCBASE+=cheri/init.spec
else
L3SRCBASE+=mips-next.spec
L3SRCBASE+=cp2-null/init.spec
endif
ifndef SIMPLEMEM
L3SRCBASE+=mips-init.spec
endif

L3SRC=$(patsubst %, $(L3SRCDIR)/%, $(L3SRCBASE))

# hol / sml sources
#######################################
HOLSRCDIR=src/hol
SMLSRCDIR=src/sml
L3_SML_LIB ?= `l3 --lib-path`
L3SMLLIB=$(shell l3 --lib-path)

# generating the sml source list
#######################################
SMLLIBSRC+=$(L3SMLLIB)/Runtime.sig
SMLLIBSRC+=$(L3SMLLIB)/Runtime.sml
SMLLIBSRC+=$(L3SMLLIB)/IntExtra.sig
SMLLIBSRC+=$(L3SMLLIB)/IntExtra.sml
SMLLIBSRC+=$(L3SMLLIB)/Nat.sig
SMLLIBSRC+=$(L3SMLLIB)/Nat.sml
SMLLIBSRC+=$(L3SMLLIB)/Set.sig
SMLLIBSRC+=$(L3SMLLIB)/Set.sml
SMLLIBSRC+=$(L3SMLLIB)/L3.sig
SMLLIBSRC+=$(L3SMLLIB)/L3.sml
SMLLIBSRC+=$(L3SMLLIB)/Bitstring.sig
SMLLIBSRC+=$(L3SMLLIB)/Bitstring.sml
SMLLIBSRC+=$(L3SMLLIB)/BitsN.sig
SMLLIBSRC+=$(L3SMLLIB)/BitsN.sml
SMLLIBSRC+=$(L3SMLLIB)/Ptree.sig
SMLLIBSRC+=$(L3SMLLIB)/Ptree.sml
SMLLIBSRC+=$(L3SMLLIB)/MutableMap.sig
SMLLIBSRC+=$(L3SMLLIB)/MutableMap.sml
SMLLIBSRC+=$(L3SMLLIB)/FP.sig
SMLLIBSRC+=$(L3SMLLIB)/FP.sml
SMLLIBSRC+=$(L3SMLLIB)/NO_FP.sml
SMLLIBSRC+=$(L3SMLLIB)/FP64.sml
SMLSRCBASE+=mips.sig
SMLSRCBASE+=mips.sml
SMLSRCBASE+=run.sml
SMLSRCBASE+=l3mips.mlb
MLBFILE=l3mips.mlb
SMLSRC=$(patsubst %, $(SMLSRCDIR)/%, $(SMLSRCBASE))

# memory subsystem params
L1ISIZE ?= 32768
L1IWAYS ?= 2
L1DSIZE ?= 32768
L1DWAYS ?= 4
L1LINESIZE ?= 128
L2SIZE ?= 262144
L2WAYS ?= 4
L2LINESIZE ?= 128

NAME_STR=l3mips
ifdef NOTRANSLATE
NAME_STR:=$(NAME_STR)-no_translate
endif
ifdef SIMPLEMEM
NAME_STR:=$(NAME_STR)-simple_mem
endif
ifdef FPU
NAME_STR:=$(NAME_STR)-fpu
endif
ifdef CAP
NAME_STR:=$(NAME_STR)-cheri_$(CAP)
endif
ifdef CACHE
NAME_STR:=$(NAME_STR)-l2_$(L2SIZE)B_$(L2WAYS)ways_$(L2LINESIZE)Bpl
endif
SIM ?= $(NAME_STR)
SIMPOLY ?= $(shell pwd)/$(NAME_STR)-poly

SIM_PROFILE ?= l3mips_prof
SIM_COVERAGE ?= l3mips_coverage

M4_CHERI_FILES = $(basename $(wildcard ${L3SRCDIR}/cheri/*.spec.m4))
M4_FILES = $(basename $(wildcard ${L3SRCDIR}/*.spec.m4))

# make targets
#######################################

all: $(SIM)

hol: ${L3SRC}
	echo 'HolExport.sigDocs false; HolExport.bigRecords false; HolExport.spec ("${L3SRC}", "${HOLSRCDIR}/cheri")' | l3

isabelle: ${L3SRC}
	echo 'IsabelleExport.spec ("${L3SRC}", "${HOLSRCDIR}/CHERI")' | l3

isabelle_monadic: ${L3SRC}
	echo 'IsabelleExport.monadicExport true; IsabelleExport.spec ("${L3SRC}", "${HOLSRCDIR}/CHERI_Monadic")' | l3

count: ${L3SRC}
	@wc -l ${L3SRC}

${L3SRCDIR}/%.spec: ${L3SRCDIR}/%.spec.m4
	m4 -I ${L3SRCDIR}/cheri/ -D CAP=$(CAP) -D L2SIZE=$(L2SIZE) -D L2WAYS=$(L2WAYS) -D L2LINESIZE=$(L2LINESIZE) -D L1ISIZE=$(L1ISIZE) -D L1IWAYS=$(L1IWAYS) -D L1DSIZE=$(L1DSIZE) -D L1DWAYS=$(L1DWAYS) -D L1LINESIZE=$(L1LINESIZE) $^ > $@

${L3SRCDIR}/cheri/%.spec: ${L3SRCDIR}/cheri/%.spec.m4
	m4 -I ${L3SRCDIR}/cheri/ -D CAP=$(CAP) -D L2SIZE=$(L2SIZE) -D L2WAYS=$(L2WAYS) -D L2LINESIZE=$(L2LINESIZE) -D L1ISIZE=$(L1ISIZE) -D L1IWAYS=$(L1IWAYS) -D L1DSIZE=$(L1DSIZE) -D L1DWAYS=$(L1DWAYS) -D L1LINESIZE=$(L1LINESIZE) $^ > $@

MLTON ?= mlton
POLYC ?= polyc

${SMLSRCDIR}/mips.sig ${SMLSRCDIR}/mips.sml: ${L3SRC}
	echo 'SMLExport.setIntInf true; SMLExport.spec ("${L3SRC}", "${SMLSRCDIR}/mips")' | l3

poly: ${SMLSRC} ${SMLLIBSRC} ${SMLSRCDIR}/run-poly.sml
	cd ${SMLSRCDIR} && $(POLYC) -o ${SIMPOLY} run-poly.sml

${SIM}: ${SMLSRC} ${SMLLIBSRC}
	$(MLTON) -inline 1000 -default-type intinf -verbose 2 -output ${SIM} -mlb-path-var 'L3_SML_LIB '$(L3_SML_LIB) ${SMLSRCDIR}/$(MLBFILE)

${SIM_PROFILE}: ${SMLSRC} ${SMLLIBSRC}
	$(MLTON) -profile time -inline 1000 -default-type intinf -verbose 2 -output ./${SIM_PROFILE} -mlb-path-var 'L3_SML_LIB '$(L3_SML_LIB) ${SMLSRCDIR}/$(MLBFILE)

${SIM_COVERAGE}: ${SMLSRC} ${SMLLIBSRC}
	$(MLTON) -profile count -profile-branch true -inline 1000 -default-type intinf -verbose 2 -output ./${SIM_COVERAGE} -mlb-path-var 'L3_SML_LIB '$(L3_SML_LIB) ${SMLSRCDIR}/$(MLBFILE)

clean:
	rm -f ${SMLSRCDIR}/mips.sig ${SMLSRCDIR}/mips.sml
	rm -f $(M4_CHERI_FILES)
	rm -f $(M4_FILES)
