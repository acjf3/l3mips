#######################################
# Makefile for the L3 mips simulator ##
#######################################

# The CAP environment variable, when defined,
# enables the CHERI capability coprocessor

# generating the L3 source list
# /!\ inclusion order matters /!\
#######################################
L3SRCDIR=src/l3
ifdef CAP
L3SRCBASE+=cheri/tlb-types.spec
L3SRCBASE+=mips-types.spec
L3SRCBASE+=mips-base.spec
L3SRCBASE+=mips-pic.spec
L3SRCBASE+=mips-uart.spec
L3SRCBASE+=cheri/state.spec
L3SRCBASE+=cheri/exception.spec
L3SRCBASE+=mips-tlb.spec
L3SRCBASE+=cheri/tlb-translate.spec
L3SRCBASE+=mips-sml.spec
L3SRCBASE+=cheri/memaccess.spec
L3SRCBASE+=cheri/instructions.spec
L3SRCBASE+=mips-instructions.spec
L3SRCBASE+=cheri/decode.spec
L3SRCBASE+=mips-decode.spec
L3SRCBASE+=cheri/encode.spec
L3SRCBASE+=cheri/init.spec
else
L3SRCBASE+=mips-tlb-types.spec
L3SRCBASE+=mips-types.spec
L3SRCBASE+=mips-base.spec
L3SRCBASE+=mips-pic.spec
L3SRCBASE+=mips-uart.spec
L3SRCBASE+=mips-exception.spec
L3SRCBASE+=mips-tlb.spec
L3SRCBASE+=mips-tlb-translate.spec
L3SRCBASE+=mips-sml.spec
L3SRCBASE+=mips-memaccess.spec
L3SRCBASE+=cp2-null/instructions.spec
L3SRCBASE+=mips-instructions.spec
L3SRCBASE+=cp2-null/decode.spec
L3SRCBASE+=mips-decode.spec
L3SRCBASE+=cp2-null/encode.spec
L3SRCBASE+=cp2-null/init.spec
endif
L3SRCBASE+=mips-encode.spec mips-init.spec
L3SRC=$(patsubst %, $(L3SRCDIR)/%, $(L3SRCBASE))

# sml lib sources
#######################################
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

# make targets
#######################################

all: l3mips

${SMLSRCDIR}/mips.sig ${SMLSRCDIR}/mips.sml: ${L3SRC}
	echo 'SMLExport.spec ("${L3SRC}", "${SMLSRCDIR}/mips")' | l3

l3mips: ${SMLLIB} ${SMLSRCDIR}/mips.sig ${SMLSRCDIR}/mips.sml ${SMLSRCDIR}/run.sml ${SMLSRCDIR}/l3mips.mlb
	mlton -inline 1000 -default-type intinf -verbose 1 -output ./l3mips ${SMLSRCDIR}/l3mips.mlb

clean:
	rm -f l3mips
	rm -f ${SMLSRCDIR}/mips.sig ${SMLSRCDIR}/mips.sml
