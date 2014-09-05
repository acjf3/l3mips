#######################################
# Makefile for the L3 mips simulator ##
#######################################

# The CAP environement variable, when defined,
# enables the CHERI capability coprocessor

# generating the L3 source list
# /!\ inclusion order matters /!\
#######################################
L3SRCDIR=src/l3
L3SRCBASE=mips-base.spec mips-pic.spec mips-uart.spec mips-sml.spec mips-memaccess.spec
ifdef CAP
L3SRCBASE+=cheri/mips-cheri-memaccess.spec
L3SRCBASE+=cheri/mips-cheri-instructions.spec
L3SRCBASE+=mips.spec
L3SRCBASE+=cheri/mips-cheri-decode.spec
L3SRCBASE+=mips-decode.spec
L3SRCBASE+=cheri/mips-cheri-encode.spec
else
L3SRCBASE+=mips-memaccess.spec
L3SRCBASE+=mips-cp2default-instructions.spec
L3SRCBASE+=mips.spec
L3SRCBASE+=mips-cp2default-decode.spec
L3SRCBASE+=mips-decode.spec
L3SRCBASE+=mips-cp2default-encode.spec
endif
L3SRCBASE+=mips-encode.spec
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
	mlton -default-type intinf -verbose 1 -output ./l3mips ${SMLSRCDIR}/l3mips.mlb

clean:
	rm -f l3mips
	rm -f ${SMLSRCDIR}/mips.sig ${SMLSRCDIR}/mips.sml
