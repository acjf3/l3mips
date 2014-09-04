LIBSRC=lib/Runtime.sig lib/Runtime.sml\
       lib/IntExtra.sig lib/IntExtra.sml\
       lib/Nat.sig lib/Nat.sml\
       lib/L3.sig lib/L3.sml\
       lib/Bitstring.sig lib/Bitstring.sml\
       lib/BitsN.sig lib/BitsN.sml\
       lib/FP64.sig lib/FP64.sml\
       lib/Ptree.sig lib/Ptree.sml\
       lib/MutableMap.sig lib/MutableMap.sml
SRCDIR=src
L3SRCBASE=mips-base.spec mips-pic.spec mips-uart.spec mips-sml.spec
ifdef CAP
L3SRCBASE+=mips-cheri-instructions.spec mips.spec mips-cheri-decode.spec mips-decode.spec mips-cheri-encode.spec mips-encode.spec
else
L3SRCBASE+=mips-cp2default-instructions.spec mips.spec mips-cp2default-decode.spec mips-decode.spec mips-cp2default-encode.spec mips-encode.spec
endif
L3SRC=$(patsubst %, src/l3/%, $(L3SRCBASE))

all: l3mips

${SRCDIR}/mips.sig ${SRCDIR}/mips.sml: ${L3SRC}
	echo 'SMLExport.spec ("${L3SRC}", "${SRCDIR}/mips")' | l3

l3mips: ${LIBSRC} ${SRCDIR}/mips.sig ${SRCDIR}/mips.sml ${SRCDIR}/run.sml ${SRCDIR}/l3mips.mlb
	mlton -default-type intinf -verbose 1 -output ./l3mips ${SRCDIR}/l3mips.mlb

clean:
	rm -f l3mips
	rm -f ${SRCDIR}/mips.sig ${SRCDIR}/mips.sml
