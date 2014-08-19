l3mips: ../lib/Runtime.sig ../lib/Runtime.sml ../lib/IntExtra.sig \
        ../lib/Nat.sig ../lib/Nat.sml ../lib/L3.sig ../lib/L3.sml \
        ../lib/Bitstring.sig ../lib/Bitstring.sml ../lib/BitsN.sig \
        ../lib/FP64.sig ../lib/FP64.sml ../lib/Ptree.sig ../lib/Ptree.sml \
        ../lib/IntExtra.sml ../lib/BitsN.sml \
        ../lib/MutableMap.sig ../lib/MutableMap.sml \
        mips.sig mips.sml run.sml l3mips.mlb
	mlton -default-type intinf -verbose 1 l3mips.mlb

clean:
	rm l3mips
