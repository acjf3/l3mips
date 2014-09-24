L3 MIPS
=======

A **MIPS ISA simulator** implemented in [L3](http://www.cl.cam.ac.uk/~acjf3/l3/ "L3: An ISA Specification Language").

project hierarchy
-----------------

+ roms
+ src
    + l3
        + cheri
        + cp2-null
    + sml
        + lib

roms directory
--------------

This directory contains some MIPS binaries that the emulator can run.

###FreeBSD

To boot FreeBSD, download a kernel image:

`$ wget http://www.cl.cam.ac.uk/research/security/ctsrd/beri/downloads/20140616-cheribsd-beri-sim-mdroot-singleuser-kernel.bz2`

Uncompress it:

`$ bunzip2 20140616-cheribsd-beri-sim-mdroot-singleuser-kernel.bz2`

And run it through the emulator:

`$ ./l3mips --non-block on --ignore HI --ignore LO --format raw --at 1048576 20140616-cheribsd-beri-sim-mdroot-singleuser-kernel simboot.mem`

Booting takes around 10 mins on a modern PC.

For users in the Computer Laboratory, a dual-core kernel image is available in:
`/usr/groups/ctsrd/cheri/20140819-cheribsd-beri-sim-2core-mdroot-singleuser-kernel.bz2`

The dual core simulation can be run using:

`$ ./l3mips --nbcore 2 --non-block on --ignore HI --ignore LO --format raw --at 1048576 20140819-cheribsd-beri-sim-2core-mdroot-singleuser-kernel simboot.mem`

src directory
-------------

This directory contains the sources for the simulator. The L3 sources for the
model are under the **l3** directory, and the sml sources to generate an
executable simulator are found under the **sml** directory.

###l3 directory

The **l3** directory contains the sources for the MIPS ISA specification. To
generate a simulator, the list of files that is required is specified in the
Makefile. The is a conditional inclusion of specific files for a build with the
*CAP* environment variable set (which targets a build with the CHERI capability
coprocessor). Each *.spec* file specify a part of the ISA :

* **mips-base.spec**
Declares the state variables of the processor as well as a few utility functions
* **mips-encode.spec**
Defines some pretty printing and encoding functions for the MIPS instructions
* **mips-init.spec**
Defines the initial state of the processor
* **mips-memaccess.spec**
Defines the data load, data store, and instructions fetch operations
* **mips-sml.spec**
/!\\ need more refactoring /!\\
* **mips-tlb-translate.spec**
Defines the TLB address translation function
* **mips-types.spec**
Defines types used throughout the model
* **mips-decode.spec**
Defines the decoder and the next state function for the MIPS model
* **mips-exception.spec**
Defines the MIPS exception codes, a function to trigger an exception, and the ERET instruction
* **mips-instructions.spec**
Defines the semantic functions representing the MIPS instructions that are called in the decoder
* **mips-pic.spec**
Models the MIPS programmable interrupt controller
* **mips-tlb.spec**
Declares some TLB state variables, and defines TLB internal functions and TLB instructions
* **mips-tlb-types.spec**
Defines TLB entry types
* **mips-uart.spec**
Models a UART controller
