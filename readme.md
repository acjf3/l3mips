L3 MIPS
=======

A **MIPS ISA simulator** implemented in [L3](http://www.cl.cam.ac.uk/~acjf3/l3/ "L3: An ISA Specification Language").

---

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

For users in the Computer Laboratory, a dual-core kernel image is available in `/usr/groups/ctsrd/cheri/20140819-cheribsd-beri-sim-2core-mdroot-singleuser-kernel.bz2`
The dual core simulation can be run using `$ ./l3mips --nbcore 2 --non-block on --ignore HI --ignore LO --format raw --at 1048576 20140819-cheribsd-beri-sim-2core-mdroot-singleuser-kernel simboot.mem`

---

src directory
-------------
+ l3
..* cheri
..* sml

