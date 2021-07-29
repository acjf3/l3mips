L3 MIPS
=======

A **MIPS ISA simulator** implemented in [L3](https://acjf3.github.io/l3/index.html).
The simulator also supports [CHERI](http://www.chericpu.org "Capability Hardware Enhanced RISC Instructions (CHERI) ") capability extensions.

Getting started
---------------

To build the simulator, you need L3 installed on your machine.

L3 is implemented in [Poly/ML 5.7](http://www.polyml.org/ "Poly/ML home page"), so in order to compile L3, you will need Poly/ML correctly installed.
Simply follow the steps described on the [Poly/ML download page](http://www.polyml.org/download.html "Poly/ML download page"). It is basically a case of:

```
wget https://downloads.sourceforge.net/project/polyml/polyml/5.7.1/polyml.5.7.1.tar.gz
tar -xf polyml.5.7.1.tar.gz
cd polyml-5.7.1/
./configure
make
sudo make install
```

You can download the sources for L3 and the L3 manual on the [L3 page](http://www.cl.cam.ac.uk/~acjf3/l3/ "L3: An ISA Specification Language").
The sources are shipped as a *.tar.bz2* archive. Extract them and `cd` into the root directory of the archive:

```
wget https://www.cl.cam.ac.uk/~mr101/l3/l3.tar.bz2
tar -xf l3.tar.bz2
cd L3-AAAA-MM-DD/
```

(with `L3-AAAA-MM-DD/` matching the date of your L3 release).

You should now be able to build L3 with:

```
make
```

Export the path to the **l3** binary in your `PATH` environment variable (if you are going to hack on the simulator's source code, you might want to consider adding that to your `.bashrc`):

```
export PATH=__path__/__to__/L3-AAAA-MM-DD/bin:$PATH
```

(with `__path__/__to__/` replaced with what is appropriate for your setup)

Once L3 is setup, you can get this project's source code :

```
git clone git@github.com:acjf3/l3mips.git
cd l3mips
```

and build the MIPS simulator, either using `mlton` if it is installed on your machine:

```
make
```

or using PolyML:

```
make poly
```

To build the [CHERI](http://www.chericpu.org "Capability Hardware Enhanced RISC Instructions (CHERI) ") MIPS simulator,
you will need the [m4](https://www.gnu.org/software/m4/) macro processor installed:

```
sudo apt-get install m4
```

You can then build your CHERI simulator, specifying a value in the `CAP` variable. Possible values for this variable are :
+ `p256` : 256-bits wide precise capabilities
+ `p128` : 128-bits wide precise capabilities
+ `p64`  : 64-bits wide precise capabilities (no compiler support yet, so currently unusable)
+ `c128c1` : 128-bits wide compressed capabilities, candidate 1
+ `c128c2` : 128-bits wide compressed capabilities, candidate 2
+ `c128c3` : 128-bits wide compressed capabilities, candidate 3
    - When using `CAP=c128c3`, the `FAST_REP_CHECK` variable can be set to `1` to use the fast representability check and left undefined to use the accurate representability check for capability manipulations taking pointers far out of bounds.
+ `c128concentrate` : 128-bits wide compressed capabilities, cheri concentrate
+ anything else : same as `p256`

```
make CAP=p256
```

To build the simulator with a floating point unit, set the `FPU` variable to `1`:

```
make FPU=1
```

To build the simulator with caches, set the `CACHE` variable to `1`:

```
make CACHE=1
```

Further variables can be set to control parameters of the caches:
+ `L1SIZE`     : L1 cache size in bytes (default is `16384`)
+ `L1WAYS`     : number of ways in the L1 cache (default is `1`, direct mapped)
+ `L1LINESIZE` : L1 cache line size in bytes (default is `128`)
+ `L2SIZE`     : L2 cache size in bytes (default is `65536`)
+ `L2WAYS`     : number of ways in the L1 cache (default is `4`, 4-way set associative)
+ `L2LINESIZE` : L2 cache line size in bytes (default is `128`)

For example, to build a simulator with caches, with a 2-way set associative L1:

```
make CACHE=1 L1WAYS=2
```

To build the simulator without virtual memory, set the `NOTRANSLATE` variable to `1`:

```
make NOTRANSLATE=1
```

To build the simulator without peripherals and without memory translation (simplifies export to HOL, but does not support logging or stats mechanisms), set the `SIMPLEMEM` variable to `1`:

```
make SIMPLEMEM=1
```

To control the name of the generated simulator, set the `SIM` variable:

```
make SIM=mySimulatorName
```

Running the simulator
---------------------

When running the `l3mips` simulator, a set of command line arguments can be specified. A list of these flags is provided when running the simulator with `-h` or `--help`, or without any argument. The simulator should be run as follows:

```
l3mips [arguments] <file>
```

with `<file>` being the binary file to execute, and `[arguments]` an optional list of arguments drawn from the following:

+ `--cycles <number>`       : maximum number of instructions being simulated
+ `--trace <level>`         : trace verbosity level (0 default, 5 maximum)
+ `--pc <address>`          : initial program counter value and start address for main Intel Hex file
+ `--at <address> <file>`   : load extra `<file>` into physical memory at location `<address>`
+ `--watch-paddr <address>` : specify a physical `<address>` to monitor memory accesses
+ `--nbcore <number>`       : `<number>` of core(s) to simulate (default is 1)
+ `--uart <address>`        : base physical `<address>` for the UART device
+ `--uart-delay <number>`   : UART cycle delay (determines baud rate)
+ `--uart-in <file>`        : UART input `<file>` (stdin if omitted)
+ `--uart-out <file>`       : UART output `<file>` (stdout if omitted)
+ `--trace-out <file>`      : Traces output `<file>` (stdout if omitted)
+ `--stats-out <file>`      : Stats output `<file>` (stdout if omitted)
+ `--stats-format <format>` : Stats output format. `<format>` can be set to `csv`, or to anything else for human readable stats output format (default is human readable)
+ `--format <format>`       : File format for the specified binaries. `<format>` can be set to `raw` or `hex`.
+ `--non-block <on|off>`    : non-blocking UART input `on` or `off`
+ `--dump-stats <number>`   : display statistics every `<number>` simulation steps
+ `--rdhwr-extra <on|off>`  : enable simulator control features through rdhwr inst, `on` or `off`
+ `--schedule <file>`       : `<file>` of core ids indicating schedule. When omitted, round robin between cores
+ `--ignore <string>`       : UNPREDICTABLE#(`<string>`) behaviour is ignored (currently `<string>` must be `HI` or `LO`)
+ `-h` or `--help`          : print help message

Booting FreeBSD
---------------

To boot FreeBSD, you will need a kernel image. There exist a CHERI port of FreeBSD, [CheriBSD](https://github.com/CTSRD-CHERI/cheribsd), available on github.

Some kernel images are readily available in the [BERI cpu website software download section](http://www.cl.cam.ac.uk/research/security/ctsrd/beri/downloads-sw.html). To get "arcina-cheribsd-beri-sim-mdroot-singleuser-kernel", run the following commands:

```
wget http://www.cl.cam.ac.uk/research/security/ctsrd/beri/downloads/arcina-cheribsd-beri-sim-mdroot-singleuser-kernel.bz2
bunzip2 arcina-cheribsd-beri-sim-mdroot-singleuser-kernel.bz2
```

To run your kernel image in the `l3mips` simulator, run the command:

```
./l3mips --non-block on --ignore HI --ignore LO --format raw --at 1048576 <your-kernel-image> roms/simboot.mem
```

Booting a FreeBSD kernel and getting to a user prompt takes around 10 mins on a recent PC.

Project hierarchy
-----------------

+ roms
+ src
    + l3
        + cheri
        + cp2-null
        + tlb
    + sml
        + lib

src directory
-------------

This directory contains the sources for the simulator. The L3 sources for the
model are under the **l3** directory, and the sml sources to generate an
executable simulator are found under the **sml** directory.

###l3 directory

The **l3** directory contains the sources for the MIPS ISA specification. To
generate a simulator, the list of files that is required is specified in the
Makefile. There is a conditional inclusion of specific files for a build with
the *CAP* environment variable set (which targets a build with the CHERI
capability coprocessor). Each *.spec* file specify a part of the ISA :

* **mips-base.spec**
Declares the state variables of the processor as well as a few utility functions
* **mips-decode.spec**
Defines the MIPS instruction decoder
* **mips-encode.spec**
Defines some pretty printing and encoding functions for the MIPS instructions
* **mips-exception.spec**
Defines the MIPS exception codes, a function to trigger an exception, and the ERET instruction
* **mips-init.spec**
Defines the initial state of the processor
* **mips-instructions.spec**
Defines the semantic functions representing the MIPS instructions that are called in the decoder
* **mips-memaccess.spec**
Defines the data load, data store, and instructions fetch operations
* **mips-sml.spec**
/!\\ need more refactoring /!\\
* **mips-next.spec**
Defines the next state function for the MIPS model
* **mips-pic.spec**
Models the MIPS programmable interrupt controller
* **mips-types.spec**
Defines types used throughout the model
* **mips-uart.spec**
Models a UART controller
* tlb directory
    * **base.spec**
      Declares some TLB state variables and internal functions
    * **instructions.spec**
      Defines TLB instructions
    * **translate.spec**
      Defines the TLB address translation function
    * **types.spec**
      Defines TLB entry types

