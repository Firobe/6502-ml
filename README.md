# 6502-ml
6502 chip family simulator in OCaml

**Documentation** containing simple examples and reference is available [online](https://firobe.fr/6502-ml/).

The CPU passes all functional tests of [Klaus Dormann's test suite](https://github.com/Klaus2m5/6502_65C02_functional_tests) (`klaus.bin` is an assembly of `6502_functional_test.a65`) and the [Nestest ROM](http://www.qmtpro.com/~nes/misc/nestest.txt) as compared to Nintendulator.  

Its zero page starts at address `0x0000`.  
Cycles count is accurate even for supported unofficial instructions, and tested.  

Great ressources used :
- http://www.6502.org/tutorials/
- http://www.obelisk.me.uk/6502/
- https://wiki.nesdev.com/
- http://www.masswerk.at/6502/6502_instruction_set.html
- http://nesdev.com/6502_cpu.txt

## Usage

- Build and install with `opam install .`
- Launch tests with `dune test`
