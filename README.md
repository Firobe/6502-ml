# 6502-ml

6502 chip family simulator in OCaml. Created for use in a [NES emulator in OCaml](https://github.com/Firobe/nes-ml).

![CI](https://github.com/Firobe/6502-ml/actions/workflows/build.yml/badge.svg)

**Documentation** containing simple examples and reference is available [online](https://firobe.fr/6502-ml/6502-ml/).

## Usage

Assuming you have `opam` installed with an existing switch, either:
- add a pin to this repo: 
   > `opam pin add nes-ml https://github.com/Firobe/6502-ml.git`

- or clone and run
  > `opam install .`

Furthermore, you can run the tests with `dune runtest`. To use the library in other programs, see the [online documentation](https://firobe.fr/6502-ml/6502-ml/).

Its zero page starts at address `0x0000`.  

## Development status

The CPU passes all functional tests of [Klaus Dormann's test suite](https://github.com/Klaus2m5/6502_65C02_functional_tests) (`klaus.bin` is an assembly of `6502_functional_test.a65`) and the [Nestest ROM](http://www.qmtpro.com/~nes/misc/nestest.txt) as compared to Nintendulator.  

Cycles count is accurate even for supported unofficial instructions, and tested.  

## References

Great ressources used:
- http://www.6502.org/tutorials/
- http://www.obelisk.me.uk/6502/
- https://wiki.nesdev.com/
- http://www.masswerk.at/6502/6502_instruction_set.html
- http://nesdev.com/6502_cpu.txt
