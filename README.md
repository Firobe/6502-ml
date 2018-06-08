# 6502-ml
6502 chip family simulator in OCaml

Exposed as a library through `cpu.mli`  
`jbuilder build unit_test.exe` creates `_build/default/unit_test.exe` which takes a path to a ROM in argument and launch the CPU on it, beginning at `PC=0x400`, stopping when encountering a trap.  

The CPU passes all functional tests of [Klaus Dormann's test suite](https://github.com/Klaus2m5/6502_65C02_functional_tests) (`klaus.bin` is an assembled of `6502_functional_test.a65) and the [Nestest ROM](http://www.qmtpro.com/~nes/misc/nestest.txt)  

It's zero page starts at address `0x0000`  

Great ressources used :
- http://www.6502.org/tutorials/
- http://www.obelisk.me.uk/6502/
- https://wiki.nesdev.com/
- http://www.masswerk.at/6502/6502_instruction_set.html
- http://nesdev.com/6502_cpu.txt
