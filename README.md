# six-five-oh-two

## 6502 disassembler/emulator in Haskell.

Many 6502 emulators already exist, including some in Haskell. I am coding this one for the fun and challenge of it. And with the hope that the resulting code will have some kind of elegence.

So far the disassember is complete. Working on the emulator...
Maybe when it's working, I will attempt a full NES.

#### Following these tutorials and references:

- [obelisk.me.uk](http://www.obelisk.me.uk/6502/index.html)
- [6502.org](http://www.6502.org/tutorials/6502opcodes.html)
- [nesdev.com](https://wiki.nesdev.com/w/index.php/CPU)
- [emulator101](http://www.emulator101.com)

And the `nestest` rom and log from
[here](https://wiki.nesdev.com/w/index.php/Emulator_tests).

#### Specific issues:

- [OpCode <-> Instruction/Addressing-Mode mapping](http://www.emulator101.com/reference/6502-reference.html)
*Used as the basis of my [Opcode.hs](https://github.com/Nick-Chapman/six-five-oh-two/blob/master/src/Six502/OpCode.hs).*
- [Flag effects & cycle times](http://www.obelisk.me.uk/6502/reference.html)
*Easiest navigation.*
- [Status Flags behaviour](https://wiki.nesdev.com/w/index.php/Status_flags)
*Including details of the mysterious B-flag!*
- [Subtraction on the 6502](http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html)
*"SBC simply takes the ones complement of the second value and then performs an ADC."*
- [Overflag flag following ADC/SBC](http://forums.nesdev.com/viewtopic.php?t=6331)
*"( (A ^ s) & (v ^ s) & 0x80 )."*

Run/test with:

    stack build --file-watch --exec ./go.sh
