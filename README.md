# six-five-oh-two

## 6502 disassembler/emulator in Haskell.

Many 6502 emulators already exist, including some in Haskell. I am coding this one for the fun and challenge of it. And with the hope that the resulting code will have some kind of elegence.

So far the disassember is complete. Working on the emulator... [emulator complete!]
Maybe when it's working, I will attempt a full NES. [yup, this is started now!]

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


## NES notes

#### References

- [Colours: RGB mapping](http://www.thealmightyguru.com/Games/Hacking/Wiki/index.php/NES_Palette)

Have most of a full NES emulator wired up now. Now the debugging starts...

Current problem...

- When running an RTS instruction, the PC resumes at a very unexpected address.
- The Cpu MemMap decode refuses to handle it, assuming it's a bug (which it is)
- The resume address should come off the stack (from the last executed a JSR)
- So how come it is corrupted?
- Add some debug at the Mem Read/Write layer...
- Then can see the last instruction which wrote to this address

Here:

    F1CE  8D 07 20  STA $2007                       A:24 X:03 Y:02 P:24 SP:FD CYC:38200
    ("WRITE","wram",510,24)

- which is very wrong!
- This adress should be mem mapped to a PPU Reg -- PPUDATA -- which it is
- And this should then cause a write into the vram.. not the wram!
- fix it...
- ignore a few more sound register writes...
- Woo! see the DK title screen


## First speed measurements

- getting 1.25 fps. Terrible.
- obvious optimization to PRG ROM (to use array indexing): -> 15 fps
- same optimization for op-decode via: -> 21 fps

Now DK reaches the next crash/not-implemented yet point

    nes: CYC:10632429 - Regs.decode, too high: 3F1F
