# honesty

`honesty` is a NES emulator written in Haskell. Very much a WIP.

Perhaps it's named for _"Haskell un-Optimized NES emulator using TYpes"_


I'm writing it for fun, and to get more experience in how best to structure a reasonably complicated Haskell application.
During the development, I've fallen into a style of describing `Effect`s for different parts of the system, which get wired together using `inter`preter functions. Often the interpreter for one kind of effect will cause further effects.

Where possible I am using good old fashioned functional/persistent state, for example for the state of the CPU and PPU registers. For the larger 2k-Rams I am interpreting their effects in `IO` to make use of mutable arrays.

### Status

The 6502 emulator is done. And matches the golden log for the `nestest.nes` rom. And the remaining system (PPU Registers, NMI, OAM, DMA, Rendering, Controller, etc) is just about complete enough to allow Donkey Kong to run, albeit rather slowly.

For a more detailed description of the development progress, see the [developer log](diary.md).

There is still much todo:

- Increase speed: Currently it runs at about 1/3 speed required.. UPDATE: now about 3/4 speed
- Improve sprite rendering to properly support sprite overlap
- Add cycle-count to DMA (currently this happens in 0-time!)
- Revisit the spec for the PPU regs: `PPUCTRL`, `PPUMASK`, `PPUSTATUS` -- done
- Support sprite-0 collision
- Support sprite overflow
- Nametable mirroring
- Scrolling
- Mappers
- PPU cycle accuracy
- Sound, maybe!

Also: _Make something other than DK work!_ -- YES, ballon fight


### These tutorials and references were very helpful during 6502 emulation

- [obelisk.me.uk](http://www.obelisk.me.uk/6502/index.html)
- [6502.org](http://www.6502.org/tutorials/6502opcodes.html)
- [nesdev.com](https://wiki.nesdev.com/w/index.php/CPU)
- [emulator101](http://www.emulator101.com)

And the `nestest` rom and log from
[here](https://wiki.nesdev.com/w/index.php/Emulator_tests).

### Help on specific issues during 6502 emulation

- [OpCode <-> Instruction/Addressing-Mode mapping](http://www.emulator101.com/reference/6502-reference.html)
*Used as the basis of my [Opcode.hs](/src/Honesty/Six502/OpCode.hs).*
- [Flag effects & cycle times](http://www.obelisk.me.uk/6502/reference.html)
*Easiest navigation.*
- [Status Flags behaviour](https://wiki.nesdev.com/w/index.php/Status_flags)
*Including details of the mysterious B-flag!*
- [Subtraction on the 6502](http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html)
*"SBC simply takes the ones complement of the second value and then performs an ADC."*
- [Overflag flag following ADC/SBC](http://forums.nesdev.com/viewtopic.php?t=6331)
*"( (A ^ s) & (v ^ s) & 0x80 )."*


### Most visited NesDev pages

- [Top of the reference guide](http://wiki.nesdev.com/w/index.php/NES_reference_guide)
- [Top of the PPU](http://wiki.nesdev.com/w/index.php/PPU)
- [PPU registers](http://wiki.nesdev.com/w/index.php/PPU_registers)


### Other helpful links for NES Graphics / PPU
- [Dustmop on NES graphics: part 1](http://www.dustmop.io/blog/2015/04/28/nes-graphics-part-1/#chr-encoding),
[part 2](http://www.dustmop.io/blog/2015/06/08/nes-graphics-part-2), and
[part 3](http://www.dustmop.io/blog/2015/12/18/nes-graphics-part-3)
- [Austin Morlan on NES rendering](https://austinmorlan.com/posts/nes_rendering_overview/)
- [Colours: RGB mapping](http://www.thealmightyguru.com/Games/Hacking/Wiki/index.php/NES_Palette)



## Run/test with:

    ./test.sh
    stack run -- --speed
    stack run
    stack run -- --dis
    stack run -- --emu          (6502 only)
    stack run -- --tiny
    stack run -- --full
    stack run -- --full --fps 45
    stack run -- --full --fps 45 path/to/rom


### Keys for NES controller emulation

The standard NES controller had 8 buttons: Up, Down, Left, Right, A, B, START, SELECT.

These are mapped to the keyboard as:

- `Up` : Up
- `Down` : Down
- `Left` : Left
- `Right` : Right
- `z` : A
- `x` : B
- `Enter` : START
- `Tab` : SELECT


### Keys to control the simulator

- `Escape` : quit
- `Space` : pause simulation
- `F1` : cycle LHS display (initially blank):
- `F2` : cycle RHS display (initially full display: playfield + sprites)
- `F3` : toggle active sprite display
- `F4` : toggle fps/skipped/frame# display
- `F5` : toggle buttons pressed display
- `F6` : toggle PPU regs (Control/Mask) display

The display mode cycles though various debug displays and the full display:

- Blank
- Attribute table palette selection
- Just playfield
- Just sprites (on an empty background)
- Combined playfield & sprites
