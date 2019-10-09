
# Developer Log / Timeline

### Sep 9, 2019

- Begin 6502 disassembler in Haskell.

### Sep 12, 2019

- 6502 disassembler complete. Begin emulation.

### Sep 14, 2019

- Support for all official 6502 instructions complete. Some unofficial instruction supported too. Get cycle accurate match with `nestest.nes` rom. At least until `DOP`...
- But enough; I am itching to move on to the rest of the NES.

### Sep 17,18 2019

- First steps with NES graphics
- Visualize CHR tile data loaded from ROM
- Begin driving dev using Donkey Kong ROM...

### Sep 21,22 2019

- Development of the various components of the NES architecture:
    - Ram2k
    - MemoryMap
    - PPU Regs
    - PRG-ROM
    - Combined CPU/PPU
- CPI/PPU renders blank screen for DK
- NMI support
- begin controller support

### Sep 24-26, 2019

- Re-think overall architecture while in York. `Effect`s and `inter`preters!
- A lot of recoding and refactoring

### Sep 27, 2019

Debugging issue when running DK...

- When running an RTS instruction, the PC resumes at a very unexpected address.
- The Cpu MemMap decode refuses to handle it, assuming it's a bug (which it is)
- The resume address should come off the stack (from the last executed JSR)
- So how come it is corrupted?
- Add debug at the Mem Read/Write layer...
- Then can see the last instruction which wrote to this address:

Here:

    F1CE  8D 07 20  STA $2007                       A:24 X:03 Y:02 P:24 SP:FD CYC:38200
    ("WRITE","wram",510,24)

- Which is very wrong!
- This address should be mem-mapped to a PPU Reg -- PPUDATA -- which it is.
- And this should then cause a write into the VRAM.. not the WRAM!

Fix it...

- Ignore a few more sound register writes...until...
- Wey hey! See the DK title screen!!


### Sep 28, 2019

First speed measurements:

- Getting 1.25 fps. Terrible.
- Obvious optimization to PRG ROM (to use array indexing): -> 15 fps
- Same optimization for op-decode via: -> 21 fps
- (Note, these fps don't include any rendering!)

And with the improved speed, get a first glimpse of DK game screen (level-1).


### Sep 30, 2019

- use bitmaps to speed up Gloss graphics
- Do some measurement to display dropped frame count

### Oct 1-4, 2019

- never ending refactoring, until finally I am happy with
- the top level split between _emulation_ and _simulation_ code.
- Nice article on r/emudev
[nes rendering overview](https://www.reddit.com/r/EmuDev/comments/dblwr2/nes_rendering_overview/). But I see that my DK colours are wrong!


### Oct 5, 2019

- Recode playfield graphics rendering from scratch
- fix palette and attribute table palette select issues, until...
- the colours for DK are correct!

### Oct 6, 2019

- Early start: OAM, DAM, Sprite palettes, debug sprite render
- combine playfield and sprite graphics to render complete DK game level
- play level-1 (slowly) and see level-2 for the first time

### Oct 7, 2019

- Preparing to rename project/repo as `honesty`
- Re-write the github README.md
- Begin this diary.md (adding retrospective entries), looking at commits

### Oct 8, 2019

- profiling... get fps to about 30

### Oct 9, 2019

- get fps of 45 on a newer laptop!
- focus on correct handling of PPU regs control/mask/status - and display values in gloss
- investigate why ice/ballon dont seem to do anything
- fix long standing bugs in setVBlank.. was updating control instead of status register!
- YES.. see title page for and game demo for balloon.nes !!
