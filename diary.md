
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

### Oct 11, 2019

- Fiddling around trying to see how scrolling should work. Added debug flag,
- See if other games work at all, and if not, where do they get stuck?
- DK and Balloon fight are the two working games so far.
- Also now tried Tennis and Popyeye.. these both seem to work too!
- Ice is said to use vertical scrolling (the easier kind) but it crashes.
- SMB is failing because it has 2 roms (that should be easy to add - I had it before)
- Gunsmoke - also has multiple roms.. 8. Another day!
- Ice.nes is crashing because it tries to write to a high PPU Mem address E... (via 0x2006)
    - is this a bug?
    - or game logic going wrong because say sprite0-hit is not implemented?
    - or do I just need mirroring for high (> 0x3fff) PPU Mem addresses?
- Try mirroring.. now Ice crashes as it attempts to write to ROM.
- After adding support for 2 ROMS, SMB also crashes at a ROM write.
- I believe ROM writes are signals to the game cart,.. so lets just ignore them.
- YES, see the SMB title screen! But ice is still blank.. is this a NT mirroring/select issues?
- Thinking about code restructure needed to allow PPU/CPU to communicate mid frame
    - CPU -> PPU : to update scrolling info
    - PPU -> CPU : to indicate when sprite0-hit occurred

### Oct 12, 2019

- trying to support NametableMirroring, but it's no right yet.
- making some progress with Ice.nes..
    - I can actually see something (after Enter is pressed)
- but mostly just logging in a more convenient way, using Log.effect, which respect --debug
- first steps with scrolling (just Y scrolling)
- Ice kinda works, but it's not right :(

### Oct 18, 2009

- Some some days off: was getting nowhere with debugging Ice.
- Had a play with hnes. Ice works here, and I can get a log. But it's very had to compare.
- Started to suspect my CPU..
- Perhaps I can compare logs against hnes on a non-PPU test rom. The blargg tests!
- So that's what I will plan for this morning's train ride.

01-basics:
- Discovered that my CPU doesn't implement CLI
- bit of jiggling to line up traces... see divergence on BIT instruction..

### Oct 20, 2009

- actually the BIT instruction is fine
- issue is just that the blaarg rom does write to the PPU, and so I get timing issuess vs hnes
- but the blaarg also writes to memory location $6004... and I can see that it says test passed.. ok!
- but why can't I see on the screem?
- hmm, the screen goes black, so something is being written
- hmm, check the other playfield (recently re-enabled as a debug view).. YES, see the test past written here
- ok, so I think I need to check that I am respecting which NT is shown
