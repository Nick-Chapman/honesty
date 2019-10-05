
## Repo

- rename -> `honesty`
- update Readme.md and TODO.md
- start diary.md


## NES, things to support

- PPU regs - status, control, mask - detailed/correct behaviour
- Dma
- Oam
- sprite render
- sprite collision
- nametable mirroring
- scroll

- PPU cycle accuracy
- mappers

- Sound (one day!)


## Debug

- Logging for important events (i.e. read a button press!)
- Graphical Display: cycles
- show cycle/pc reached on any crash + dump other status (pp regs state, control state, etc) + mem status
- Byte support un-defined elements (for unitialized ram reads etc)
- Support un-defined sub-bits of a byte


## Refactoring

- Step continuation mess in Top.hs


## Optimization ideas

- Addr, represent as 2 bytes, more efficient?
- cache op-fetch/decode (it's a rom, so we can!)
