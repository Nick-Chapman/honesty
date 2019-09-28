
## Repo

- rename -> `honesty`
- update Readme.md and TODO.md
- start diary.md


## NES, things to support

- PPU regs - status, control, mask - detailed/correct behaviour
- Pallet
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

- synchronization of button events / picture display / logging -- still very laggy
- Logging for important events (i.e. read a button press!)
- Graphical Display: cycles
- show cycle/pc reached on any crash + dump other status (pp regs state, control state, etc) + mem status
- Byte support undefined elements (for unitialized ram reads etc)
- Support undefined sub-bits of a byte


## Refactoring

- NesState type to sep file Nes.State
- Gloss picture code to sep file
- Step continuation mess in Top.hs
- split up Top.hs & move to correct places
- MemMapEffect to own file
- render code to own file

- Byte/Addr -> Top level


## Optimization ideas

- Addr, represent as 2 bytes, more efficient?
- cache op-fetch/decode (it's a rom, so we can!)
