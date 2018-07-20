# Lunatic86

Barely-IBM-compatible PC emulator written in Lua 5.3.

## Compilation Instructions

1. Run ./compile.sh on a (preferably?) Linux machine.
2. Edit disks.lua to point to your disk images.
3. Run platform_oc.lua in OpenComputers or Kallisti, or platform.lua with lcurses (text mode only).

## What's Emulated?

### Probably decently

* CPU: 8086-esque
* Graphics: CGA (640x200x1, 320x200x2, text modes), 320x200x8 (Mode 13h)
    * With some partial I/O ports emulation!
* BIOS: High-level emulation
    * Decent implementations of most expected interrupts

### Pretend

* Audio: PC Speaker (Computronics Beep Card)
* Keyboard: PC XT-compatible
* 8259 PIC
* 8253 PIT

### Not even tested

* Graphics: Tandy-compatible

### Not even implemented

* BIOS: Serial/Printer Port interrupts, INT 15h
* Pretty much anything not listed

## Notes

* platform.lua outputs debug text to stderr. Run with "2>/dev/null" to avoid.
* The performance on my desktop (i7-4790K) seems to be:
    * about 3x PC XT in Lua 5.3,
    * about 0.6x PC XT in latest stable OpenComputers,
    * about 1x PC XT in latest stable OpenComputers + [fixed machine.lua](https://github.com/MightyPirates/OpenComputers/pull/2877) - you may override 
it server-side,
    * about 1.8x PC XT in latest stable OpenComputers + fixed machine.lua + natives recompiled to actually use -O2 and not debug mode,
    * about 2.5x PC XT in Kallisti with the timeout-checking code disabled altogether, and about 1.8x PC XT otherwise.
* There's a lot of bugs and hacks.

## Known issues

Many. See the Wiki for the Compatibility List.

In particular, a "reduced memory usage" mode has to be introduced for OC users.

## Licensing

TBD. For now, consider it "all rights reserved" - or just ask on IRC.

## Acknowledgements

* 8086tiny, for providing the initial motivation and being one of the emulators which proved helpful during 
development
* Publicly available 8086 test suites used:
    * 80186_tests.zip
    * [riapyx's test suite](https://github.com/ynben/riapyx) - incomplete, but helpful nonetheless
