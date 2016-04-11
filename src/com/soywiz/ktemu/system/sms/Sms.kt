package com.soywiz.ktemu.system.sms

import com.soywiz.ktemu.cpu.z80.Z80State
import com.soywiz.ktemu.cpu.z80.execOne

// Sega Master System
class Sms {
	companion object {
		const val framesPerSecond = 50;
		const val scanLinesPerFrame = 313; // 313 lines in PAL TODO: unify all this
		const val scanLinesPerSecond = scanLinesPerFrame * framesPerSecond;
		const val cpuHz = (3.58 * 1000 * 1000).toInt(); // According to Sega docs.
		const val tstatesPerHblank = cpuHz / scanLinesPerSecond
	}

	val vdp = SmsVDP()
	val spu = SmsSPU()
	val input = SmsInput()
	val ports = SmsPorts(vdp, input, spu)
	val mem = SmsMem()
	val cpu = Z80State(ports, mem)

	fun loadRom(data: ByteArray) {
		mem.loadRom(data)
	}

	fun execStep() {
		if (cpu.tstates >= tstatesPerHblank) {
			cpu.tstates -= tstatesPerHblank
			vdp.hblank()
			cpu.interrupt()
		}
		cpu.execOne()
	}
}

/*
- Z80 CPU running at about 3.3MHz (53.203MHz/16).  [This value is from Marat's
  document, though I suspect the real value is closer to 4MHz]



- Video information held in 16k of VRAM, not in the Z80 memory map, but
  accessed via input/output ports.

- Generic SN76489 sound chip (PSG) made by Texas Instruments, providing:
    - 3 square-wave sound channels
    - 1 white noise/periodic noise channel
  It's actually the same sound chip as in the good ol' BBC Micro...

- Memory paging registers accessed through main RAM at $FFFC..$FFFF.
  Other input/output implemented via standard Z80 I/O ports.

- Yamaha YM2413 FM sound chip - this was present in some of the Japanese Mk 3
  machines and, apparently, also in *some* European machines.  It provides:
    - 9 channel synthesised sound (9 pure voices, or 6 pure + 3 percussion)
    - 15 instruments + 1 user definable
    - Hardware vibrato / amplitude modulation


 */