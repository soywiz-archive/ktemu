package com.soywiz.ktemu.system

import com.soywiz.ktemu.cpu.z80.Z80IO
import com.soywiz.ktemu.gpu.sms.SmsVDP
import com.soywiz.ktemu.input.sms.SmsInput
import com.soywiz.ktemu.spu.sms.SmsSPU

class SmsPorts(private val vdp: SmsVDP, private val input: SmsInput, private val spu: SmsSPU) : Z80IO {
	var inputMode: Int = 0

	override fun get(addr: Int): Int {
		return when (addr) {
			0x7e -> vdp.getLine();
			0x7f -> vdp.getX();
			0xdc, 0xc0 -> (input.joystick ushr 0) and 0xff;
			0xdd, 0xc1 -> (input.joystick ushr 8) and 0xff;
			0xbe -> vdp.readByte();
			0xbd, 0xbf -> vdp.readStatus()
			0xde -> 0xff // if we ever support keyboard: return inputMode;
			0xdf -> 0xff // Unknown use
			0xf2 -> 0 // YM2413
			else -> {
				println("Unknown IO port %02X".format(addr))
				0xff
			}
		}
	}

	override fun set(addr: Int, value: Int) {
		when (addr) {
			0x3f -> {
				var natbit = (( value ushr 5) and 1);
				if (( value and 1) === 0) natbit = 1;
				input.joystick = (input.joystick and (1 shl 14).inv()) or (natbit shl 14);
				natbit = (( value ushr 7) and 1);
				if (( value and 4) === 0) natbit = 1;
				input.joystick = (input.joystick and (1 shl 15).inv()) or (natbit shl 15);
			}
			0x7e, 0x7f -> {
				spu.poke(value)
			}
			0xbd, 0xbf -> vdp.writeaddr(value)
			0xbe -> vdp.writebyte(value)
			0xde -> inputMode = value;
			0xdf -> Unit // Unknown use
			0xf0, 0xf1, 0xf2 -> {
				// YM2413 sound support: TODO

			}
			0x3e -> {
				// enable/disable of RAM and stuff, ignore

			}
			else -> {
				println("Unknown IO port %02X = %02X".format(addr, value))
			}
		}
	}
}
