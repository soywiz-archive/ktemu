package com.soywiz.ktemu.system.sms

import com.soywiz.ktemu.cpu.z80.Z80IO
import com.soywiz.ktemu.system.sms.SmsVDP
import com.soywiz.ktemu.system.sms.SmsInput
import com.soywiz.ktemu.system.sms.SmsSPU

class SmsPorts(private val vdp: SmsVDP, private val input: SmsInput, private val spu: SmsSPU) : Z80IO {
	var inputMode: Int = 7

	override fun get(addr: Int): Int {
		//println(":: PORT: %04X".format(addr))

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
		//println(":: PORT: %04X <- %02X".format(addr, value))
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
