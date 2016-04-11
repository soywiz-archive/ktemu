package com.soywiz.ktemu.system.sms

import com.soywiz.ktemu.common.invalidOp
import com.soywiz.ktemu.common.noImpl
import com.soywiz.ktemu.cpu.z80.PACK16
import com.soywiz.ktemu.cpu.z80.Z80IO

class SmsMem : Z80IO {
	var romPageMask = 1
	var numRomBanks = 0
	var ramSelectRegister = 0
	val ram = ByteArray(8 * 1024) // 8k

	var page0 = 0
	var page1 = 1
	var page2 = 2
	var rom = ByteArray(0)

	fun loadRom(data: ByteArray) {
		this.rom = data
		this.numRomBanks = data.size / ROM_BANK_SIZE
		romPageMask = (numRomBanks - 1)

		println("Loading ROM of: $numRomBanks banks, mask: $romPageMask, serial: " + "%04X".format(getSerial()))
	}

	fun getSerial():Int {
		return PACK16(rom(1, 0x3ffc), rom(1, 0x3ffd))
	}

	fun rom(bank:Int, address: Int): Int {
		val offset = bank * ROM_BANK_SIZE + address
		return if (offset < rom.size) (rom[offset].toInt() and 0xFF) else 0
	}

	fun ram(address: Int): Int {
		return ram[address].toInt() and 0xFF
	}

	fun cartridgeRam(address:Int):Int {
		return 0x00
	}

	override operator fun get(_address:Int):Int {
		val address = _address and 0x3fff
		return when (_address) {
			in 0x0000 .. 0x03FF -> rom(0, address) // First 1k of ROM Bank 0, never paged out with rest of Page 0
			in 0x0400 .. 0x3FFF -> rom(page0, address) // 15k ROM Page 0
			in 0x4000 .. 0x7FFF -> rom(page1, address) // 16k ROM Page 1
			in 0x8000 .. 0xBFFF ->// 16k ROM Page 2, or one of two pages of Cartridge RAM
				when (ramSelectRegister and 12) {
					8 -> cartridgeRam(address + 0x0000)
					12 -> cartridgeRam(address + 0x4000)
					else -> rom(page2, address)
				}
			in 0xC000 .. 0xDFFF -> ram(address and 0x1fff) // 8k of on-board RAM (mirrored at $E000-$FFFF)
			in 0xE000 .. 0xFFFB -> ram(address and 0x1fff) // Mirror of RAM at $C000-$DFFF
			in 0xFFFC .. 0xFFFF -> 0x00
			else -> invalidOp
		}.toInt() and 0xFF
	}

	override operator fun set(address:Int, value:Int) {
		if (address >= 0xfffc) {
			when (address) {
				0xfffc -> ramSelectRegister = value
				0xfffd -> page0 = value and romPageMask
				0xfffe -> page1 = value and romPageMask
				0xffff -> page2 = value and romPageMask
				else -> invalidOp
			}
		} else {
			val addr = address - 0xc000
			if (addr < 0) return; // Ignore ROM writes
			ram[addr and 0x1fff] = value.toByte()
		}
	}

	companion object {
		const val ROM_BANK_SIZE = 0x4000
	}
}

