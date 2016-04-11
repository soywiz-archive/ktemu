package com.soywiz.ktemu.cpu.z80

import com.soywiz.ktemu.common.*

const val FLAG_C = 0x01
const val FLAG_N = 0x02
const val FLAG_P = 0x04
const val FLAG_V = FLAG_P
const val FLAG_3 = 0x08
const val FLAG_H = 0x10
const val FLAG_5 = 0x20
const val FLAG_Z = 0x40
const val FLAG_S = 0x80


fun LOW8(v: Int): Int = ((v ushr 0) and 0xFF)
fun HIGH8(v: Int): Int = ((v ushr 8) and 0xFF)
fun PACK16(H: Int, L: Int): Int = (H shl 8) or (L shl 0)

class Z80IOArray(val data: ByteArray) : Z80IO {
	override operator fun get(offset: Int): Int = data[offset].toInt() and 0xFF
	override operator fun set(offset: Int, value: Int): Unit = exec { data[offset] = value.toByte() }
}

interface Z80IO {
	operator fun get(offset: Int): Int
	operator fun set(offset: Int, value: Int): Unit
}

// http://www.cantrell.org.uk/david/tech/cpc/cpc-firmware/z80index.pl
class Z80State(val ports: Z80IO, val memory: Z80IO) {
	val M8 = Z80ByteView(memory)
	val M16 = Z80ShortView(memory)

	var tstates: Int = 0 // clock pulses

	var PC: Int = 0; get() = field; set(value) = exec { field = value and 0xFFFF }
	var SP: Int = 0; get() = field; set(value) = exec { field = value and 0xFFFF }
	var A: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var B: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var C: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var D: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var E: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var F: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var H: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var L: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var A_: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var F_: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var H_: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var L_: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }

	var PCHL: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var PCHH: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }

	var IXL: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var IXH: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }

	var IYL: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }
	var IYH: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }


	// ?
	var R: Int = 0; get() = field; set(value) = exec { field = value and 0xFF }

	// Interrupt Flags
	var iff1 = false
	var iff2 = false

	// Interrupt Mode
	var im: Int = 0

	var AF: Int get() = PACK16(A, F); set(value) = exec { A = HIGH8(value); F = LOW8(value) }
	var AF_: Int get() = PACK16(A_, F_); set(value) = exec { A_ = HIGH8(value); F_ = LOW8(value) }
	var BC: Int get() = PACK16(B, C); set(value) = exec { B = HIGH8(value); C = LOW8(value) } // ByteCounter
	var DE: Int get() = PACK16(D, E); set(value) = exec { D = HIGH8(value); E = LOW8(value) }
	var HL: Int get() = PACK16(H, L); set(value) = exec { H = HIGH8(value); L = LOW8(value) }
	var HL_: Int get() = PACK16(H_, L_); set(value) = exec { H_ = HIGH8(value); L_ = LOW8(value) }
	var PCH: Int get() = PACK16(PCHH, PCHL); set(value) = exec { PCHH = HIGH8(value); PCHL = LOW8(value) }

	var IX: Int get() = PACK16(IXH, IXL); set(value) = exec { IXH = HIGH8(value); IXL = LOW8(value) }
	var IY: Int get() = PACK16(IYH, IYL); set(value) = exec { IYH = HIGH8(value); IYL = LOW8(value) }

	var regkind = false

	var IXY: Int get() = if (regkind) IX else IY; set(value) = exec { if (regkind) IX = value else IY = value }

	var rBC: Int get() = M8[BC]; set(value) = exec { M8[BC] = value }
	var rDE: Int get() = M8[DE]; set(value) = exec { M8[DE] = value }
	var rHL: Int get() = M8[HL]; set(value) = exec { M8[HL] = value }
	var rSP16: Int get() = M16[SP]; set(value) = exec { M16[SP] = value }


	fun HALT(tstates: Int) {
		this.tstates += tstates
	}

	fun OUT(index: Int, value: Int, tstates: Int) {
		this.tstates += tstates
		this.ports[index] = value
	}

	fun IN(value: Int, index: Int, tstates: Int): Int {
		this.tstates += tstates
		return this.ports[index]
	}

	companion object {
		/*
		val parity_table = (0x00 .. 0xFF).map { if ((it % 2) == 0) FLAG_P else 0 }.toIntArray()

		val sz53_table = (0x00 .. 0xFF).map { it and ( FLAG_3 or FLAG_5 or FLAG_S ) }.toIntArray()

		val sz53p_table = (0x00 .. 0xFF).map {
			var result = 0

			sz53_table[i]= i & ( 0x08 | 0x20 | 0x80 );
			j=i; parity=0;
			for(k=0;k<8;k++) { parity ^= j & 1; j >>=1; }
			parity_table[i]= ( parity ? 0 : 0x04 );
			sz53p_table[i] = sz53_table[i] | parity_table[i];

			sz53_table[0] |= 0x40;
			sz53p_table[0] |= 0x40;

			result
		}.toIntArray()
		*/

		val AND_TABLE = (0x00..0xFF).map {
			var result = 0
			if (result hasAll 0x80) result = result or FLAG_S
			if (result == 0) result = result or FLAG_Z
			result = result or FLAG_H
			result
		}.toIntArray()
	}

	var rnnnn: Int get() = M8[l16()]; set(value) = exec { M8[l16()] = value }
	var rnnnn16: Int get() = M16[l16()]; set(value) = exec { M16[l16()] = value }
	val nnnn: Int get() = l16()
	val nn: Int get() = l8()
	val offset: Int get() = l8().toByte().toInt()

	private fun l8(): Int {
		return M8[PC++]
	}

	private fun l16(): Int {
		val value = M16[PC]
		PC += 2
		return value
	}

	var flag_s: Boolean get() = F hasAll FLAG_S; set(value) = exec { F = F.setResetBits(FLAG_S, value) }
	var flag_z: Boolean get() = F hasAll FLAG_Z; set(value) = exec { F = F.setResetBits(FLAG_Z, value) }
	var flag_n: Boolean get() = F hasAll FLAG_N; set(value) = exec { F = F.setResetBits(FLAG_N, value) }
	var flag_c: Boolean get() = F hasAll FLAG_C; set(value) = exec { F = F.setResetBits(FLAG_C, value) }
	var flag_h: Boolean get() = F hasAll FLAG_H; set(value) = exec { F = F.setResetBits(FLAG_H, value) }
	var flag_p: Boolean get() = F hasAll FLAG_P; set(value) = exec { F = F.setResetBits(FLAG_P, value) }
	var flag_p_v: Boolean get() = F hasAll FLAG_P; set(value) = exec { F = F.setResetBits(FLAG_P, value) }
	var flag_3: Boolean get() = F hasAll FLAG_3; set(value) = exec { F = F.setResetBits(FLAG_3, value) }
	var flag_5: Boolean get() = F hasAll FLAG_5; set(value) = exec { F = F.setResetBits(FLAG_5, value) }

	fun LD(v: Int, tstates: Int): Int {
		this.tstates += tstates
		return v
	}

	fun CP(a: Int, b: Int, tstates: Int): Int {
		this.tstates += tstates

		val cptemp = a - b
		val overflow = cptemp.toByte().toInt() != cptemp // @TODO: Check!

		//var lookup = ((a and 0x88) ushr 3) or (((b) and 0x88) ushr 2) or ((cptemp and 0x88 ) ushr 1);
		//F = ( if((cptemp and 0x100) != 0) 0x01 else ( if (cptemp != 0) 0 else 0x40 ) ) or 0x02 or halfcarry_sub_table[lookup and 0x07] or overflow_sub_table[lookup ushr 4] or ( b and ( 0x08 or 0x20 ) ) or ( cptemp and 0x80 );}

		flag_s = (cptemp < 0) //S is set if result is negative; reset otherwise
		flag_z = (cptemp == 0) //Z is set if result is zero; reset otherwise
		flag_h = (cptemp and 0x08) != 0 //H is set if borrow from bit 4; reset otherwise
		flag_p = overflow //P/V is set if overflow; reset otherwise
		flag_n = true //N is set
		flag_c = (cptemp and 0x08) != 0 //C is set if borrow; reset otherwise

		return a
	}

	fun JP(absoluteAddr: Int, tstates: Int) {
		this.tstates += tstates
		PC = absoluteAddr
	}

	fun JRr(cond: Boolean, offset: Int, tstates: Int) {
		JRa(cond, PC + offset + 1, tstates)
	}

	fun JRa(cond: Boolean, absoluteAddr: Int, tstates: Int) {
		this.tstates += tstates
		if (cond) PC = absoluteAddr
	}

	// RESET
	fun RST(address: Int, tstates: Int) {
		PUSH16(PC, tstates)
		PC = address
	}

	fun CALL(address: Int, tstates: Int) {
		PUSH16(PC, tstates)
		PC = address
	}


	fun PUSH16(value: Int, tstates: Int) {
		this.tstates += tstates
		SP -= 2
		M16[SP] = value
	}

	fun POP16(tstates: Int): Int {
		this.tstates += tstates
		val ret = M16[SP]
		SP += 2
		return ret
	}

	// @TODO:
	fun INC(v: Int, tstates: Int): Int {
		this.tstates += tstates
		//{ (z80.b) = ((z80.b) + 1) & 0xff; z80.f = ( z80.f & 0x01 ) | ( (z80.b)==0x80 ? 0x04 : 0 ) | ( (z80.b)&0x0f ? 0 : 0x10 ) | sz53_table[(z80.b)];};
		return v + 1
	}

	// @TODO:
	fun DEC(v: Int, tstates: Int): Int {
		this.tstates += tstates
		val result = v - 1

		/*
		const val FLAG_C = 0x01
		const val FLAG_N = 0x02
		const val FLAG_P = 0x04
		const val FLAG_V = FLAG_P
		const val FLAG_3 = 0x08
		const val FLAG_H = 0x10
		const val FLAG_5 = 0x20
		const val FLAG_Z = 0x40
		const val FLAG_S = 0x80
		*/

		//flag_c = true
		flag_c = flag_c //C is not affected
		flag_n = false //N is reset
		flag_p_v = (v == 0x7F)
		flag_h = (v and 0xF) != 0
		flag_z = result == 0
		flag_s = result < 0

		//S is set if result is negative; reset otherwise
		//H is set if carry from bit 3; reset otherwise



		//{ z80.f = ( z80.f & FLAG_C ) | ( (z80.b)&0x0f ? 0 : FLAG_H ) | 0x02; (z80.b) = ((z80.b) - 1) & 0xff; z80.f |= ( (z80.b)==0x7f ? 0x04 : 0 ) | sz53_table[z80.b];};
		return result and 0xFF
	}

	// @TODO:
	fun DEC16(v: Int, tstates: Int): Int {
		this.tstates += tstates
		return v - 1
	}

	fun INC16(v: Int, tstates: Int): Int {
		this.tstates += tstates
		return v + 1
	}

	// @TODO:
	fun ADD16(a: Int, b: Int, tstates: Int): Int {
		this.tstates += tstates
		//{ var add16temp = ((z80.l | (z80.h << 8))) + ((z80.c | (z80.b << 8))); var lookup = ( ( ((z80.l | (z80.h << 8))) & 0x0800 ) >> 11 ) | ( ( ((z80.c | (z80.b << 8))) & 0x0800 ) >> 10 ) | ( ( add16temp & 0x0800 ) >> 9 ); tstates += 7; (z80.h) = (add16temp >> 8) & 0xff; (z80.l) = add16temp & 0xff; z80.f = ( z80.f & ( 0x04 | 0x40 | 0x80 ) ) | ( add16temp & 0x10000 ? 0x01 : 0 )| ( ( add16temp >> 8 ) & ( 0x08 | 0x20 ) ) | halfcarry_add_table[lookup];};
		return a + b
	}

	fun READ8(offset: Int, tstates: Int): Int {
		this.tstates += tstates
		return M8[offset]
	}

	fun WRITE8(offset: Int, value: Int, tstates: Int) {
		this.tstates += tstates
		M8[offset] = value
	}

	fun AND(a: Int, b: Int, tstates: Int): Int {
		//AND r 1 4 1.00
		//AND n 2 7 (4, 3) 1.75
		//AND (HL) 2 7 (4, 3) 1.75
		//AND (IX+d) 5 19 (4, 4, 3, 5, 3) 4.75
		//AND (IX+d) 5 19 (4, 4, 3. 5, 3) 4.75
		this.tstates += tstates
		val result = (a and b) and 0xFF
		F = AND_TABLE[result]
		return result
	}

	fun XOR(a: Int, b: Int, tstates: Int): Int {
		this.tstates += tstates
		val result = (a xor b) and 0xFF
		F = AND_TABLE[result]
		return result
	}

	fun OR(a: Int, b: Int, tstates: Int): Int {
		this.tstates += tstates
		val result = (a or b) and 0xFF
		F = AND_TABLE[result]
		return result
	}

	fun SUB(a: Int, b: Int, tstates: Int): Int {
		this.tstates += tstates
		val result = (a - b) and 0xFF
		F = AND_TABLE[result]
		return result
	}

	fun SBC(a: Int, b: Int, tstates: Int): Int {
		this.tstates += tstates
		val result = (a - b) and 0xFF
		F = AND_TABLE[result]
		return result
	}

	fun ADD(a: Int, b: Int, tstates: Int): Int {
		this.tstates += tstates
		val result = (a + b) and 0xFF
		F = AND_TABLE[result]

		var overflow = (a < 0x7F == b < 0x7F) && (a < 0x7F == result < 0); // if both operands are positive and result is negative or if both are negative and result is positive

		return result
	}

	fun ADC(a: Int, b: Int, tstates: Int): Int {
		this.tstates += tstates
		val result = (a + b) and 0xFF
		F = AND_TABLE[result]
		return result
	}

	fun BIT(bit: Int, reg: Int, tstates: Int) {
		//  z80.f = ( z80.f & FLAG_C ) | FLAG_H | ( z80.b & ( FLAG_3 | FLAG_5 ) );
		this.tstates += tstates
		flag_h = true
		flag_p = (reg and (1 shl bit)) == 0
		flag_z = (reg and (1 shl bit)) == 0
		flag_s = ( bit == 7 && ((reg and 0x80) != 0))
	}

	fun SLL(value: Int, tstates: Int): Int {
		this.tstates += tstates
		return (value shl 1) or 1
	}

	fun NOP(tstates: Int) {
		this.tstates += tstates
	}

	fun RET(cond: Boolean, tstates: Int) {
		this.tstates += tstates
		if (cond) PC = POP16(0)
	}

	fun interrupt() {
		if (!iff1) return

		//if( z80.halted ) { PC++; halted = false; }

		iff1 = false
		iff2 = false

		PUSH16(PCH, 0)

		R = (R + 1) and 0x7f; /* rzx_instructions_offset--; */

		when (im) {
			0 -> {
				PC = 0x0038; tstates += 12;
			}
			1 -> {
				PC = 0x0038; tstates += 13;
			}
			2 -> {
				/*
				var inttemp=(0x100*I)+0xff;
				var pcl = readbyte(inttemp++);
				inttemp &= 0xfff;
				var pch = readbyte(inttemp);
				PC = pcl or (pch shl 8);
				tstates+=19;
				*/
				noImpl("im: $im")
			}
			else -> invalidOp("Unknown interrupt mode $im")
		}
	}
}

class Z80ByteView(val m: Z80IO) {
	operator fun get(offset: Int): Int = m[offset]
	operator fun set(offset: Int, value: Int) = exec { m[offset] = value }
}

class Z80ShortView(val m: Z80IO) {
	operator fun get(offset: Int): Int = PACK16(m[offset + 1], m[offset + 0])
	operator fun set(offset: Int, value: Int) = exec { m[offset + 1] = HIGH8(value); m[offset + 0] = LOW8(value) }
}

fun ROT8_L1(v: Int): Int {
	return ((v shl 1) and 0xFF) or (v ushr 7)
}


/*
class Delegate {
	inline operator fun getValue(thisRef: Any?, property: KProperty<*>): String {
		return "$thisRef, thank you for delegating '${property.name}' to me!"
	}

	inline operator fun setValue(thisRef: Any?, property: KProperty<*>, value: String) {
		println("$value has been assigned to '${property.name} in $thisRef.'")
	}
}
*/

