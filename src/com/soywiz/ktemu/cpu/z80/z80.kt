package com.soywiz.ktemu.cpu.z80

import com.soywiz.ktemu.common.exec
import com.soywiz.ktemu.common.hasAll
import com.soywiz.ktemu.common.noImpl
import com.soywiz.ktemu.common.setResetBits

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

interface Z80IO {
	operator fun get(offset: Int): Int
	operator fun set(offset: Int, value: Int): Unit
}

class Z80State(val ports: Z80IO) {
	val m = Z80Mem()
	val M8 = Z80ByteView(m)

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

	var AF: Int get() = PACK16(A, F); set(value) = exec { A = HIGH8(value); F = LOW8(value) }
	var AF_: Int get() = PACK16(A_, F_); set(value) = exec { A_ = HIGH8(value); F_ = LOW8(value) }
	var BC: Int get() = PACK16(B, C); set(value) = exec { B = HIGH8(value); C = LOW8(value) }
	var DE: Int get() = PACK16(D, E); set(value) = exec { D = HIGH8(value); E = LOW8(value) }
	var HL: Int get() = PACK16(H, L); set(value) = exec { H = HIGH8(value); L = LOW8(value) }
	var HL_: Int get() = PACK16(H_, L_); set(value) = exec { H_ = HIGH8(value); L_ = LOW8(value) }

	var rBC: Int get() = M8[BC]; set(value) = exec { M8[BC] = value }
	var rDE: Int get() = M8[DE]; set(value) = exec { M8[DE] = value }
	var rHL: Int get() = M8[HL]; set(value) = exec { M8[HL] = value }

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


	fun l8(): Int {
		val value = m.l8(PC)
		PC++
		return value
	}

	fun l16(): Int {
		val value = m.l16(PC)
		PC += 2
		return value
	}

	var flag_s: Boolean get() = F hasAll FLAG_S; set(value) {
		F = F.setResetBits(FLAG_S, value)
	}

	var flag_z: Boolean get() = F hasAll FLAG_Z; set(value) {
		F = F.setResetBits(FLAG_Z, value)
	}

	var flag_h: Boolean get() = F hasAll FLAG_H; set(value) {
		F = F.setResetBits(FLAG_H, value)
	}

	fun LD(v: Int, tstates: Int): Int {
		this.tstates += tstates
		return v
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
		//{ z80.f = ( z80.f & 0x01 ) | ( (z80.b)&0x0f ? 0 : 0x10 ) | 0x02; (z80.b) = ((z80.b) - 1) & 0xff; z80.f |= ( (z80.b)==0x7f ? 0x04 : 0 ) | sz53_table[z80.b];};
		return v - 1
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
		val result = a and b
		F = AND_TABLE[result]
		return result
	}
}

class Z80Mem {
	private val data = ByteArray(0x10000)

	fun l8(address: Int): Int = (data[address].toInt()) and 0xFF
	fun l16(address: Int): Int = ((l8(address + 0) shl 0) or (l8(address + 1) shl 8)) and 0xFFFF
	fun s8(address: Int, value: Int) {
		data[address.toInt()] = value.toByte()
	}
}

class Z80ByteView(val m: Z80Mem) {
	operator fun get(offset: Int): Int = m.l8(offset)
	operator fun set(offset: Int, value: Int): Unit {
		m.s8(offset, value)
	}
}

fun ROT8_L1(v: Int): Int {
	return ((v shl 1) and 0xFF) or (v ushr 7)
}

fun Z80State.decode() {
	val op = l8()
	when (op.toInt()) {
		0x00 -> {
			// 0x00 NOP
		}
		0x01 -> BC = LD(l16(), 10)
		0x02 -> rBC = LD(A, 7) // LD (BC),A
		0x06 -> B = LD(l8(), 7) // 0x06 LD B,nn

	// INC 8 bits
		0x04 -> B = INC(B, 4) // INC B
		0x0c -> C = INC(C, 4) // INC C
		0x14 -> D = INC(D, 4) // INC D
		0x1c -> E = INC(E, 4) // INC E
		0x24 -> H = INC(H, 4) // INC H
		0x2c -> L = INC(L, 4) // INC L
		0x34 -> rHL = INC(rHL, 7) // INC (HL)
		0x3c -> A = INC(A, 4)

	// DEC 8 bits
		0x05 -> B = DEC(B, 4) // DEC B
		0x0d -> C = DEC(C, 4) // DEC C
		0x15 -> D = DEC(D, 4) // DEC D
		0x1d -> E = DEC(E, 4) // DEC E
		0x25 -> H = DEC(H, 4) // DEC H
		0x2d -> L = DEC(L, 4) // DEC L
		0x3d -> A = DEC(A, 4) // DEC A

	// INC 16 bits
		0x03 -> BC = INC16(BC, 6) // INC BC
		0x13 -> DE = INC16(DE, 6) // INC DE
		0x23 -> HL = INC16(HL, 6) // INC HL
		0x33 -> SP = INC16(SP, 6) // INC SP


		0x07 -> {
			// 0x07 RLCA
			A = ROT8_L1(A)
			F = (F and (0x04 or 0x40 or 0x80)) or (A and (0x01 or 0x08 or 0x20))
		}
		0x08 -> {
			// 0x08 EX AF,AF'
			val old = AF
			AF = AF_
			AF_ = old
		}
		0x09 -> HL = ADD16(HL, BC, 11)
		0x0a -> A = READ8(BC, 7)
		0x0b -> BC = DEC16(BC, 6)
		0x0e -> C = LD(l8(), 7) // 0x0e LD C,nn
		0x0f -> {
			noImpl
			//0x0f RRCA
			//z80.f = ( z80.f & ( 0x04 | 0x40 | 0x80 ) ) | ( z80.a & 0x01 );
			//z80.a = ( z80.a >> 1) | ( (z80.a & 0x01) << 7 );
			//z80.f |= ( z80.a & ( 0x08 | 0x20 ) );
		}
		0x11 -> DE = LD(l16(), 10)
		0x12 -> rDE = LD(A, 7) // LD (DE),A

		// AND A, op
		0xa0 -> A = AND(A, B, 4) // AND B
		0xa1 -> A = AND(A, C, 4) // AND C
		0xa2 -> A = AND(A, D, 4) // AND D
		0xa3 -> A = AND(A, E, 4) // AND E
		0xa4 -> A = AND(A, H, 4) // AND H
		0xa5 -> A = AND(A, L, 4) // AND L
		0xa6 -> A = AND(A, rHL, 7) // AND (HL)
		0xa7 -> A = AND(A, A, 4) // AND A
		0xe6 -> A = AND(A, l8(), 7) // AND nn

	}
}


/*


0x10 DJNZ offset
0x16 LD D,nn
0x17 RLA
0x18 JR offset
0x19 ADD HL,DE
0x1a LD A,(DE)
0x1b DEC DE
0x1e LD E,nn
0x1f RRA
0x20 JR NZ,offset
0x21 LD HL,nnnn
0x22 LD (nnnn),HL
0x26 LD H,nn
0x27 DAA
0x28 JR Z,offset
0x29 ADD HL,HL
0x2a LD HL,(nnnn)
0x2b DEC HL
0x2e LD L,nn
0x2f CPL
0x30 JR NC,offset
0x31 LD SP,nnnn
0x32 LD (nnnn),A
0x35 DEC (HL)
0x36 LD (HL),nn
0x37 SCF
0x38 JR C,offset
0x39 ADD HL,SP
0x3a LD A,(nnnn)
0x3b DEC SP
0x3e LD A,nn
0x3f CCF
0x40 LD B,B
0x41 LD B,C
0x42 LD B,D
0x43 LD B,E
0x44 LD B,H
0x45 LD B,L
0x46 LD B,(HL)
0x47 LD B,A
0x48 LD C,B
0x49 LD C,C
0x4a LD C,D
0x4b LD C,E
0x4c LD C,H
0x4d LD C,L
0x4e LD C,(HL)
0x4f LD C,A
0x50 LD D,B
0x51 LD D,C
0x52 LD D,D
0x53 LD D,E
0x54 LD D,H
0x55 LD D,L
0x56 LD D,(HL)
0x57 LD D,A
0x58 LD E,B
0x59 LD E,C
0x5a LD E,D
0x5b LD E,E
0x5c LD E,H
0x5d LD E,L
0x5e LD E,(HL)
0x5f LD E,A
0x60 LD H,B
0x61 LD H,C
0x62 LD H,D
0x63 LD H,E
0x64 LD H,H
0x65 LD H,L
0x66 LD H,(HL)
0x67 LD H,A
0x68 LD L,B
0x69 LD L,C
0x6a LD L,D
0x6b LD L,E
0x6c LD L,H
0x6d LD L,L
0x6e LD L,(HL)
0x6f LD L,A
0x70 LD (HL),B
0x71 LD (HL),C
0x72 LD (HL),D
0x73 LD (HL),E
0x74 LD (HL),H
0x75 LD (HL),L
0x76 HALT
0x77 LD (HL),A
0x78 LD A,B
0x79 LD A,C
0x7a LD A,D
0x7b LD A,E
0x7c LD A,H
0x7d LD A,L
0x7e LD A,(HL)
0x7f LD A,A
0x80 ADD A,B
0x81 ADD A,C
0x82 ADD A,D
0x83 ADD A,E
0x84 ADD A,H
0x85 ADD A,L
0x86 ADD A,(HL)
0x87 ADD A,A
0x88 ADC A,B
0x89 ADC A,C
0x8a ADC A,D
0x8b ADC A,E
0x8c ADC A,H
0x8d ADC A,L
0x8e ADC A,(HL)
0x8f ADC A,A
0x90 SUB A,B
0x91 SUB A,C
0x92 SUB A,D
0x93 SUB A,E
0x94 SUB A,H
0x95 SUB A,L
0x96 SUB A,(HL)
0x97 SUB A,A
0x98 SBC A,B
0x99 SBC A,C
0x9a SBC A,D
0x9b SBC A,E
0x9c SBC A,H
0x9d SBC A,L
0x9e SBC A,(HL)
0x9f SBC A,A
0xa8 XOR A,B
0xa9 XOR A,C
0xaa XOR A,D
0xab XOR A,E
0xac XOR A,H
0xad XOR A,L
0xae XOR A,(HL)
0xaf XOR A,A
0xb0 OR A,B
0xb1 OR A,C
0xb2 OR A,D
0xb3 OR A,E
0xb4 OR A,H
0xb5 OR A,L
0xb6 OR A,(HL)
0xb7 OR A,A
0xb8 CP B
0xb9 CP C
0xba CP D
0xbb CP E
0xbc CP H
0xbd CP L
0xbe CP (HL)
0xbf CP A
0xc0 RET NZ
0xc1 POP BC
0xc2 JP NZ,nnnn
0xc3 JP nnnn
0xc4 CALL NZ,nnnn
0xc5 PUSH BC
0xc6 ADD A,nn
0xc7 RST 00
0xc8 RET Z
0xc9 RET
0xca JP Z,nnnn
0xcb shift CB
0xcc CALL Z,nnnn
0xcd CALL nnnn
0xce ADC A,nn
0xcf RST 8
0xd0 RET NC
0xd1 POP DE
0xd2 JP NC,nnnn
0xd3 OUT (nn),A
0xd4 CALL NC,nnnn
0xd5 PUSH DE
0xd6 SUB nn
0xd7 RST 10
0xd8 RET C
0xd9 EXX
0xda JP C,nnnn
0xdb IN A,(nn)
0xdc CALL C,nnnn
0xdd shift DD
0xde SBC A,nn
0xdf RST 18
0xe0 RET PO
0xe1 POP HL
0xe2 JP PO,nnnn
0xe3 EX (SP),HL
0xe4 CALL PO,nnnn
0xe5 PUSH HL
0xe7 RST 20
0xe8 RET PE
0xe9 JP HL
0xea JP PE,nnnn
0xeb EX DE,HL
0xec CALL PE,nnnn
0xed shift ED
0xee XOR A,nn
0xef RST 28
0xf0 RET P
0xf1 POP AF
0xf2 JP P,nnnn
0xf3 DI
0xf4 CALL P,nnnn
0xf5 PUSH AF
0xf6 OR nn
0xf7 RST 30
0xf8 RET M
0xf9 LD SP,HL
0xfa JP M,nnnn
0xfb EI
0xfc CALL M,nnnn
0xfd shift FD
0xfe CP nn
0xff RST 38
*/