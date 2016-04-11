package com.soywiz.ktemu.cpu.z80

import com.soywiz.ktemu.common.noImpl

fun Z80State.execOne() {
	val op = nn
	when (op.toInt()) {
		0x00 -> NOP(4) // NOP
		0x01 -> BC = LD(nnnn, 4 + 6)
		0x02 -> rBC = LD(A, 4 + 3) // LD (BC), A
		0x06 -> B = LD(nn, 4 + 3) // LD B, nn
		0x0a -> A = LD(rBC, 7) // LD A, (BC)
		0x0e -> C = LD(nn, 7)  // LD C, nn

	// INC op
		0x04 -> B = INC(B, 4) // INC B
		0x0c -> C = INC(C, 4) // INC C
		0x14 -> D = INC(D, 4) // INC D
		0x1c -> E = INC(E, 4) // INC E
		0x24 -> H = INC(H, 4) // INC H
		0x2c -> L = INC(L, 4) // INC L
		0x34 -> rHL = INC(rHL, 4 + 7) // INC (HL)
		0x3c -> A = INC(A, 4)

	// DEC op
		0x05 -> B = DEC(B, 4) // DEC B
		0x0d -> C = DEC(C, 4) // DEC C
		0x15 -> D = DEC(D, 4) // DEC D
		0x1d -> E = DEC(E, 4) // DEC E
		0x25 -> H = DEC(H, 4) // DEC H
		0x2d -> L = DEC(L, 4) // DEC L
		0x35 -> rHL = DEC(rHL, 4 + 7) // DEC (HL)
		0x3d -> A = DEC(A, 4) // DEC A

	// INC op16
		0x03 -> BC = INC16(BC, 4 + 2) // INC BC
		0x13 -> DE = INC16(DE, 4 + 2) // INC DE
		0x23 -> HL = INC16(HL, 4 + 2) // INC HL
		0x33 -> SP = INC16(SP, 4 + 2) // INC SP

	// DEC op16
		0x0b -> BC = DEC16(BC, 4 + 2) // DEC BC
		0x1b -> DE = DEC16(DE, 4 + 2) // DEC DE
		0x2b -> HL = DEC16(HL, 4 + 2) // DEC HL
		0x3b -> SP = DEC16(SP, 4 + 2) // DEC SP

	// ADD HL, op16
		0x09 -> HL = ADD16(HL, BC, 4 + 7) // ADD HL, BC
		0x19 -> HL = ADD16(HL, DE, 4 + 7) // ADD HL, DE
		0x29 -> HL = ADD16(HL, HL, 4 + 7) // ADD HL, HL
		0x39 -> HL = ADD16(HL, SP, 4 + 7) // ADD HL, SP

		0x07 -> {
			// 0x07 RLCA
			A = ROT8_L1(A)
			F = (F and (0x04 or 0x40 or 0x80)) or (A and (0x01 or 0x08 or 0x20))
		}
		0x08 -> {
			// EX AF,AF'
			val old = AF
			AF = AF_
			AF_ = old
		}
		0xe3 -> {
			// EX (SP),HL
			val temp = rSP16
			rSP16 = HL
			HL = temp
		}
		0x0f -> {
			noImpl
			//0x0f RRCA
			//z80.f = ( z80.f & ( 0x04 | 0x40 | 0x80 ) ) | ( z80.a & 0x01 );
			//z80.a = ( z80.a >> 1) | ( (z80.a & 0x01) << 7 );
			//z80.f |= ( z80.a & ( 0x08 | 0x20 ) );
		}

	// LD
		0x11 -> DE = LD(nnnn, 10) // LD DE,nnnn
		0x12 -> rDE = LD(A, 7) // LD (DE),A
		0x16 -> D = LD(nn, 7)
		0x1a -> A = LD(rDE, 7)
		0x1e -> E = LD(nn, 7)
		0x21 -> HL = LD(nnnn, 4 + 6)
		0x22 -> rnnnn = LD(HL, 4 + 12)
		0x26 -> H = LD(nn, 4 + 3)
		0x2a -> HL = LD(rnnnn, 4 + 12)
		0x2e -> L = LD(nn, 4 + 3)
		0x31 -> SP = LD(nnnn, 4 + 6)
		0x32 -> rnnnn = LD(A, 4 + 3)
		0x36 -> rHL = LD(nn, 4 + 6)
		0x3a -> A = LD(rnnnn, 4 + 9)
		0x3e -> A = LD(nn, 4 + 3)
		0xf9 -> SP = LD(HL, 4 + 2)

	// AND A, op
		0xa0 -> A = AND(A, B, 4)
		0xa1 -> A = AND(A, C, 4)
		0xa2 -> A = AND(A, D, 4)
		0xa3 -> A = AND(A, E, 4)
		0xa4 -> A = AND(A, H, 4)
		0xa5 -> A = AND(A, L, 4)
		0xa6 -> A = AND(A, rHL, 4 + 3)
		0xa7 -> A = AND(A, A, 4)
		0xe6 -> A = AND(A, nn, 4 + 3)

	// XOR A, op
		0xa8 -> A = XOR(A, B, 4)
		0xa9 -> A = XOR(A, C, 4)
		0xaa -> A = XOR(A, D, 4)
		0xab -> A = XOR(A, E, 4)
		0xac -> A = XOR(A, H, 4)
		0xad -> A = XOR(A, L, 4)
		0xae -> A = XOR(A, rHL, 4 + 3)
		0xaf -> A = XOR(A, A, 4)
		0xee -> A = XOR(A, nn, 4 + 3)

	// OR A, op
		0xb0 -> A = OR(A, B, 4)
		0xb1 -> A = OR(A, C, 4)
		0xb2 -> A = OR(A, D, 4)
		0xb3 -> A = OR(A, E, 4)
		0xb4 -> A = OR(A, H, 4)
		0xb5 -> A = OR(A, L, 4)
		0xb6 -> A = OR(A, rHL, 4 + 3)
		0xb7 -> A = OR(A, A, 4)
		0xf6 -> A = OR(A, nn, 4 + 3)

	// SBC A, op
		0x98 -> A = SBC(A, B, 4)
		0x99 -> A = SBC(A, C, 4)
		0x9a -> A = SBC(A, D, 4)
		0x9b -> A = SBC(A, E, 4)
		0x9c -> A = SBC(A, H, 4)
		0x9d -> A = SBC(A, L, 4)
		0x9e -> A = SBC(A, rHL, 4 + 3)
		0x9f -> A = SBC(A, A, 4)
		0xde -> A = SBC(A, nn, 4 + 3)

	// ADD A, op
		0x80 -> A = ADD(A, B, 4)
		0x81 -> A = ADD(A, C, 4)
		0x82 -> A = ADD(A, D, 4)
		0x83 -> A = ADD(A, E, 4)
		0x84 -> A = ADD(A, H, 4)
		0x85 -> A = ADD(A, L, 4)
		0x86 -> A = ADD(A, rHL, 4 + 3)
		0x87 -> A = ADD(A, A, 4)
		0xc6 -> A = ADD(A, nn, 4 + 3)

	// ADC A, op
		0x88 -> A = ADC(A, B, 4)
		0x89 -> A = ADC(A, C, 4)
		0x8a -> A = ADC(A, D, 4)
		0x8b -> A = ADC(A, E, 4)
		0x8c -> A = ADC(A, H, 4)
		0x8d -> A = ADC(A, L, 4)
		0x8e -> A = ADC(A, rHL, 4 + 3)
		0x8f -> A = ADC(A, A, 4)
		0xce -> A = ADC(A, nn, 4 + 3)

	// SUB A, op
		0x90 -> A = SUB(A, B, 4)
		0x91 -> A = SUB(A, C, 4)
		0x92 -> A = SUB(A, D, 4)
		0x93 -> A = SUB(A, E, 4)
		0x94 -> A = SUB(A, H, 4)
		0x95 -> A = SUB(A, L, 4)
		0x96 -> A = SUB(A, rHL, 4 + 3)
		0x97 -> A = SUB(A, A, 4)
		0xd6 -> A = SUB(A, nn, 4 + 3)

	// LD B, op
		0x40 -> B = LD(B, 4)
		0x41 -> B = LD(C, 4)
		0x42 -> B = LD(D, 4)
		0x43 -> B = LD(E, 4)
		0x44 -> B = LD(H, 4)
		0x45 -> B = LD(L, 4)
		0x46 -> B = LD(rHL, 4 + 3)
		0x47 -> B = LD(A, 4)

	// LD C, op
		0x48 -> C = LD(B, 4)
		0x49 -> C = LD(C, 4)
		0x4a -> C = LD(D, 4)
		0x4b -> C = LD(E, 4)
		0x4c -> C = LD(H, 4)
		0x4d -> C = LD(L, 4)
		0x4e -> C = LD(rHL, 4 + 3)
		0x4f -> C = LD(A, 4)

	// LD D, op
		0x50 -> D = LD(B, 4)
		0x51 -> D = LD(C, 4)
		0x52 -> D = LD(D, 4)
		0x53 -> D = LD(E, 4)
		0x54 -> D = LD(H, 4)
		0x55 -> D = LD(L, 4)
		0x56 -> D = LD(rHL, 4 + 3)
		0x57 -> D = LD(A, 4)

	// LD E, op
		0x58 -> E = LD(B, 4)
		0x59 -> E = LD(C, 4)
		0x5a -> E = LD(D, 4)
		0x5b -> E = LD(E, 4)
		0x5c -> E = LD(H, 4)
		0x5d -> E = LD(L, 4)
		0x5e -> E = LD(rHL, 4 + 3)
		0x5f -> E = LD(A, 4)

	// LD H, op
		0x60 -> H = LD(B, 4)
		0x61 -> H = LD(C, 4)
		0x62 -> H = LD(D, 4)
		0x63 -> H = LD(E, 4)
		0x64 -> H = LD(H, 4)
		0x65 -> H = LD(L, 4)
		0x66 -> H = LD(rHL, 4 + 3)
		0x67 -> H = LD(A, 4)

	// LD L, op
		0x68 -> L = LD(B, 4)
		0x69 -> L = LD(C, 4)
		0x6a -> L = LD(D, 4)
		0x6b -> L = LD(E, 4)
		0x6c -> L = LD(H, 4)
		0x6d -> L = LD(L, 4)
		0x6e -> L = LD(rHL, 4 + 3)
		0x6f -> L = LD(A, 4)

	// LD (HL), op
		0x70 -> rHL = LD(B, 4 + 3)
		0x71 -> rHL = LD(C, 4 + 3)
		0x72 -> rHL = LD(D, 4 + 3)
		0x73 -> rHL = LD(E, 4 + 3)
		0x74 -> rHL = LD(H, 4 + 3)
		0x75 -> rHL = LD(L, 4 + 3)
		0x77 -> rHL = LD(A, 4 + 3)

	// LD A, op
		0x78 -> A = LD(B, 4)
		0x79 -> A = LD(C, 4)
		0x7a -> A = LD(D, 4)
		0x7b -> A = LD(E, 4)
		0x7c -> A = LD(H, 4)
		0x7d -> A = LD(L, 4)
		0x7e -> A = LD(rHL, 4 + 3)
		0x7f -> A = LD(A, 4)

	// CP op
		0xb8 -> CP(A, B, 4)
		0xb9 -> CP(A, C, 4)
		0xba -> CP(A, D, 4)
		0xbb -> CP(A, E, 4)
		0xbc -> CP(A, H, 4)
		0xbd -> CP(A, L, 4)
		0xbe -> CP(A, rHL, 4 + 3)
		0xbf -> CP(A, A, 4)
		0xfe -> CP(A, nn, 4 + 3)

	// SPECIAL
		0x76 -> this.HALT(4) // HALT

		0xf3 -> {
			// DI (Disable Interrupts)
			iff1 = false
			iff2 = false
		}
		0xfb -> {
			// EI (Enable Interrupts)
			iff1 = true
			iff2 = true
		}
		0xed -> {
			// 0xed shift ED
			R = (R + 1) and 0x7f;
			this.execOneED(nn)
		}

		0xcb -> {
			// 0xcb shift CB
			R = (R + 1) and 0x7f;
			this.execOneCB(nn)
		}

		0xdd -> {
			// 0xdd shift DD
			R = (R + 1) and 0x7f;
			this.execOneDDFD(nn, false)
		}

	// IN/OUT
		0xd3 -> OUT(nn, A, 4 + 4)
		0xdb -> A = IN(A, nn, 4 + 4 + 3)

	// RESET
		0xc7 -> RST(0x00, 4 + 6)
		0xcf -> RST(0x08, 4 + 6)
		0xd7 -> RST(0x10, 4 + 6)
		0xdf -> RST(0x18, 4 + 6)
		0xe7 -> RST(0x20, 4 + 6)
		0xef -> RST(0x28, 4 + 6)
		0xf7 -> RST(0x30, 4 + 6)
		0xff -> RST(0x38, 4 + 6)

	// CALL
		0xcd -> CALL(nnnn, 4 + 6)
		0xc4 -> noImpl("CALL NZ,nnnn")
		0xcc -> noImpl("CALL Z,nnnn")
		0xd4 -> noImpl("CALL NC,nnnn")
		0xdc -> noImpl("CALL C,nnnn")
		0xe4 -> noImpl("CALL PO,nnnn")
		0xec -> noImpl("CALL PE,nnnn")
		0xf4 -> noImpl("CALL P,nnnn")
		0xfc -> noImpl("CALL M,nnnn")

	// RETURN
		0xc9 -> RET(true, 4 + 5) // RET
		0xc0 -> RET(!flag_z, 4 + 1)
		0xc8 -> RET(flag_z, 4 + 1)
		0xd0 -> RET(!flag_c, 4 + 1)
		0xd8 -> RET(flag_c, 4 + 1)
		0xe0 -> RET(!flag_p, 4 + 1) // RET PO
		0xe8 -> RET(flag_p, 4 + 1) // RET PE
		0xf0 -> RET(!flag_s, 4 + 1) // RET P
		0xf8 -> RET(flag_s, 4 + 1) // RET M

		0xc3 -> JP(nnnn, 4 + 6)
		0x10 -> JRr(--B != 0, offset, 4 + 4)



	// JR k, offset
		0x20 -> JRr(!flag_z, offset, 4 + 4) // JR NZ, offset
		0x28 -> JRr(flag_z, offset, 4 + 4) // JR Z, offset
		0x30 -> JRr(!flag_c, offset, 4 + 4) // JR NC, offset
		0x38 -> JRr(flag_c, offset, 4 + 4) // JR C, offset
		0x18 -> JRr(true, offset, 4 + 4);// JR C, offset

		0xc2 -> JRa(!flag_z, nnnn, 4 + 6) // JP NZ,nnnn
		0xca -> JRa(flag_z, nnnn, 4 + 6) // JP Z,nnnn

	// PUSH 16
		0xc5 -> PUSH16(BC, 4 + 1)
		0xd5 -> PUSH16(DE, 4 + 1)
		0xe5 -> PUSH16(HL, 4 + 1)
		0xf5 -> PUSH16(AF, 4 + 1)

	// POP 16
		0xc1 -> BC = POP16(4 + 6)
		0xd1 -> DE = POP16(4 + 6)
		0xe1 -> HL = POP16(4 + 6)
		0xf1 -> AF = POP16(4 + 6)

		0x17 -> noImpl("RLA")
		0x1f -> noImpl("RRA")
		0x27 -> noImpl("DAA")
		0x2f -> {
			A = XOR(A, 0xFF, 4)
			//z80.f = ( z80.f & ( 0x01 | 0x04 | 0x40 | 0x80 ) ) |( z80.a & ( 0x08 | 0x20 ) ) | ( 0x02 | 0x10 );

			//noImpl("CPL")
		}
		0x37 -> noImpl("SCF")
		0x3f -> noImpl("CCF")


		0xcb -> noImpl("shift CB")
		0xd2 -> noImpl("JP NC,nnnn")
		0xd9 -> noImpl("EXX")
		0xda -> noImpl("JP C,nnnn")
		0xdd -> noImpl("shift DD")
		0xe2 -> noImpl("JP PO,nnnn")
		0xe9 -> noImpl("JP HL")
		0xea -> noImpl("JP PE,nnnn")
		0xeb -> noImpl("EX DE,HL")
		0xf2 -> noImpl("JP P,nnnn")
		0xfa -> noImpl("JP M,nnnn")
		0xfd -> {
			R = (R + 1) and 0x7f;
			this.execOneDDFD(nn, true)
		}

		else -> {
			println("Not implemented: 0x%02X".format(op))
			noImpl
		}
	}
}

