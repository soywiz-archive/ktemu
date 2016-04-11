package com.soywiz.ktemu.cpu.z80

import com.soywiz.ktemu.common.noImpl

fun Z80State.execOneED(op: Int) {
	when (op) {
		0x40 -> noImpl("IN B, (C)")
		0x41 -> noImpl("OUT (C), B")
		0x42 -> noImpl("SBC HL, BC")
		0x43 -> noImpl("LD (nnnn), BC")
		0x44, 0x4c, 0x54, 0x5c, 0x64, 0x6c, 0x74, 0x7c -> noImpl("NEG")
		0x45, 0x4d, 0x55, 0x5d, 0x65, 0x6d, 0x75, 0x7d -> {
			iff1 = iff2;
			//{ { tstates+=6; var lowbyte =readbyte(z80.sp++); z80.sp &= 0xffff; var highbyte=readbyte(z80.sp++); z80.sp &= 0xffff; (z80.pc) = lowbyte | (highbyte << 8);};};
			PC = POP16(4 + 6)
		}
		0x46, 0x4e, 0x66, 0x6e -> noImpl("IM 0")
		0x47 -> noImpl("LD I, A")
		0x48 -> noImpl("IN C, (C)")
		0x49 -> noImpl("OUT (C), C")
		0x4a -> noImpl("ADC HL, BC")
		0x4b -> noImpl("LD BC, (nnnn)")
		0x4f -> noImpl("LD R, A")
		0x50 -> noImpl("IN D, (C)")
		0x51 -> noImpl("OUT (C), D")
		0x52 -> noImpl("SBC HL, DE")
		0x53 -> rnnnn16 = LD(DE, 4 + 12)
		0x56, 0x76 -> im = LD(1, 4)
		0x57 -> noImpl("LD A, I")
		0x58 -> noImpl("IN E, (C)")
		0x59 -> noImpl("OUT (C), E")
		0x5a -> noImpl("ADC HL, DE")
		0x5b -> noImpl("LD DE, (nnnn)")
		0x5e, 0x7e -> noImpl("IM 2")
		0x5f -> noImpl("LD A, R")
		0x60 -> noImpl("IN H, (C)")
		0x61 -> noImpl("OUT (C), H")
		0x62 -> noImpl("SBC HL, HL")
		0x63 -> noImpl("LD (nnnn), HL")
		0x67 -> noImpl("RRD")
		0x68 -> noImpl("IN L, (C)")
		0x69 -> noImpl("OUT (C), L")
		0x6a -> noImpl("ADC HL, HL")
		0x6b -> noImpl("LD HL, (nnnn)")
		0x6f -> noImpl("RLD")
		0x70 -> noImpl("IN F, (C)")
		0x71 -> noImpl("OUT (C), 0")
		0x72 -> noImpl("SBC HL, SP")
		0x73 -> noImpl("LD (nnnn), SP")
		0x78 -> noImpl("IN A, (C)")
		0x79 -> noImpl("OUT (C), A")
		0x7a -> noImpl("ADC HL, SP")
		0x7b -> noImpl("LD SP, (nnnn)")
		0xa0 -> noImpl("LDI")
		0xa1 -> noImpl("CPI")
		0xa2 -> noImpl("INI")
		0xa3 -> noImpl("OUTI")
		0xa8 -> noImpl("LDD")
		0xa9 -> noImpl("CPD")
		0xaa -> noImpl("IND")
		0xab -> noImpl("OUTD")
		0xb0 -> { // LDIR (memcpy)
			tstates += 4 + 8
			rDE = rHL
			DE++
			HL++
			BC--
			if (BC != 0) {
				tstates += 5
				PC -= 2
			}
		}
		0xb1 -> noImpl("CPIR")
		0xb2 -> noImpl("INIR")
		0xb3 -> noImpl("OTIR")
		0xb8 -> noImpl("LDDR")
		0xb9 -> noImpl("CPDR")
		0xba -> noImpl("INDR")
		0xbb -> noImpl("OTDR")
		else -> noImpl
	}
}
