package com.soywiz.ktemu.cpu.z80

import com.soywiz.ktemu.common.noImpl

fun Z80State.execOneDDFD_CB(op: Int, tempaddr:Int, kind: Boolean) {
	regkind = kind

	fun BITDD(BITN:Int) {
		val BITN = 2
		var bytetemp = M8[tempaddr]
		flag_c = flag_c
		flag_h = true
		flag_3 = ((tempaddr ushr 8) and FLAG_3) != 0
		flag_5 = ((tempaddr ushr 8) and FLAG_5) != 0
		if ((bytetemp and (1 shl BITN)) == 0) {
			flag_p = true
			flag_z = true
		}
		if( BITN == 7 && (bytetemp and 0x80) != 0) {
			flag_s = true
		}
	}

	when (op) {
		0x00 -> {
			noImpl("LD B,RLC (REGISTER+dd)")

			/*
			ops[0x00] = function op_0x00(tempaddr) {
				tstates += 8;
				z80.b=readbyte(tempaddr);
				{ (z80.b) = ( ((z80.b) & 0x7f)<<1 ) | ( (z80.b)>>7 ); z80.f = ( (z80.b) & 0x01 ) | sz53p_table[(z80.b)];};
				writebyte(tempaddr, z80.b);
			};
			*/
		}
		0x01 -> noImpl("LD C,RLC (REGISTER+dd)")
		0x02 -> noImpl("LD D,RLC (REGISTER+dd)")
		0x03 -> noImpl("LD E,RLC (REGISTER+dd)")
		0x04 -> noImpl("LD H,RLC (REGISTER+dd)")
		0x05 -> noImpl("LD L,RLC (REGISTER+dd)")
		0x06 -> noImpl("RLC (REGISTER+dd)")
		0x07 -> noImpl("LD A,RLC (REGISTER+dd)")
		0x08 -> noImpl("LD B,RRC (REGISTER+dd)")
		0x09 -> noImpl("LD C,RRC (REGISTER+dd)")
		0x0a -> noImpl("LD D,RRC (REGISTER+dd)")
		0x0b -> noImpl("LD E,RRC (REGISTER+dd)")
		0x0c -> noImpl("LD H,RRC (REGISTER+dd)")
		0x0d -> noImpl("LD L,RRC (REGISTER+dd)")
		0x0e -> noImpl("RRC (REGISTER+dd)")
		0x0f -> noImpl("LD A,RRC (REGISTER+dd)")
		0x10 -> noImpl("LD B,RL (REGISTER+dd)")
		0x11 -> noImpl("LD C,RL (REGISTER+dd)")
		0x12 -> noImpl("LD D,RL (REGISTER+dd)")
		0x13 -> noImpl("LD E,RL (REGISTER+dd)")
		0x14 -> noImpl("LD H,RL (REGISTER+dd)")
		0x15 -> noImpl("LD L,RL (REGISTER+dd)")
		0x16 -> noImpl("RL (REGISTER+dd)")
		0x17 -> noImpl("LD A,RL (REGISTER+dd)")
		0x18 -> noImpl("LD B,RR (REGISTER+dd)")
		0x19 -> noImpl("LD C,RR (REGISTER+dd)")
		0x1a -> noImpl("LD D,RR (REGISTER+dd)")
		0x1b -> noImpl("LD E,RR (REGISTER+dd)")
		0x1c -> noImpl("LD H,RR (REGISTER+dd)")
		0x1d -> noImpl("LD L,RR (REGISTER+dd)")
		0x1e -> noImpl("RR (REGISTER+dd)")
		0x1f -> noImpl("LD A,RR (REGISTER+dd)")
		0x20 -> noImpl("LD B,SLA (REGISTER+dd)")
		0x21 -> noImpl("LD C,SLA (REGISTER+dd)")
		0x22 -> noImpl("LD D,SLA (REGISTER+dd)")
		0x23 -> noImpl("LD E,SLA (REGISTER+dd)")
		0x24 -> noImpl("LD H,SLA (REGISTER+dd)")
		0x25 -> noImpl("LD L,SLA (REGISTER+dd)")
		0x26 -> noImpl("SLA (REGISTER+dd)")
		0x27 -> noImpl("LD A,SLA (REGISTER+dd)")
		0x28 -> noImpl("LD B,SRA (REGISTER+dd)")
		0x29 -> noImpl("LD C,SRA (REGISTER+dd)")
		0x2a -> noImpl("LD D,SRA (REGISTER+dd)")
		0x2b -> noImpl("LD E,SRA (REGISTER+dd)")
		0x2c -> noImpl("LD H,SRA (REGISTER+dd)")
		0x2d -> noImpl("LD L,SRA (REGISTER+dd)")
		0x2e -> noImpl("SRA (REGISTER+dd)")
		0x2f -> noImpl("LD A,SRA (REGISTER+dd)")
		0x30 -> noImpl("LD B,SLL (REGISTER+dd)")
		0x31 -> noImpl("LD C,SLL (REGISTER+dd)")
		0x32 -> noImpl("LD D,SLL (REGISTER+dd)")
		0x33 -> noImpl("LD E,SLL (REGISTER+dd)")
		0x34 -> noImpl("LD H,SLL (REGISTER+dd)")
		0x35 -> noImpl("LD L,SLL (REGISTER+dd)")
		0x36 -> noImpl("SLL (REGISTER+dd)")
		0x37 -> noImpl("LD A,SLL (REGISTER+dd)")
		0x38 -> noImpl("LD B,SRL (REGISTER+dd)")
		0x39 -> noImpl("LD C,SRL (REGISTER+dd)")
		0x3a -> noImpl("LD D,SRL (REGISTER+dd)")
		0x3b -> noImpl("LD E,SRL (REGISTER+dd)")
		0x3c -> noImpl("LD H,SRL (REGISTER+dd)")
		0x3d -> noImpl("LD L,SRL (REGISTER+dd)")
		0x3e -> noImpl("SRL (REGISTER+dd)")
		0x3f -> noImpl("LD A,SRL (REGISTER+dd)")
		in 0x40 .. 0x47 -> BITDD(0)
		in 0x48 .. 0x4f -> BITDD(1)
		in 0x50 .. 0x57 -> BITDD(2)
		in 0x58 .. 0x5f -> BITDD(3)
		in 0x60 .. 0x67 -> BITDD(4)
		in 0x68 .. 0x6f -> BITDD(5)
		in 0x70 .. 0x77 -> BITDD(6)
		in 0x78 .. 0x7f -> BITDD(7)
		0x80 -> noImpl("LD B,RES 0,(REGISTER+dd)")
		0x81 -> noImpl("LD C,RES 0,(REGISTER+dd)")
		0x82 -> noImpl("LD D,RES 0,(REGISTER+dd)")
		0x83 -> noImpl("LD E,RES 0,(REGISTER+dd)")
		0x84 -> noImpl("LD H,RES 0,(REGISTER+dd)")
		0x85 -> noImpl("LD L,RES 0,(REGISTER+dd)")
		0x86 -> noImpl("RES 0,(REGISTER+dd)")
		0x87 -> noImpl("LD A,RES 0,(REGISTER+dd)")
		0x88 -> noImpl("LD B,RES 1,(REGISTER+dd)")
		0x89 -> noImpl("LD C,RES 1,(REGISTER+dd)")
		0x8a -> noImpl("LD D,RES 1,(REGISTER+dd)")
		0x8b -> noImpl("LD E,RES 1,(REGISTER+dd)")
		0x8c -> noImpl("LD H,RES 1,(REGISTER+dd)")
		0x8d -> noImpl("LD L,RES 1,(REGISTER+dd)")
		0x8e -> noImpl("RES 1,(REGISTER+dd)")
		0x8f -> noImpl("LD A,RES 1,(REGISTER+dd)")
		0x90 -> noImpl("LD B,RES 2,(REGISTER+dd)")
		0x91 -> noImpl("LD C,RES 2,(REGISTER+dd)")
		0x92 -> noImpl("LD D,RES 2,(REGISTER+dd)")
		0x93 -> noImpl("LD E,RES 2,(REGISTER+dd)")
		0x94 -> noImpl("LD H,RES 2,(REGISTER+dd)")
		0x95 -> noImpl("LD L,RES 2,(REGISTER+dd)")
		0x96 -> noImpl("RES 2,(REGISTER+dd)")
		0x97 -> noImpl("LD A,RES 2,(REGISTER+dd)")
		0x98 -> noImpl("LD B,RES 3,(REGISTER+dd)")
		0x99 -> noImpl("LD C,RES 3,(REGISTER+dd)")
		0x9a -> noImpl("LD D,RES 3,(REGISTER+dd)")
		0x9b -> noImpl("LD E,RES 3,(REGISTER+dd)")
		0x9c -> noImpl("LD H,RES 3,(REGISTER+dd)")
		0x9d -> noImpl("LD L,RES 3,(REGISTER+dd)")
		0x9e -> noImpl("RES 3,(REGISTER+dd)")
		0x9f -> noImpl("LD A,RES 3,(REGISTER+dd)")
		0xa0 -> noImpl("LD B,RES 4,(REGISTER+dd)")
		0xa1 -> noImpl("LD C,RES 4,(REGISTER+dd)")
		0xa2 -> noImpl("LD D,RES 4,(REGISTER+dd)")
		0xa3 -> noImpl("LD E,RES 4,(REGISTER+dd)")
		0xa4 -> noImpl("LD H,RES 4,(REGISTER+dd)")
		0xa5 -> noImpl("LD L,RES 4,(REGISTER+dd)")
		0xa6 -> noImpl("RES 4,(REGISTER+dd)")
		0xa7 -> noImpl("LD A,RES 4,(REGISTER+dd)")
		0xa8 -> noImpl("LD B,RES 5,(REGISTER+dd)")
		0xa9 -> noImpl("LD C,RES 5,(REGISTER+dd)")
		0xaa -> noImpl("LD D,RES 5,(REGISTER+dd)")
		0xab -> noImpl("LD E,RES 5,(REGISTER+dd)")
		0xac -> noImpl("LD H,RES 5,(REGISTER+dd)")
		0xad -> noImpl("LD L,RES 5,(REGISTER+dd)")
		0xae -> noImpl("RES 5,(REGISTER+dd)")
		0xaf -> noImpl("LD A,RES 5,(REGISTER+dd)")
		0xb0 -> noImpl("LD B,RES 6,(REGISTER+dd)")
		0xb1 -> noImpl("LD C,RES 6,(REGISTER+dd)")
		0xb2 -> noImpl("LD D,RES 6,(REGISTER+dd)")
		0xb3 -> noImpl("LD E,RES 6,(REGISTER+dd)")
		0xb4 -> noImpl("LD H,RES 6,(REGISTER+dd)")
		0xb5 -> noImpl("LD L,RES 6,(REGISTER+dd)")
		0xb6 -> noImpl("RES 6,(REGISTER+dd)")
		0xb7 -> noImpl("LD A,RES 6,(REGISTER+dd)")
		0xb8 -> noImpl("LD B,RES 7,(REGISTER+dd)")
		0xb9 -> noImpl("LD C,RES 7,(REGISTER+dd)")
		0xba -> noImpl("LD D,RES 7,(REGISTER+dd)")
		0xbb -> noImpl("LD E,RES 7,(REGISTER+dd)")
		0xbc -> noImpl("LD H,RES 7,(REGISTER+dd)")
		0xbd -> noImpl("LD L,RES 7,(REGISTER+dd)")
		0xbe -> noImpl("RES 7,(REGISTER+dd)")
		0xbf -> noImpl("LD A,RES 7,(REGISTER+dd)")
		0xc0 -> noImpl("LD B,SET 0,(REGISTER+dd)")
		0xc1 -> noImpl("LD C,SET 0,(REGISTER+dd)")
		0xc2 -> noImpl("LD D,SET 0,(REGISTER+dd)")
		0xc3 -> noImpl("LD E,SET 0,(REGISTER+dd)")
		0xc4 -> noImpl("LD H,SET 0,(REGISTER+dd)")
		0xc5 -> noImpl("LD L,SET 0,(REGISTER+dd)")
		0xc6 -> noImpl("SET 0,(REGISTER+dd)")
		0xc7 -> noImpl("LD A,SET 0,(REGISTER+dd)")
		0xc8 -> noImpl("LD B,SET 1,(REGISTER+dd)")
		0xc9 -> noImpl("LD C,SET 1,(REGISTER+dd)")
		0xca -> noImpl("LD D,SET 1,(REGISTER+dd)")
		0xcb -> noImpl("LD E,SET 1,(REGISTER+dd)")
		0xcc -> noImpl("LD H,SET 1,(REGISTER+dd)")
		0xcd -> noImpl("LD L,SET 1,(REGISTER+dd)")
		0xce -> noImpl("SET 1,(REGISTER+dd)")
		0xcf -> noImpl("LD A,SET 1,(REGISTER+dd)")
		0xd0 -> noImpl("LD B,SET 2,(REGISTER+dd)")
		0xd1 -> noImpl("LD C,SET 2,(REGISTER+dd)")
		0xd2 -> noImpl("LD D,SET 2,(REGISTER+dd)")
		0xd3 -> noImpl("LD E,SET 2,(REGISTER+dd)")
		0xd4 -> noImpl("LD H,SET 2,(REGISTER+dd)")
		0xd5 -> noImpl("LD L,SET 2,(REGISTER+dd)")
		0xd6 -> noImpl("SET 2,(REGISTER+dd)")
		0xd7 -> noImpl("LD A,SET 2,(REGISTER+dd)")
		0xd8 -> noImpl("LD B,SET 3,(REGISTER+dd)")
		0xd9 -> noImpl("LD C,SET 3,(REGISTER+dd)")
		0xda -> noImpl("LD D,SET 3,(REGISTER+dd)")
		0xdb -> noImpl("LD E,SET 3,(REGISTER+dd)")
		0xdc -> noImpl("LD H,SET 3,(REGISTER+dd)")
		0xdd -> noImpl("LD L,SET 3,(REGISTER+dd)")
		0xde -> noImpl("SET 3,(REGISTER+dd)")
		0xdf -> noImpl("LD A,SET 3,(REGISTER+dd)")
		0xe0 -> noImpl("LD B,SET 4,(REGISTER+dd)")
		0xe1 -> noImpl("LD C,SET 4,(REGISTER+dd)")
		0xe2 -> noImpl("LD D,SET 4,(REGISTER+dd)")
		0xe3 -> noImpl("LD E,SET 4,(REGISTER+dd)")
		0xe4 -> noImpl("LD H,SET 4,(REGISTER+dd)")
		0xe5 -> noImpl("LD L,SET 4,(REGISTER+dd)")
		0xe6 -> noImpl("SET 4,(REGISTER+dd)")
		0xe7 -> noImpl("LD A,SET 4,(REGISTER+dd)")
		0xe8 -> noImpl("LD B,SET 5,(REGISTER+dd)")
		0xe9 -> noImpl("LD C,SET 5,(REGISTER+dd)")
		0xea -> noImpl("LD D,SET 5,(REGISTER+dd)")
		0xeb -> noImpl("LD E,SET 5,(REGISTER+dd)")
		0xec -> noImpl("LD H,SET 5,(REGISTER+dd)")
		0xed -> noImpl("LD L,SET 5,(REGISTER+dd)")
		0xee -> noImpl("SET 5,(REGISTER+dd)")
		0xef -> noImpl("LD A,SET 5,(REGISTER+dd)")
		0xf0 -> noImpl("LD B,SET 6,(REGISTER+dd)")
		0xf1 -> noImpl("LD C,SET 6,(REGISTER+dd)")
		0xf2 -> noImpl("LD D,SET 6,(REGISTER+dd)")
		0xf3 -> noImpl("LD E,SET 6,(REGISTER+dd)")
		0xf4 -> noImpl("LD H,SET 6,(REGISTER+dd)")
		0xf5 -> noImpl("LD L,SET 6,(REGISTER+dd)")
		0xf6 -> noImpl("SET 6,(REGISTER+dd)")
		0xf7 -> noImpl("LD A,SET 6,(REGISTER+dd)")
		0xf8 -> noImpl("LD B,SET 7,(REGISTER+dd)")
		0xf9 -> noImpl("LD C,SET 7,(REGISTER+dd)")
		0xfa -> noImpl("LD D,SET 7,(REGISTER+dd)")
		0xfb -> noImpl("LD E,SET 7,(REGISTER+dd)")
		0xfc -> noImpl("LD H,SET 7,(REGISTER+dd)")
		0xfd -> noImpl("LD L,SET 7,(REGISTER+dd)")
		0xfe -> noImpl("SET 7,(REGISTER+dd)")
		0xff -> noImpl("LD A,SET 7,(REGISTER+dd)")
	}
}
