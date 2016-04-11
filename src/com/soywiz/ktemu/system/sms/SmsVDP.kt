package com.soywiz.ktemu.system.sms

import com.soywiz.ktemu.common.UbyteArray
import com.soywiz.ktemu.common.noImpl

/*
- Custom video controller (VDP), derived from the TMS9918/9928 chip made by
  Texas Instruments, and providing:
    - 256x192 tile-based screen in 16 colours,
    - 64 8x8, 8x16 or 16x16 hardware sprites,
    - 32 colours on screen (16 for sprites, 16 for background) from a palette of 64,
    - Hardware up/down/left/right scrolling of all or part of screen.

The Master System had a 6-bit RGB palette (64 colors), with 32 colors on-screen at once. It is possible to display all 64 colors at once using raster effects (line interrupts).
 */
class SmsVDP {
	val vram = UbyteArray(0x4000);
	val vramUntwiddled = UbyteArray(0x8000);
	val palette = UbyteArray(32);
	val paletteR = UbyteArray(32);
	val paletteG = UbyteArray(32);
	val paletteB = UbyteArray(32);
	val paletteRGB = IntArray(32);
	val vdp_regs = UbyteArray(16);
	var vdp_addr_state = 0
	var vdp_addr_latch = 0
	var vdp_mode_select = 0
	var vdp_addr = 0
	var vdp_status = 0;
	var vdp_pending_hblank = false
	var vdp_current_line = 0
	var vdp_hblank_counter = 0
	var currentFrame = 0

	init {
		vdp_regs[0] = 0
		vdp_regs[1] = 0
		vdp_regs[2] = 0xFF
		vdp_regs[3] = 0xFF
		vdp_regs[4] = 0xFF
		vdp_regs[5] = 0xFF
		vdp_regs[6] = 0xfb;
		vdp_regs[10] = 0xff;
	}

	fun writeaddr(v: Int) {
		//println("SmsVDP.addr: %02X".format(v))
		//noImpl("SmsVDP.addr: %02X".format(address))

		if (vdp_addr_state === 0) {
			vdp_addr_state = 1
			vdp_addr_latch = v
		} else {
			vdp_addr_state = 0;
			when (v ushr 6) {
				0, 1 -> {
					vdp_mode_select = 0;
					vdp_addr = vdp_addr_latch or ((v and 0x3f) shl 8);
				}
				2 -> {
					var regnum = v and 0xf;
					vdp_regs[regnum] = vdp_addr_latch
					when (regnum) {
						7 -> update_border()
					}
				}
				3 -> {
					vdp_mode_select = 1;
					vdp_addr = vdp_addr_latch and 0x1f;
				}
			}
		}
	}

	fun writebyte(value: Int) {
		vdp_addr_state = 0;
		if (vdp_mode_select === 0) {
			writeram(value);
		} else {
			writepalette(value);
		}
	}

	fun getLine(): Int {
		noImpl("vdp.getLine()")
	}

	fun getX(): Int {
		noImpl("vdp.getX()")
	}

	fun readByte(): Int {
		noImpl("vdp.readByte()")
	}

	fun readStatus(): Int {
		val res = vdp_status;
		vdp_status = vdp_status and 0x1f;
		vdp_pending_hblank = false;

		//z80_set_irq(false);
		//println("@TODO: z80_set_irq(false);")

		vdp_addr_state = 0;
		return res;
	}

	private fun writeram(value: Int) {
		vram[vdp_addr] = value;
		var planarBase = vdp_addr and 0x3ffc;
		var twiddledBase = planarBase * 2;
		var val0 = vram[planarBase + 0];
		var val1 = vram[planarBase + 1];
		var val2 = vram[planarBase + 2];
		var val3 = vram[planarBase + 3];
		for (i in 0 until 8) {
			var effectiveBit = 7 - i;
			var index = (((val0 ushr effectiveBit) and 1)) or (((val1 ushr effectiveBit) and 1) shl 1) or (((val2 ushr effectiveBit) and 1) shl 2) or (((val3 ushr effectiveBit) and 1) shl 3);
			vramUntwiddled[twiddledBase + i] = index;
			println("writeram! ${twiddledBase + i} = $index")
		}
		vdp_addr = (vdp_addr + 1) and 0x3fff;
	}

	private fun writepalette(value: Int) {
		fun expandBits(value: Int): Int {
			var v = value and 3;
			v = v or (v shl 2);
			v = v or (v shl 4);
			return v
		}

		val r = expandBits(value ushr 0);
		val g = expandBits(value ushr 2);
		val b = expandBits(value ushr 4);
		val pal_addr = vdp_addr and 0x1f;
		paletteR[pal_addr] = r;
		paletteG[pal_addr] = g;
		paletteB[pal_addr] = b;
		paletteRGB[pal_addr] = 0xff000000.toInt() or (b shl 16) or (g shl 8) or (r shl 0);
		palette[pal_addr] = value;
		vdp_addr = (vdp_addr + 1) and 0x3fff;
		update_border();

		println("writepalette! RGB($b, $g, $r)")
	}

	private fun update_border() {

	}


	fun hblank():Int {
		val firstDisplayLine = 3 + 13 + 54;
		val pastEndDisplayLine = firstDisplayLine + 192;
		val endOfFrame = pastEndDisplayLine + 48 + 3;
		if (vdp_current_line == firstDisplayLine) vdp_hblank_counter = vdp_regs[10];
		if (vdp_current_line >= firstDisplayLine && vdp_current_line < pastEndDisplayLine) {
			rasterize_line(vdp_current_line - firstDisplayLine);
			if (--vdp_hblank_counter < 0) {
				vdp_hblank_counter = vdp_regs[10];
				vdp_pending_hblank = true;
			}
		}
		vdp_current_line++;
		var needIrq = 0;
		if (vdp_current_line === endOfFrame) {
			vdp_current_line = 0;
			vdp_status = vdp_status or 128;
			needIrq = needIrq or 4;
			currentFrame++;
			vdp_frame_hook(currentFrame);
			//if (borderColourCss) {
			//	// Lazily updated and only on changes.
			//	canvas.style.borderColor = borderColourCss;
			//	borderColourCss = null;
			//}
		}
		if ((vdp_regs[1] and 32) != 0 && (vdp_status and 128) != 0) {
			needIrq = needIrq or 2;
		}
		if ((vdp_regs[0] and 16) != 0 && vdp_pending_hblank) {
			needIrq = needIrq or 1;
		}
		return needIrq;
	}

	private fun rasterize_line(i: Int) {

	}

	private fun vdp_frame_hook(currentFrame: Int) {
	}
}