package com.soywiz.ktemu.system

import com.soywiz.ktemu.cpu.z80.Z80IO
import com.soywiz.ktemu.cpu.z80.Z80State
import com.soywiz.ktemu.gpu.sms.SmsVDP
import com.soywiz.ktemu.input.sms.SmsInput
import com.soywiz.ktemu.spu.sms.SmsSPU

// Sega Master System
class Sms {
	val vdp = SmsVDP()
	val spu = SmsSPU()
	val input = SmsInput()
	val ports = SmsPorts(vdp, input, spu)
	val cpu = Z80State(ports)
}