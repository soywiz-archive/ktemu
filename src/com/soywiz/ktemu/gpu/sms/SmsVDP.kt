package com.soywiz.ktemu.gpu.sms

import com.soywiz.ktemu.common.noImpl

class SmsVDP {
	fun writeaddr(value: Int) {
		noImpl
	}

	fun writebyte(value: Int) {
		noImpl
	}

	fun getLine(): Int {
		noImpl
	}

	fun getX(): Int {
		return 0
	}

	fun readByte(): Int {
		noImpl
	}

	fun readStatus(): Int {
		noImpl
	}
}