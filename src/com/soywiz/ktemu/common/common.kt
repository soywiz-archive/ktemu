package com.soywiz.ktemu.common

val noImpl: Nothing get() = throw RuntimeException("Not implemented")

infix fun Int.hasAll(bits: Int): Boolean = (this and bits) == bits
infix fun Int.setBits(bits: Int): Int = (this or bits)
infix fun Int.resetBits(bits: Int): Int = (this and bits.inv())
fun Int.setResetBits(bits: Int, set: Boolean): Int {
	if (set) {
		return this setBits bits
	} else {
		return this resetBits  bits
	}
}

inline fun exec(callback: () -> Unit): Unit {
	callback()
}
