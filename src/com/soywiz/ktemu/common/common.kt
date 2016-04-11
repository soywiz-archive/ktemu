package com.soywiz.ktemu.common

val noImpl: Nothing get() = throw RuntimeException("Not implemented")
val invalidOp: Nothing get() = throw RuntimeException("Invalid operation")

fun noImpl(msg:String): Nothing = throw RuntimeException("Not implemented: $msg")
fun invalidOp(msg:String): Nothing = throw RuntimeException("Invalid operation: $msg")

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

//val Int.kilobytes:Int = this * 1024

fun <T> Class<T>.getResourceAsByteArray(name:String): ByteArray {
	return this.getResourceAsStream(name).readBytes()
}

class UbyteArray(val size:Int) {
	val data = ByteArray(size)
	operator fun get(offset:Int):Int = data[offset].toInt() and 0xFF
	operator fun set(offset:Int, value:Int):Unit = exec { data[offset] = value.toByte() }
}