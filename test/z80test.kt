import com.soywiz.ktemu.common.noImpl
import com.soywiz.ktemu.cpu.z80.Z80IO
import com.soywiz.ktemu.cpu.z80.Z80State
import com.soywiz.ktemu.cpu.z80.decode
import org.junit.Assert
import org.junit.Test

class Z80Test {
	val state = Z80State(ports = object : Z80IO {
		override fun set(offset: Int, value: Int) = noImpl
		override fun get(offset: Int): Int = noImpl
	})
	val mem = state.M8

	@Test
	fun test1() {
		mem[0] = 0x0c // INC C
		Assert.assertEquals(state.C, 0)
		Assert.assertEquals(state.PC, 0)
		Assert.assertEquals(state.tstates, 0)
		state.decode()
		Assert.assertEquals(state.C, 1)
		Assert.assertEquals(state.PC, 1)
		Assert.assertEquals(state.tstates, 4)
	}
}