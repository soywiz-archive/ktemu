import com.soywiz.ktemu.cpu.z80.Z80IOArray
import com.soywiz.ktemu.cpu.z80.Z80State
import com.soywiz.ktemu.cpu.z80.execOne
import org.junit.Assert
import org.junit.Test

class Z80Test {
	val state = Z80State(ports = Z80IOArray(ByteArray(8)), memory = Z80IOArray(ByteArray(8)))
	val mem = state.M8

	@Test
	fun test1() {
		mem[0] = 0x0c // INC C
		Assert.assertEquals(state.C, 0)
		Assert.assertEquals(state.PC, 0)
		Assert.assertEquals(state.tstates, 0)
		state.execOne()
		Assert.assertEquals(state.C, 1)
		Assert.assertEquals(state.PC, 1)
		Assert.assertEquals(state.tstates, 4)
	}
}