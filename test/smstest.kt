import com.soywiz.ktemu.common.getResourceAsByteArray
import com.soywiz.ktemu.system.sms.Sms
import org.junit.Test

class SmsTest {
	@Test
	fun test1() {
		val sms = Sms()
		val cpu = sms.cpu
		val mem = sms.mem
		val c = SmsTest::class.java
		sms.loadRom(SmsTest::class.java.getResourceAsByteArray("wb3.sms"))
		for (n in 0 .. 20000) {
			//println("%04X: %04X %02X".format(cpu.PC, cpu.SP, mem[cpu.PC]))
			sms.execStep()
		}
	}
}

/*

PC:0, OP:f3
z80_ops_full.js:4342 PC:1, OP:ed
z80_ops_full.js:4342 PC:3, OP:c3
z80_ops_full.js:4342 PC:97, OP:31
z80_ops_full.js:4342 PC:9a, OP:af
z80_ops_full.js:4342 PC:9b, OP:ef
z80_ops_full.js:4342 PC:28, OP:d3
z80_ops_full.js:4342 PC:2a, OP:c9
z80_ops_full.js:4342 PC:9c, OP:3e
z80_ops_full.js:4342 PC:9e, OP:ef
z80_ops_full.js:4342 PC:28, OP:d3
z80_ops_full.js:4342 PC:2a, OP:c9
z80_ops_full.js:4342 PC:9f, OP:af
z80_ops_full.js:4342 PC:a0, OP:ef
z80_ops_full.js:4342 PC:28, OP:d3
z80_ops_full.js:4342 PC:2a, OP:c9
z80_ops_full.js:4342 PC:a1, OP:3e
z80_ops_full.js:4342 PC:a3, OP:ef
z80_ops_full.js:4342 PC:28, OP:d3
z80_ops_full.js:4342 PC:2a, OP:c9
z80_ops_full.js:4342 PC:a4, OP:af
z80_ops_full.js:4342 PC:a5, OP:f7
z80_ops_full.js:4342 PC:30, OP:d3
z80_ops_full.js:4342 PC:32, OP:c9
z80_ops_full.js:4342 PC:a6, OP:3e
z80_ops_full.js:4342 PC:a8, OP:ef
z80_ops_full.js:4342 PC:28, OP:d3
z80_ops_full.js:4342 PC:2a, OP:c9
z80_ops_full.js:4342 PC:a9, OP:3e

 */