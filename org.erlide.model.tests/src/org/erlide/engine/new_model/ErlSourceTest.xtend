package org.erlide.engine.new_model

import org.erlide.testing.utils.WorkspaceTest
import org.junit.Before
import org.junit.Test

import static org.hamcrest.MatcherAssert.*
import static org.hamcrest.Matchers.*

class ErlSourceTest extends WorkspaceTest {
	private IErlSource erlFile

	@Before
	def void setup() throws Exception
    {
		val IErlProject erlProject = ErlModelCore.create(setUpProject("Test002"))
		erlFile = erlProject.getSourceFile("nop.erl")
	}

	@Test
	def void testErlFile() {
		assertThat(erlFile.exists(), is(true))

		assertThat(erlFile.forms.length, is(erlFile.children.length))
		assertThat(erlFile.children.length, is(5))

		val attrs = erlFile.attributes
		assertThat(attrs.length, is(2))
		assertThat(#[attrs.head], is(erlFile.getAttributesWithTag("module")))
		assertThat(#[attrs.tail.head], is(erlFile.getAttributesWithTag("export")))

		val funs = erlFile.functions
		assertThat(funs.length, is(3))
		assertThat(funs.head, is(erlFile.getFunction("a", 1)))
		assertThat(funs.tail.head, is(erlFile.getFunction("b", 0)))
		assertThat(funs.tail.tail.head, is(erlFile.getFunction("c", 3)))

	}

}
