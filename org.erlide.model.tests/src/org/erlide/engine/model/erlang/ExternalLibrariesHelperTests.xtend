package org.erlide.engine.model.erlang

import org.eclipse.core.runtime.Path
import org.erlide.engine.model.root.ExternalLibrariesHelper
import org.junit.Before
import org.junit.Test

import static org.junit.Assert.assertEquals
import org.erlide.engine.model.root.ErlangLibraryProperties
import org.erlide.engine.model.root.ProjectPreferencesConstants

class ExternalLibrariesHelperTests {

	ExternalLibrariesHelper helper

	@Before
	def void setup() {
		helper = new ExternalLibrariesHelper("", "")
	}

	@Test
	def void expand_1() {
		val expected = newArrayList("z")
		val actual = helper.expand("z", new Path(""))[null]
		assertEquals(expected, actual)
	}

	@Test
	def void expand_2() {
		val xmap = newHashMap("m.erlidex" -> newArrayList("a", "b"))
		val expected = newArrayList("a", "b")
		val actual = helper.expand_it("m.erlidex", new Path(""))[xmap.get(it)]
		assertEquals(expected, actual)
	}

	@Test
	def void expand_3() {
		val xmap = newHashMap("m.erlidex" -> newArrayList("a", "b.erlidex", "c"), "b.erlidex" -> newArrayList("z", "y"))
		val expected = newArrayList("a", "z", "y", "c")
		val actual = helper.expand_it("m.erlidex", new Path(""))[xmap.get(it)]
		assertEquals(expected, actual)
	}

	@Test
	def void merge() {
	}

	@Test
	def void merge_1() {
		val expected = newArrayList()
		val actual = helper.merge(newArrayList(), newArrayList())
		assertEquals(expected, actual)
	}

	@Test
	def void merge_2() {
		val mods = newArrayList("foo")
		val expected = newArrayList(
			new ErlangLibraryProperties(
				new Path(""),
				mods.map[new Path(it)],
				newArrayList(),
				ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
			))
		val actual = helper.merge(mods, newArrayList())
		assertEquals(expected, actual)
	}

	@Test
	def void merge_3() {
		val incs = newArrayList("foo")
		val expected = newArrayList(
			new ErlangLibraryProperties(
				new Path(""),
				newArrayList(),
				incs.map[new Path(it)],
				ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
			))
		val actual = helper.merge(newArrayList(), incs)
		assertEquals(expected, actual)
	}

}
