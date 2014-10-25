package org.erlide.engine.model.erlang

import org.erlide.engine.model.root.ExternalLibrariesHelper
import org.junit.Before
import org.junit.Test

import static org.junit.Assert.assertEquals
import org.eclipse.core.runtime.Path

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

}
