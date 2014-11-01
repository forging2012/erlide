package org.erlide.engine.model.erlang

import java.util.Collection
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.engine.model.root.ExternalLibrariesHelper
import org.junit.Before
import org.junit.Test

import static org.junit.Assert.assertEquals

class ExternalLibrariesHelperTests {

    ExternalLibrariesHelper helper

    @Before
    def void setup() {
        helper = new ExternalLibrariesHelper()
    }

    @Test
    def void expand_1() {
        val expected = #["z"]
        val actual = helper.expand("z", new Path(""))[null]
        assertEquals(expected, actual)
    }

    @Test
    def void expand_2() {
        val xmap = #{"m.erlidex" -> #["a", "b"]}
        val expected = #{"a", "b"}
        val actual = helper.expand_it("m.erlidex", new Path(""))[xmap.get(it)]
        assertEquals(expected, actual)
    }

    @Test
    def void expand_3() {
        val xmap = #{"m.erlidex" -> #["a", "b.erlidex", "c"], "b.erlidex" -> #["z", "y"]}
        val expected = #{"a", "z", "y", "c"}
        val actual = helper.expand_it("m.erlidex", new Path(""))[xmap.get(it)]
        assertEquals(expected, actual)
    }

    @Test
    def void expand_4() {
        val xmap = #{"m.erlidex" -> #["a", "b", "a"]}
        val expected = #{"a", "b"}
        val actual = helper.expand_it("m.erlidex", new Path(""))[xmap.get(it)]
        assertEquals(expected, actual)
    }

    @Test
    def void group_1() {
        val Collection<IPath> input = #[newPath("a/b"), newPath("a/c")]
        val expected = #{newPath("a") -> #[newPath("a/b"), newPath("a/c")]}
        val actual = helper.group(input)
        assertEquals(expected, actual)
    }

    def private IPath newPath(String path) {
        new Path(path)
    }

    @Test
    def void justFiles_1() {
        val input = #["a/b", "b/g", "a/c"]
        val expected = #{new Path("a"), new Path("b")}
        val actual = helper.justFolders(input)
        assertEquals(expected, actual)
    }

}
