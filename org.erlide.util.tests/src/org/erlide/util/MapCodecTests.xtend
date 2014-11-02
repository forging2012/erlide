package org.erlide.util

import org.hamcrest.MatcherAssert
import org.hamcrest.Matchers
import org.junit.Test

class MapCodecTests {
    
    @Test
    def void mapShouldRestore() {
        val expected= #{"a"->"b", " c "->" d ", "e"->"f"}
        val str = MapCodec.encode(expected)
        val actual = MapCodec.decode(str)
        MatcherAssert.assertThat(actual, Matchers.is(expected))
    }

}