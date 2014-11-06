package org.erlide.util

import org.erlide.testing.utils.TestingUtils
import org.junit.Test

import static org.hamcrest.MatcherAssert.*
import static org.hamcrest.Matchers.*

class TestingUtilsTests {

	@Test
	def void positionsOf_0() {
		val input = "something"
		val ch = "z"
		val expected = #[]
		assertThat(TestingUtils.positionsOf(input, ch), is(expected))
	}

	@Test
	def void positionsOf_1() {
		val input = "mething"
		val ch = "m"
		val expected = #[0]
		assertThat(TestingUtils.positionsOf(input, ch), is(expected))
	}

	@Test
	def void positionsOf_2() {
		val input = "sommethimg"
		val ch = "m"
		val expected = #[2, 3, 8]
		assertThat(TestingUtils.positionsOf(input, ch), is(expected))
	}

	@Test
	def void positionsOf_3() {
		val input = "somxmxethimxg"
		val ch = "mx"
		val expected = #[2, 4, 10]
		assertThat(TestingUtils.positionsOf(input, ch), is(expected))
	}

	@Test
	def void markersOf_0() {
		val input = "somx2mxethimxg"
		val ch = "mx"
		val expected = #[]
		assertThat(TestingUtils.markersOf(input, ch), is(expected))
	}

	@Test
	def void markersOf_1() {
		val input = "§mx2§mxethimxg"
		val ch = "mx"
		val expected = #[0 -> 2]
		assertThat(TestingUtils.markersOf(input, ch), is(expected))
	}

	@Test
	def void markersOf_2() {
		val input = "so§mx2§mxethimxg"
		val ch = "mx"
		val expected = #[2 -> 2]
		assertThat(TestingUtils.markersOf(input, ch), is(expected))
	}

	@Test
	def void markersOf_3() {
		val input = "so§mx2§§mx1§ethi§mx2§g"
		val ch = "mx"
		val expected = #[2 -> 2, 7 -> 1, 16 -> 2]
		assertThat(TestingUtils.markersOf(input, ch), is(expected))
	}
}
