package org.erlide.testing.utils

import java.util.regex.Pattern

class TestingUtils {

	/** Return all positions (offsets) of the given str in the given source file.
       */
	def static Iterable<Integer> positionsOf(String source, String str) {
		val buf = newArrayList()
		var pos = source.indexOf(str)
		while (pos >= 0) {
			buf += pos // we need the position before the first character of this marker
			pos = source.indexOf(str, pos + 1)
		}
		buf
	}

	/** Return all positions and the number in the given marker. The marker is
         *  wrapped by §§, and the method returns matches for §[0-9]+§, as a sequence
         *  of pairs (offset, parsedNumber)
         */
	def static Iterable<Pair<Integer, Integer>> markersOf(String source, String prefix) {
		val Pattern regex = Pattern.compile('''§«prefix»([0-9]+)§''')
		val matcher = regex.matcher(source)
		val buf = newArrayList()
		var found = matcher.find
		while (found) {
			buf.add(matcher.start -> Integer.valueOf(matcher.group(1)))
			found = matcher.find
		}
		buf
	}

}
