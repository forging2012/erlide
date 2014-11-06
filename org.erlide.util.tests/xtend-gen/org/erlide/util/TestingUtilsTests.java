package org.erlide.util;

import java.util.Collections;
import java.util.List;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.testing.utils.TestingUtils;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

@SuppressWarnings("all")
public class TestingUtilsTests {
  @Test
  public void positionsOf_0() {
    final String input = "something";
    final String ch = "z";
    final List<Integer> expected = Collections.<Integer>unmodifiableList(CollectionLiterals.<Integer>newArrayList());
    Iterable<Integer> _positionsOf = TestingUtils.positionsOf(input, ch);
    Matcher<Iterable<Integer>> _is = Matchers.<Iterable<Integer>>is(expected);
    MatcherAssert.<Iterable<Integer>>assertThat(_positionsOf, _is);
  }
  
  @Test
  public void positionsOf_1() {
    final String input = "mething";
    final String ch = "m";
    final List<Integer> expected = Collections.<Integer>unmodifiableList(CollectionLiterals.<Integer>newArrayList(Integer.valueOf(0)));
    Iterable<Integer> _positionsOf = TestingUtils.positionsOf(input, ch);
    Matcher<Iterable<Integer>> _is = Matchers.<Iterable<Integer>>is(expected);
    MatcherAssert.<Iterable<Integer>>assertThat(_positionsOf, _is);
  }
  
  @Test
  public void positionsOf_2() {
    final String input = "sommethimg";
    final String ch = "m";
    final List<Integer> expected = Collections.<Integer>unmodifiableList(CollectionLiterals.<Integer>newArrayList(Integer.valueOf(2), Integer.valueOf(3), Integer.valueOf(8)));
    Iterable<Integer> _positionsOf = TestingUtils.positionsOf(input, ch);
    Matcher<Iterable<Integer>> _is = Matchers.<Iterable<Integer>>is(expected);
    MatcherAssert.<Iterable<Integer>>assertThat(_positionsOf, _is);
  }
  
  @Test
  public void positionsOf_3() {
    final String input = "somxmxethimxg";
    final String ch = "mx";
    final List<Integer> expected = Collections.<Integer>unmodifiableList(CollectionLiterals.<Integer>newArrayList(Integer.valueOf(2), Integer.valueOf(4), Integer.valueOf(10)));
    Iterable<Integer> _positionsOf = TestingUtils.positionsOf(input, ch);
    Matcher<Iterable<Integer>> _is = Matchers.<Iterable<Integer>>is(expected);
    MatcherAssert.<Iterable<Integer>>assertThat(_positionsOf, _is);
  }
  
  @Test
  public void markersOf_0() {
    final String input = "somx2mxethimxg";
    final String ch = "mx";
    final List<Pair<Integer, Integer>> expected = Collections.<Pair<Integer, Integer>>unmodifiableList(CollectionLiterals.<Pair<Integer, Integer>>newArrayList());
    Iterable<Pair<Integer, Integer>> _markersOf = TestingUtils.markersOf(input, ch);
    Matcher<Iterable<Pair<Integer, Integer>>> _is = Matchers.<Iterable<Pair<Integer, Integer>>>is(expected);
    MatcherAssert.<Iterable<Pair<Integer, Integer>>>assertThat(_markersOf, _is);
  }
  
  @Test
  public void markersOf_1() {
    final String input = "§mx2§mxethimxg";
    final String ch = "mx";
    Pair<Integer, Integer> _mappedTo = Pair.<Integer, Integer>of(Integer.valueOf(0), Integer.valueOf(2));
    final List<Pair<Integer, Integer>> expected = Collections.<Pair<Integer, Integer>>unmodifiableList(CollectionLiterals.<Pair<Integer, Integer>>newArrayList(_mappedTo));
    Iterable<Pair<Integer, Integer>> _markersOf = TestingUtils.markersOf(input, ch);
    Matcher<Iterable<Pair<Integer, Integer>>> _is = Matchers.<Iterable<Pair<Integer, Integer>>>is(expected);
    MatcherAssert.<Iterable<Pair<Integer, Integer>>>assertThat(_markersOf, _is);
  }
  
  @Test
  public void markersOf_2() {
    final String input = "so§mx2§mxethimxg";
    final String ch = "mx";
    Pair<Integer, Integer> _mappedTo = Pair.<Integer, Integer>of(Integer.valueOf(2), Integer.valueOf(2));
    final List<Pair<Integer, Integer>> expected = Collections.<Pair<Integer, Integer>>unmodifiableList(CollectionLiterals.<Pair<Integer, Integer>>newArrayList(_mappedTo));
    Iterable<Pair<Integer, Integer>> _markersOf = TestingUtils.markersOf(input, ch);
    Matcher<Iterable<Pair<Integer, Integer>>> _is = Matchers.<Iterable<Pair<Integer, Integer>>>is(expected);
    MatcherAssert.<Iterable<Pair<Integer, Integer>>>assertThat(_markersOf, _is);
  }
  
  @Test
  public void markersOf_3() {
    final String input = "so§mx2§§mx1§ethi§mx2§g";
    final String ch = "mx";
    Pair<Integer, Integer> _mappedTo = Pair.<Integer, Integer>of(Integer.valueOf(2), Integer.valueOf(2));
    Pair<Integer, Integer> _mappedTo_1 = Pair.<Integer, Integer>of(Integer.valueOf(7), Integer.valueOf(1));
    Pair<Integer, Integer> _mappedTo_2 = Pair.<Integer, Integer>of(Integer.valueOf(16), Integer.valueOf(2));
    final List<Pair<Integer, Integer>> expected = Collections.<Pair<Integer, Integer>>unmodifiableList(CollectionLiterals.<Pair<Integer, Integer>>newArrayList(_mappedTo, _mappedTo_1, _mappedTo_2));
    Iterable<Pair<Integer, Integer>> _markersOf = TestingUtils.markersOf(input, ch);
    Matcher<Iterable<Pair<Integer, Integer>>> _is = Matchers.<Iterable<Pair<Integer, Integer>>>is(expected);
    MatcherAssert.<Iterable<Pair<Integer, Integer>>>assertThat(_markersOf, _is);
  }
}
