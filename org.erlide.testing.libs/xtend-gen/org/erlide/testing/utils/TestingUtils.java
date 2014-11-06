package org.erlide.testing.utils;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pair;

@SuppressWarnings("all")
public class TestingUtils {
  /**
   * Return all positions (offsets) of the given str in the given source file.
   */
  public static Iterable<Integer> positionsOf(final String source, final String str) {
    ArrayList<Integer> _xblockexpression = null;
    {
      final ArrayList<Integer> buf = CollectionLiterals.<Integer>newArrayList();
      int pos = source.indexOf(str);
      while ((pos >= 0)) {
        {
          buf.add(Integer.valueOf(pos));
          int _indexOf = source.indexOf(str, (pos + 1));
          pos = _indexOf;
        }
      }
      _xblockexpression = buf;
    }
    return _xblockexpression;
  }
  
  /**
   * Return all positions and the number in the given marker. The marker is
   *  wrapped by §§, and the method returns matches for §[0-9]+§, as a sequence
   *  of pairs (offset, parsedNumber)
   */
  public static Iterable<Pair<Integer, Integer>> markersOf(final String source, final String prefix) {
    ArrayList<Pair<Integer, Integer>> _xblockexpression = null;
    {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("§");
      _builder.append(prefix, "");
      _builder.append("([0-9]+)§");
      final Pattern regex = Pattern.compile(_builder.toString());
      final Matcher matcher = regex.matcher(source);
      final ArrayList<Pair<Integer, Integer>> buf = CollectionLiterals.<Pair<Integer, Integer>>newArrayList();
      boolean found = matcher.find();
      while (found) {
        {
          int _start = matcher.start();
          String _group = matcher.group(1);
          Integer _valueOf = Integer.valueOf(_group);
          Pair<Integer, Integer> _mappedTo = Pair.<Integer, Integer>of(Integer.valueOf(_start), _valueOf);
          buf.add(_mappedTo);
          boolean _find = matcher.find();
          found = _find;
        }
      }
      _xblockexpression = buf;
    }
    return _xblockexpression;
  }
}
