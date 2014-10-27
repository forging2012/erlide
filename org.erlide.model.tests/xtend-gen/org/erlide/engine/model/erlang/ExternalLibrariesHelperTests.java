package org.erlide.engine.model.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.engine.model.root.ExternalLibrariesHelper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class ExternalLibrariesHelperTests {
  private ExternalLibrariesHelper helper;
  
  @Before
  public void setup() {
    ExternalLibrariesHelper _externalLibrariesHelper = new ExternalLibrariesHelper("", "");
    this.helper = _externalLibrariesHelper;
  }
  
  @Test
  public void expand_1() {
    final ArrayList<String> expected = CollectionLiterals.<String>newArrayList("z");
    Path _path = new Path("");
    final Function1<String, Collection<String>> _function = new Function1<String, Collection<String>>() {
      public Collection<String> apply(final String it) {
        return null;
      }
    };
    final Collection<String> actual = this.helper.expand("z", _path, _function);
    Assert.assertEquals(expected, actual);
  }
  
  @Test
  public void expand_2() {
    ArrayList<String> _newArrayList = CollectionLiterals.<String>newArrayList("a", "b");
    Pair<String, ArrayList<String>> _mappedTo = Pair.<String, ArrayList<String>>of("m.erlidex", _newArrayList);
    final HashMap<String, ArrayList<String>> xmap = CollectionLiterals.<String, ArrayList<String>>newHashMap(_mappedTo);
    final ArrayList<String> expected = CollectionLiterals.<String>newArrayList("a", "b");
    Path _path = new Path("");
    final Function1<String, ArrayList<String>> _function = new Function1<String, ArrayList<String>>() {
      public ArrayList<String> apply(final String it) {
        return xmap.get(it);
      }
    };
    final Collection<String> actual = this.helper.expand_it("m.erlidex", _path, _function);
    Assert.assertEquals(expected, actual);
  }
  
  @Test
  public void expand_3() {
    ArrayList<String> _newArrayList = CollectionLiterals.<String>newArrayList("a", "b.erlidex", "c");
    Pair<String, ArrayList<String>> _mappedTo = Pair.<String, ArrayList<String>>of("m.erlidex", _newArrayList);
    ArrayList<String> _newArrayList_1 = CollectionLiterals.<String>newArrayList("z", "y");
    Pair<String, ArrayList<String>> _mappedTo_1 = Pair.<String, ArrayList<String>>of("b.erlidex", _newArrayList_1);
    final HashMap<String, ArrayList<String>> xmap = CollectionLiterals.<String, ArrayList<String>>newHashMap(_mappedTo, _mappedTo_1);
    final ArrayList<String> expected = CollectionLiterals.<String>newArrayList("a", "z", "y", "c");
    Path _path = new Path("");
    final Function1<String, ArrayList<String>> _function = new Function1<String, ArrayList<String>>() {
      public ArrayList<String> apply(final String it) {
        return xmap.get(it);
      }
    };
    final Collection<String> actual = this.helper.expand_it("m.erlidex", _path, _function);
    Assert.assertEquals(expected, actual);
  }
}
