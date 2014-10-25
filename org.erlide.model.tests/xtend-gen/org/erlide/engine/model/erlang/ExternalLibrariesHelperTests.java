package org.erlide.engine.model.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.model.root.ExternalLibrariesHelper;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
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
  
  @Test
  public void merge() {
  }
  
  @Test
  public void merge_1() {
    final ArrayList<Object> expected = CollectionLiterals.<Object>newArrayList();
    ArrayList<String> _newArrayList = CollectionLiterals.<String>newArrayList();
    ArrayList<String> _newArrayList_1 = CollectionLiterals.<String>newArrayList();
    final Collection<ErlangLibraryProperties> actual = this.helper.merge(_newArrayList, _newArrayList_1);
    Assert.assertEquals(expected, actual);
  }
  
  @Test
  public void merge_2() {
    final ArrayList<String> mods = CollectionLiterals.<String>newArrayList("foo");
    Path _path = new Path("");
    final Function1<String, IPath> _function = new Function1<String, IPath>() {
      public IPath apply(final String it) {
        return new Path(it);
      }
    };
    List<IPath> _map = ListExtensions.<String, IPath>map(mods, _function);
    ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList();
    ErlangLibraryProperties _erlangLibraryProperties = new ErlangLibraryProperties(_path, _map, _newArrayList, 
      ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    final ArrayList<ErlangLibraryProperties> expected = CollectionLiterals.<ErlangLibraryProperties>newArrayList(_erlangLibraryProperties);
    ArrayList<String> _newArrayList_1 = CollectionLiterals.<String>newArrayList();
    final Collection<ErlangLibraryProperties> actual = this.helper.merge(mods, _newArrayList_1);
    Assert.assertEquals(expected, actual);
  }
  
  @Test
  public void merge_3() {
    final ArrayList<String> incs = CollectionLiterals.<String>newArrayList("foo");
    Path _path = new Path("");
    ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList();
    final Function1<String, IPath> _function = new Function1<String, IPath>() {
      public IPath apply(final String it) {
        return new Path(it);
      }
    };
    List<IPath> _map = ListExtensions.<String, IPath>map(incs, _function);
    ErlangLibraryProperties _erlangLibraryProperties = new ErlangLibraryProperties(_path, _newArrayList, _map, 
      ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    final ArrayList<ErlangLibraryProperties> expected = CollectionLiterals.<ErlangLibraryProperties>newArrayList(_erlangLibraryProperties);
    ArrayList<String> _newArrayList_1 = CollectionLiterals.<String>newArrayList();
    final Collection<ErlangLibraryProperties> actual = this.helper.merge(_newArrayList_1, incs);
    Assert.assertEquals(expected, actual);
  }
}
