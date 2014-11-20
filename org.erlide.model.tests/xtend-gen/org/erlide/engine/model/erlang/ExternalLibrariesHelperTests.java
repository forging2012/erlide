package org.erlide.engine.model.erlang;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.eclipse.core.runtime.IPath;
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
    ExternalLibrariesHelper _externalLibrariesHelper = new ExternalLibrariesHelper();
    this.helper = _externalLibrariesHelper;
  }
  
  @Test
  public void expand_1() {
    final List<String> expected = Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("z"));
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
    Pair<String, List<String>> _mappedTo = Pair.<String, List<String>>of("m.erlidex", Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("a", "b")));
    final Map<String, List<String>> xmap = Collections.<String, List<String>>unmodifiableMap(CollectionLiterals.<String, List<String>>newHashMap(_mappedTo));
    final Set<String> expected = Collections.<String>unmodifiableSet(CollectionLiterals.<String>newHashSet("a", "b"));
    Path _path = new Path("");
    final Function1<String, Collection<String>> _function = new Function1<String, Collection<String>>() {
      public Collection<String> apply(final String it) {
        return xmap.get(it);
      }
    };
    final Collection<String> actual = this.helper.expand_it("m.erlidex", _path, _function);
    Assert.assertEquals(expected, actual);
  }
  
  @Test
  public void expand_3() {
    Pair<String, List<String>> _mappedTo = Pair.<String, List<String>>of("m.erlidex", Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("a", "b.erlidex", "c")));
    Pair<String, List<String>> _mappedTo_1 = Pair.<String, List<String>>of("b.erlidex", Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("z", "y")));
    final Map<String, List<String>> xmap = Collections.<String, List<String>>unmodifiableMap(CollectionLiterals.<String, List<String>>newHashMap(_mappedTo, _mappedTo_1));
    final Set<String> expected = Collections.<String>unmodifiableSet(CollectionLiterals.<String>newHashSet("a", "z", "y", "c"));
    Path _path = new Path("");
    final Function1<String, Collection<String>> _function = new Function1<String, Collection<String>>() {
      public Collection<String> apply(final String it) {
        return xmap.get(it);
      }
    };
    final Collection<String> actual = this.helper.expand_it("m.erlidex", _path, _function);
    Assert.assertEquals(expected, actual);
  }
  
  @Test
  public void expand_4() {
    Pair<String, List<String>> _mappedTo = Pair.<String, List<String>>of("m.erlidex", Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("a", "b", "a")));
    final Map<String, List<String>> xmap = Collections.<String, List<String>>unmodifiableMap(CollectionLiterals.<String, List<String>>newHashMap(_mappedTo));
    final Set<String> expected = Collections.<String>unmodifiableSet(CollectionLiterals.<String>newHashSet("a", "b"));
    Path _path = new Path("");
    final Function1<String, Collection<String>> _function = new Function1<String, Collection<String>>() {
      public Collection<String> apply(final String it) {
        return xmap.get(it);
      }
    };
    final Collection<String> actual = this.helper.expand_it("m.erlidex", _path, _function);
    Assert.assertEquals(expected, actual);
  }
  
  @Test
  public void group_1() {
    IPath _newPath = this.newPath("a/b");
    IPath _newPath_1 = this.newPath("a/c");
    final Collection<IPath> input = Collections.<IPath>unmodifiableList(CollectionLiterals.<IPath>newArrayList(_newPath, _newPath_1));
    IPath _newPath_2 = this.newPath("a");
    IPath _newPath_3 = this.newPath("a/b");
    IPath _newPath_4 = this.newPath("a/c");
    Pair<IPath, List<IPath>> _mappedTo = Pair.<IPath, List<IPath>>of(_newPath_2, Collections.<IPath>unmodifiableList(CollectionLiterals.<IPath>newArrayList(_newPath_3, _newPath_4)));
    final Map<IPath, List<IPath>> expected = Collections.<IPath, List<IPath>>unmodifiableMap(CollectionLiterals.<IPath, List<IPath>>newHashMap(_mappedTo));
    final Map<IPath, List<IPath>> actual = this.helper.group(input);
    Assert.assertEquals(expected, actual);
  }
  
  private IPath newPath(final String path) {
    return new Path(path);
  }
  
  @Test
  public void justFiles_1() {
    final List<String> input = Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("a/b", "b/g", "a/c"));
    Path _path = new Path("a");
    Path _path_1 = new Path("b");
    final Set<Path> expected = Collections.<Path>unmodifiableSet(CollectionLiterals.<Path>newHashSet(_path, _path_1));
    final Collection<IPath> actual = this.helper.justFolders(input);
    Assert.assertEquals(expected, actual);
  }
}
