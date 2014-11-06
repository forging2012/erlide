package org.erlide.engine.new_model;

import java.util.Collections;
import java.util.List;
import org.eclipse.core.resources.IProject;
import org.eclipse.handly.model.IHandle;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlForm;
import org.erlide.engine.new_model.IErlFunction;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.testing.utils.WorkspaceTest;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class ErlSourceTest extends WorkspaceTest {
  private IErlSource erlFile;
  
  @Before
  public void setup() throws Exception {
    IProject _setUpProject = this.setUpProject("Test002");
    final IErlProject erlProject = ErlModelCore.create(_setUpProject);
    IErlSource _sourceFile = erlProject.getSourceFile("nop.erl");
    this.erlFile = _sourceFile;
  }
  
  @Test
  public void testErlFile() {
    try {
      boolean _exists = this.erlFile.exists();
      Matcher<Boolean> _is = Matchers.<Boolean>is(Boolean.valueOf(true));
      MatcherAssert.<Boolean>assertThat(Boolean.valueOf(_exists), _is);
      Iterable<IErlForm> _forms = this.erlFile.getForms();
      int _length = ((Object[])Conversions.unwrapArray(_forms, Object.class)).length;
      IHandle[] _children = this.erlFile.getChildren();
      int _length_1 = _children.length;
      Matcher<Integer> _is_1 = Matchers.<Integer>is(Integer.valueOf(_length_1));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length), _is_1);
      IHandle[] _children_1 = this.erlFile.getChildren();
      int _length_2 = _children_1.length;
      Matcher<Integer> _is_2 = Matchers.<Integer>is(Integer.valueOf(5));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_2), _is_2);
      final Iterable<IErlAttribute> attrs = this.erlFile.getAttributes();
      int _length_3 = ((Object[])Conversions.unwrapArray(attrs, Object.class)).length;
      Matcher<Integer> _is_3 = Matchers.<Integer>is(Integer.valueOf(2));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_3), _is_3);
      IErlAttribute _head = IterableExtensions.<IErlAttribute>head(attrs);
      Iterable<IErlAttribute> _attributesWithTag = this.erlFile.getAttributesWithTag("module");
      Matcher<Iterable<IErlAttribute>> _is_4 = Matchers.<Iterable<IErlAttribute>>is(_attributesWithTag);
      MatcherAssert.<List<IErlAttribute>>assertThat(Collections.<IErlAttribute>unmodifiableList(CollectionLiterals.<IErlAttribute>newArrayList(_head)), _is_4);
      Iterable<IErlAttribute> _tail = IterableExtensions.<IErlAttribute>tail(attrs);
      IErlAttribute _head_1 = IterableExtensions.<IErlAttribute>head(_tail);
      Iterable<IErlAttribute> _attributesWithTag_1 = this.erlFile.getAttributesWithTag("export");
      Matcher<Iterable<IErlAttribute>> _is_5 = Matchers.<Iterable<IErlAttribute>>is(_attributesWithTag_1);
      MatcherAssert.<List<IErlAttribute>>assertThat(Collections.<IErlAttribute>unmodifiableList(CollectionLiterals.<IErlAttribute>newArrayList(_head_1)), _is_5);
      final Iterable<IErlFunction> funs = this.erlFile.getFunctions();
      int _length_4 = ((Object[])Conversions.unwrapArray(funs, Object.class)).length;
      Matcher<Integer> _is_6 = Matchers.<Integer>is(Integer.valueOf(3));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_4), _is_6);
      IErlFunction _head_2 = IterableExtensions.<IErlFunction>head(funs);
      IErlFunction _function = this.erlFile.getFunction("a", 1);
      Matcher<IErlFunction> _is_7 = Matchers.<IErlFunction>is(_function);
      MatcherAssert.<IErlFunction>assertThat(_head_2, _is_7);
      Iterable<IErlFunction> _tail_1 = IterableExtensions.<IErlFunction>tail(funs);
      IErlFunction _head_3 = IterableExtensions.<IErlFunction>head(_tail_1);
      IErlFunction _function_1 = this.erlFile.getFunction("b", 0);
      Matcher<IErlFunction> _is_8 = Matchers.<IErlFunction>is(_function_1);
      MatcherAssert.<IErlFunction>assertThat(_head_3, _is_8);
      Iterable<IErlFunction> _tail_2 = IterableExtensions.<IErlFunction>tail(funs);
      Iterable<IErlFunction> _tail_3 = IterableExtensions.<IErlFunction>tail(_tail_2);
      IErlFunction _head_4 = IterableExtensions.<IErlFunction>head(_tail_3);
      IErlFunction _function_2 = this.erlFile.getFunction("c", 3);
      Matcher<IErlFunction> _is_9 = Matchers.<IErlFunction>is(_function_2);
      MatcherAssert.<IErlFunction>assertThat(_head_4, _is_9);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
