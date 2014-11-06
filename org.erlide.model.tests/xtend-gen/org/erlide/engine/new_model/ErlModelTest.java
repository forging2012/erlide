package org.erlide.engine.new_model;

import com.google.common.base.Objects;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Path;
import org.eclipse.handly.model.IHandle;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.testing.utils.WorkspaceTest;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class ErlModelTest extends WorkspaceTest {
  @Before
  public void setup() {
    try {
      this.setUpProject("Test001");
      this.setUpProject("SimpleProject");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void testErlModel() {
    try {
      final IErlModel erlModel = ErlModelCore.getErlModel();
      Iterable<IErlProject> erlProjects = erlModel.getProjects();
      final Iterable<IErlProject> _converted_erlProjects = (Iterable<IErlProject>)erlProjects;
      int _length = ((Object[])Conversions.unwrapArray(_converted_erlProjects, Object.class)).length;
      Matcher<Integer> _is = Matchers.<Integer>is(Integer.valueOf(1));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length), _is);
      final IErlProject erlProject = IterableExtensions.<IErlProject>head(erlProjects);
      String _name = ((IHandle) erlProject).getName();
      Matcher<String> _is_1 = Matchers.<String>is("Test001");
      MatcherAssert.<String>assertThat(_name, _is_1);
      final IErlProject erlProject2 = erlModel.getProject("Test002");
      boolean _exists = ((IHandle) erlProject2).exists();
      Matcher<Boolean> _is_2 = Matchers.<Boolean>is(Boolean.valueOf(false));
      MatcherAssert.<Boolean>assertThat(Boolean.valueOf(_exists), _is_2);
      this.setUpProject("Test002");
      boolean _exists_1 = ((IHandle) erlProject2).exists();
      Matcher<Boolean> _is_3 = Matchers.<Boolean>is(Boolean.valueOf(true));
      MatcherAssert.<Boolean>assertThat(Boolean.valueOf(_exists_1), _is_3);
      Iterable<IErlProject> _projects = erlModel.getProjects();
      erlProjects = _projects;
      final Iterable<IErlProject> _converted_erlProjects_1 = (Iterable<IErlProject>)erlProjects;
      int _length_1 = ((Object[])Conversions.unwrapArray(_converted_erlProjects_1, Object.class)).length;
      Matcher<Integer> _is_4 = Matchers.<Integer>is(Integer.valueOf(2));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_1), _is_4);
      final Function1<IErlProject, Boolean> _function = new Function1<IErlProject, Boolean>() {
        public Boolean apply(final IErlProject it) {
          return Boolean.valueOf(Objects.equal(it, erlProject));
        }
      };
      boolean _exists_2 = IterableExtensions.<IErlProject>exists(erlProjects, _function);
      Matcher<Boolean> _is_5 = Matchers.<Boolean>is(Boolean.valueOf(true));
      MatcherAssert.<Boolean>assertThat(Boolean.valueOf(_exists_2), _is_5);
      final Function1<IErlProject, Boolean> _function_1 = new Function1<IErlProject, Boolean>() {
        public Boolean apply(final IErlProject it) {
          return Boolean.valueOf(Objects.equal(it, erlProject2));
        }
      };
      boolean _exists_3 = IterableExtensions.<IErlProject>exists(erlProjects, _function_1);
      Matcher<Boolean> _is_6 = Matchers.<Boolean>is(Boolean.valueOf(true));
      MatcherAssert.<Boolean>assertThat(Boolean.valueOf(_exists_3), _is_6);
      final Iterable<IErlSource> erlFiles = erlProject.getSourceFiles();
      int _length_2 = ((Object[])Conversions.unwrapArray(erlFiles, Object.class)).length;
      Matcher<Integer> _is_7 = Matchers.<Integer>is(Integer.valueOf(1));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_2), _is_7);
      final IErlSource erlFile = IterableExtensions.<IErlSource>head(erlFiles);
      String _name_1 = erlFile.getName();
      Matcher<String> _is_8 = Matchers.<String>is("nop.erl");
      MatcherAssert.<String>assertThat(_name_1, _is_8);
      final Iterable<IErlSource> erlFiles2 = erlProject2.getSourceFiles();
      int _length_3 = ((Object[])Conversions.unwrapArray(erlFiles2, Object.class)).length;
      Matcher<Integer> _is_9 = Matchers.<Integer>is(Integer.valueOf(1));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_3), _is_9);
      final IErlSource erlFile2 = IterableExtensions.<IErlSource>head(erlFiles2);
      String _name_2 = erlFile2.getName();
      Matcher<String> _is_10 = Matchers.<String>is("nop.erl");
      MatcherAssert.<String>assertThat(_name_2, _is_10);
      IFile _file = erlFile.getFile();
      _file.delete(true, null);
      boolean _exists_4 = erlFile.exists();
      Matcher<Boolean> _is_11 = Matchers.<Boolean>is(Boolean.valueOf(false));
      MatcherAssert.<Boolean>assertThat(Boolean.valueOf(_exists_4), _is_11);
      IHandle _parent = erlFile.getParent();
      IHandle[] _children = _parent.getChildren();
      int _length_4 = _children.length;
      Matcher<Integer> _is_12 = Matchers.<Integer>is(Integer.valueOf(0));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_4), _is_12);
      IFile _file_1 = erlFile2.getFile();
      Path _path = new Path("/Test001/nop.erl");
      _file_1.move(_path, true, null);
      boolean _exists_5 = erlFile2.exists();
      Matcher<Boolean> _is_13 = Matchers.<Boolean>is(Boolean.valueOf(false));
      MatcherAssert.<Boolean>assertThat(Boolean.valueOf(_exists_5), _is_13);
      Iterable<IErlSource> _sourceFiles = erlProject2.getSourceFiles();
      int _length_5 = ((Object[])Conversions.unwrapArray(_sourceFiles, Object.class)).length;
      Matcher<Integer> _is_14 = Matchers.<Integer>is(Integer.valueOf(0));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_5), _is_14);
      Iterable<IErlSource> _sourceFiles_1 = erlProject.getSourceFiles();
      int _length_6 = ((Object[])Conversions.unwrapArray(_sourceFiles_1, Object.class)).length;
      Matcher<Integer> _is_15 = Matchers.<Integer>is(Integer.valueOf(1));
      MatcherAssert.<Integer>assertThat(Integer.valueOf(_length_6), _is_15);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
