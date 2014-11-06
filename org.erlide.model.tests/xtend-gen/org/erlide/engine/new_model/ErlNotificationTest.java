package org.erlide.engine.new_model;

import com.google.common.base.Objects;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.Path;
import org.eclipse.handly.model.IElementChangeEvent;
import org.eclipse.handly.model.IElementChangeListener;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.IHandleDelta;
import org.eclipse.handly.model.impl.HandleDelta;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.testing.utils.WorkspaceTest;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("all")
public class ErlNotificationTest extends WorkspaceTest {
  private static class ErlModelListener implements IElementChangeListener {
    public HandleDelta delta;
    
    public void elementChanged(final IElementChangeEvent event) {
      IHandleDelta _delta = event.getDelta();
      this.delta = ((HandleDelta) _delta);
    }
  }
  
  private final IErlModel erlModel = ErlModelCore.getErlModel();
  
  private final ErlNotificationTest.ErlModelListener listener = new ErlNotificationTest.ErlModelListener();
  
  @Before
  public void setup() {
    try {
      this.setUpProject("Test001");
      this.erlModel.addElementChangeListener(this.listener);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @After
  public void teardown() {
    try {
      this.erlModel.removeElementChangeListener(this.listener);
      super.tearDown();
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void testErlModelNotification() {
    try {
      final IErlProject erlProject1 = this.erlModel.getProject("Test001");
      final IErlProject erlProject2 = this.erlModel.getProject("Test002");
      this.setUpProject("Test002");
      HandleDelta _newDelta = this.newDelta();
      HandleDelta _insertAdded = _newDelta.insertAdded(erlProject2);
      ErlNotificationTest.assertEquality(_insertAdded, this.listener.delta);
      final IErlSource erlFile1 = erlProject1.getSourceFile("nop.erl");
      IFile _file = erlFile1.getFile();
      _file.touch(null);
      HandleDelta _newDelta_1 = this.newDelta();
      HandleDelta _insertChanged = _newDelta_1.insertChanged(erlFile1, HandleDelta.F_CONTENT);
      ErlNotificationTest.assertEquality(_insertChanged, this.listener.delta);
      IFile _file_1 = erlFile1.getFile();
      Path _path = new Path("/Test002/test1.erl");
      _file_1.copy(_path, true, null);
      HandleDelta _newDelta_2 = this.newDelta();
      IErlSource _sourceFile = erlProject2.getSourceFile("test1.erl");
      HandleDelta _insertAdded_1 = _newDelta_2.insertAdded(_sourceFile);
      ErlNotificationTest.assertEquality(_insertAdded_1, this.listener.delta);
      IFile _file_2 = erlFile1.getFile();
      _file_2.delete(true, null);
      HandleDelta _newDelta_3 = this.newDelta();
      HandleDelta _insertRemoved = _newDelta_3.insertRemoved(erlFile1);
      ErlNotificationTest.assertEquality(_insertRemoved, this.listener.delta);
      final IErlSource erlFile2 = erlProject2.getSourceFile("nop.erl");
      final IErlSource movedErlFile2 = erlProject1.getSourceFile("test1.erl");
      IFile _file_3 = erlFile2.getFile();
      Path _path_1 = new Path("/Test001/test1.erl");
      _file_3.move(_path_1, true, null);
      HandleDelta _newDelta_4 = this.newDelta();
      HandleDelta _insertMovedTo = _newDelta_4.insertMovedTo(movedErlFile2, erlFile2);
      HandleDelta _insertMovedFrom = _insertMovedTo.insertMovedFrom(erlFile2, movedErlFile2);
      ErlNotificationTest.assertEquality(_insertMovedFrom, 
        this.listener.delta);
      IProject _workspaceProject = erlProject2.getWorkspaceProject();
      _workspaceProject.close(null);
      HandleDelta _newDelta_5 = this.newDelta();
      HandleDelta _insertRemoved_1 = _newDelta_5.insertRemoved(erlProject2, HandleDelta.F_OPEN);
      ErlNotificationTest.assertEquality(_insertRemoved_1, this.listener.delta);
      IProject _workspaceProject_1 = erlProject2.getWorkspaceProject();
      _workspaceProject_1.open(null);
      HandleDelta _newDelta_6 = this.newDelta();
      HandleDelta _insertAdded_2 = _newDelta_6.insertAdded(erlProject2, HandleDelta.F_OPEN);
      ErlNotificationTest.assertEquality(_insertAdded_2, this.listener.delta);
      IProject _workspaceProject_2 = erlProject2.getWorkspaceProject();
      _workspaceProject_2.delete(true, null);
      HandleDelta _newDelta_7 = this.newDelta();
      HandleDelta _insertRemoved_2 = _newDelta_7.insertRemoved(erlProject2);
      ErlNotificationTest.assertEquality(_insertRemoved_2, this.listener.delta);
      IProject _workspaceProject_3 = erlProject1.getWorkspaceProject();
      final IProjectDescription description = _workspaceProject_3.getDescription();
      final String[] oldNatures = description.getNatureIds();
      description.setNatureIds(new String[] {});
      IProject _workspaceProject_4 = erlProject1.getWorkspaceProject();
      _workspaceProject_4.setDescription(description, null);
      HandleDelta _newDelta_8 = this.newDelta();
      HandleDelta _insertRemoved_3 = _newDelta_8.insertRemoved(erlProject1, HandleDelta.F_DESCRIPTION);
      ErlNotificationTest.assertEquality(_insertRemoved_3, this.listener.delta);
      description.setNatureIds(oldNatures);
      IProject _workspaceProject_5 = erlProject1.getWorkspaceProject();
      _workspaceProject_5.setDescription(description, null);
      HandleDelta _newDelta_9 = this.newDelta();
      HandleDelta _insertAdded_3 = _newDelta_9.insertAdded(erlProject1, HandleDelta.F_DESCRIPTION);
      ErlNotificationTest.assertEquality(_insertAdded_3, this.listener.delta);
      final IErlProject movedErlProject1 = this.erlModel.getProject("Test");
      IProject _workspaceProject_6 = erlProject1.getWorkspaceProject();
      Path _path_2 = new Path("Test");
      _workspaceProject_6.move(_path_2, true, null);
      HandleDelta _newDelta_10 = this.newDelta();
      HandleDelta _insertMovedTo_1 = _newDelta_10.insertMovedTo(movedErlProject1, erlProject1);
      HandleDelta _insertMovedFrom_1 = _insertMovedTo_1.insertMovedFrom(erlProject1, movedErlProject1);
      ErlNotificationTest.assertEquality(_insertMovedFrom_1, 
        this.listener.delta);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  private HandleDelta newDelta() {
    return new HandleDelta(this.erlModel);
  }
  
  private static void assertEquality(final IHandleDelta expected, final IHandleDelta actual) {
    boolean _equals = Objects.equal(expected, null);
    if (_equals) {
      Matcher<Object> _nullValue = Matchers.nullValue();
      Matcher<Object> _is = Matchers.<Object>is(_nullValue);
      MatcherAssert.<IHandleDelta>assertThat(actual, _is);
      return;
    }
    Matcher<Object> _nullValue_1 = Matchers.nullValue();
    Matcher<Object> _not = Matchers.<Object>not(_nullValue_1);
    Matcher<Object> _is_1 = Matchers.<Object>is(_not);
    MatcherAssert.<IHandleDelta>assertThat(actual, _is_1);
    IHandle _element = expected.getElement();
    IHandle _element_1 = actual.getElement();
    Matcher<IHandle> _is_2 = Matchers.<IHandle>is(_element_1);
    MatcherAssert.<IHandle>assertThat(_element, _is_2);
    int _kind = expected.getKind();
    int _kind_1 = actual.getKind();
    Matcher<Integer> _is_3 = Matchers.<Integer>is(Integer.valueOf(_kind_1));
    MatcherAssert.<Integer>assertThat(Integer.valueOf(_kind), _is_3);
    int _flags = expected.getFlags();
    int _flags_1 = actual.getFlags();
    Matcher<Integer> _is_4 = Matchers.<Integer>is(Integer.valueOf(_flags_1));
    MatcherAssert.<Integer>assertThat(Integer.valueOf(_flags), _is_4);
    IHandle _movedToElement = expected.getMovedToElement();
    IHandle _movedToElement_1 = actual.getMovedToElement();
    Matcher<IHandle> _is_5 = Matchers.<IHandle>is(_movedToElement_1);
    MatcherAssert.<IHandle>assertThat(_movedToElement, _is_5);
    IHandle _movedFromElement = expected.getMovedFromElement();
    IHandle _movedFromElement_1 = actual.getMovedFromElement();
    Matcher<IHandle> _is_6 = Matchers.<IHandle>is(_movedFromElement_1);
    MatcherAssert.<IHandle>assertThat(_movedFromElement, _is_6);
    final IHandleDelta[] expectedChildren = expected.getAffectedChildren();
    final IHandleDelta[] actualChildren = actual.getAffectedChildren();
    int _length = expectedChildren.length;
    int _length_1 = actualChildren.length;
    Matcher<Integer> _is_7 = Matchers.<Integer>is(Integer.valueOf(_length_1));
    MatcherAssert.<Integer>assertThat(Integer.valueOf(_length), _is_7);
    for (int i = 0; (i < expectedChildren.length); i++) {
      IHandleDelta _get = expectedChildren[i];
      IHandleDelta _get_1 = actualChildren[i];
      ErlNotificationTest.assertEquality(_get, _get_1);
    }
  }
}
