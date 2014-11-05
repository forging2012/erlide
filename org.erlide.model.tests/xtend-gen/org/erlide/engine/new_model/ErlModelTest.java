package org.erlide.engine.new_model;

import com.google.common.base.Objects;
import org.eclipse.handly.model.IHandle;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Assert;
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
      Iterable<IErlProject> erlProjects = erlModel.getErlProjects();
      final Iterable<IErlProject> _converted_erlProjects = (Iterable<IErlProject>)erlProjects;
      int _length = ((Object[])Conversions.unwrapArray(_converted_erlProjects, Object.class)).length;
      Assert.assertEquals(1, _length);
      final IErlProject erlProject = IterableExtensions.<IErlProject>head(erlProjects);
      String _name = ((IHandle) erlProject).getName();
      Assert.assertEquals("Test001", _name);
      final IErlProject erlProject2 = erlModel.getErlProject("Test002");
      boolean _exists = ((IHandle) erlProject2).exists();
      Assert.assertFalse(_exists);
      this.setUpProject("Test002");
      boolean _exists_1 = ((IHandle) erlProject2).exists();
      Assert.assertTrue(_exists_1);
      Iterable<IErlProject> _erlProjects = erlModel.getErlProjects();
      erlProjects = _erlProjects;
      final Iterable<IErlProject> _converted_erlProjects_1 = (Iterable<IErlProject>)erlProjects;
      int _length_1 = ((Object[])Conversions.unwrapArray(_converted_erlProjects_1, Object.class)).length;
      Assert.assertEquals(2, _length_1);
      final Function1<IErlProject, Boolean> _function = new Function1<IErlProject, Boolean>() {
        public Boolean apply(final IErlProject it) {
          return Boolean.valueOf(Objects.equal(it, erlProject));
        }
      };
      boolean _exists_2 = IterableExtensions.<IErlProject>exists(erlProjects, _function);
      Assert.assertTrue(_exists_2);
      final Function1<IErlProject, Boolean> _function_1 = new Function1<IErlProject, Boolean>() {
        public Boolean apply(final IErlProject it) {
          return Boolean.valueOf(Objects.equal(it, erlProject2));
        }
      };
      boolean _exists_3 = IterableExtensions.<IErlProject>exists(erlProjects, _function_1);
      Assert.assertTrue(_exists_3);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
