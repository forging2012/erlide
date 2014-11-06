package org.erlide.engine.new_model.internal;

import java.util.List;
import java.util.Map;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.HandleManager;
import org.eclipse.handly.model.impl.SourceElementBody;
import org.eclipse.handly.model.impl.SourceFile;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.new_model.IErlComment;
import org.erlide.engine.new_model.IErlForm;
import org.erlide.engine.new_model.IErlModule;
import org.erlide.engine.new_model.internal.ErlModelManager;
import org.erlide.engine.new_model.internal.ErlProject;

@Data
@SuppressWarnings("all")
public abstract class ErlSource extends SourceFile implements IErlModule {
  public ErlSource(final ErlProject parent, final IFile file) {
    super(parent, file);
  }
  
  protected void buildStructure(final SourceElementBody body, final Map<IHandle, Body> newElements, final Object ast, final String source) {
  }
  
  protected Object createStructuralAst(final String source) throws CoreException {
    return null;
  }
  
  protected HandleManager getHandleManager() {
    return ErlModelManager.INSTANCE.getHandleManager();
  }
  
  public Iterable<IErlForm> getForms() {
    try {
      IHandle[] _children = this.getChildren();
      final Function1<IHandle, IErlForm> _function = new Function1<IHandle, IErlForm>() {
        public IErlForm apply(final IHandle it) {
          return ((IErlForm) it);
        }
      };
      return ListExtensions.<IHandle, IErlForm>map(((List<IHandle>)Conversions.doWrapArray(_children)), _function);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IErlComment getHeaderComment() {
    try {
      IErlComment _xblockexpression = null;
      {
        IHandle[] _children = this.getChildren();
        final IHandle first = IterableExtensions.<IHandle>head(((Iterable<IHandle>)Conversions.doWrapArray(_children)));
        IErlComment _xifexpression = null;
        if ((first instanceof IErlComment)) {
          _xifexpression = ((IErlComment)first);
        } else {
          _xifexpression = null;
        }
        _xblockexpression = _xifexpression;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    return result;
  }
  
  @Override
  @Pure
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    if (!super.equals(obj))
      return false;
    ErlSource other = (ErlSource) obj;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    String result = new ToStringBuilder(this)
    	.addAllFields()
    	.toString();
    return result;
  }
}
