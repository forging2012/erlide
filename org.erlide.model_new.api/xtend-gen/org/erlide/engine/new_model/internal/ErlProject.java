package org.erlide.engine.new_model.internal;

import com.google.common.base.Objects;
import java.text.MessageFormat;
import java.util.List;
import java.util.Map;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.NewModelActivator;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.new_model.IErlHeader;
import org.erlide.engine.new_model.IErlLibrary;
import org.erlide.engine.new_model.IErlModule;
import org.erlide.engine.new_model.IErlOtpLibrary;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.engine.new_model.internal.ErlHeader;
import org.erlide.engine.new_model.internal.ErlLibrary;
import org.erlide.engine.new_model.internal.ErlModel;
import org.erlide.engine.new_model.internal.ErlModule;
import org.erlide.engine.new_model.internal.ErlSource;

@Data
@SuppressWarnings("all")
public class ErlProject extends ErlLibrary implements IErlProject {
  private final IProject workspaceProject;
  
  public ErlProject(final ErlModel parent, final IProject workspaceProject, final ErlangLibraryProperties properties) {
    super(parent, workspaceProject.getName(), properties);
    this.workspaceProject = workspaceProject;
  }
  
  public IResource getResource() {
    return this.workspaceProject;
  }
  
  public ErlangProjectProperties getProjectProperties() {
    ErlangLibraryProperties _properties = this.getProperties();
    return ((ErlangProjectProperties) _properties);
  }
  
  protected void buildStructure(final Body body, final Map<IHandle, Body> newElements) throws CoreException {
    final IResource[] members = this.workspaceProject.members();
    final List<IErlSource> erlFiles = CollectionLiterals.<IErlSource>newArrayList();
    for (final IResource file : members) {
      if ((file instanceof IFile)) {
        final IErlSource source = this.createInstance(((IFile)file));
        boolean _tripleNotEquals = (source != null);
        if (_tripleNotEquals) {
          erlFiles.add(source);
        }
      }
    }
    body.setChildren(((IHandle[])Conversions.unwrapArray(erlFiles, IHandle.class)));
  }
  
  private ErlSource createInstance(final IFile file) {
    ErlSource _switchResult = null;
    String _fileExtension = file.getFileExtension();
    boolean _matched = false;
    if (!_matched) {
      if (Objects.equal(_fileExtension, "erl")) {
        _matched=true;
        _switchResult = new ErlModule(this, file);
      }
    }
    if (!_matched) {
      if (Objects.equal(_fileExtension, "hrl")) {
        _matched=true;
        _switchResult = new ErlHeader(this, file);
      }
    }
    if (!_matched) {
      _switchResult = null;
    }
    return _switchResult;
  }
  
  public Iterable<IErlSource> getSourceFiles() {
    try {
      return (Iterable<IErlSource>)Conversions.doWrapArray(this.<IErlSource>getChildren(IErlSource.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IErlSource getSourceFile(final String name) {
    IFile _file = this.workspaceProject.getFile(name);
    return this.createInstance(_file);
  }
  
  public Iterable<IErlModule> getModules() {
    try {
      return (Iterable<IErlModule>)Conversions.doWrapArray(this.<IErlModule>getChildren(IErlModule.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public Iterable<IErlHeader> getHeaders() {
    try {
      return (Iterable<IErlHeader>)Conversions.doWrapArray(this.<IErlHeader>getChildren(IErlHeader.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IErlOtpLibrary getOtpLibrary() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public Iterable<IErlLibrary> getLibraries() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  protected void validateExistence() throws CoreException {
    boolean _exists = this.workspaceProject.exists();
    boolean _not = (!_exists);
    if (_not) {
      String _format = MessageFormat.format("Project \'\'{0}\'\' does not exist in workspace", this.name);
      IStatus _createErrorStatus = NewModelActivator.createErrorStatus(_format, null);
      throw new CoreException(_createErrorStatus);
    }
    boolean _isOpen = this.workspaceProject.isOpen();
    boolean _not_1 = (!_isOpen);
    if (_not_1) {
      String _format_1 = MessageFormat.format("Project \'\'{0}\'\' is not open", this.name);
      IStatus _createErrorStatus_1 = NewModelActivator.createErrorStatus(_format_1, null);
      throw new CoreException(_createErrorStatus_1);
    }
    boolean _hasNature = this.workspaceProject.hasNature(IErlProject.NATURE_ID);
    boolean _not_2 = (!_hasNature);
    if (_not_2) {
      String _format_2 = MessageFormat.format("Project \'\'{0}\'\' does not have the erlang nature", this.name);
      IStatus _createErrorStatus_2 = NewModelActivator.createErrorStatus(_format_2, null);
      throw new CoreException(_createErrorStatus_2);
    }
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.workspaceProject== null) ? 0 : this.workspaceProject.hashCode());
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
    ErlProject other = (ErlProject) obj;
    if (this.workspaceProject == null) {
      if (other.workspaceProject != null)
        return false;
    } else if (!this.workspaceProject.equals(other.workspaceProject))
      return false;
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
  
  @Pure
  public IProject getWorkspaceProject() {
    return this.workspaceProject;
  }
}