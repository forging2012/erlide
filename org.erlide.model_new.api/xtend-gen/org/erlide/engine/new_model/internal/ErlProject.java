package org.erlide.engine.new_model.internal;

import com.google.common.base.Objects;
import java.net.URI;
import java.text.MessageFormat;
import java.util.List;
import java.util.Map;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.CollectionExtensions;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.NewModelActivator;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.new_model.IErlFolder;
import org.erlide.engine.new_model.IErlLibrary;
import org.erlide.engine.new_model.IErlModule;
import org.erlide.engine.new_model.IErlOtpLibrary;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.engine.new_model.internal.ErlHeader;
import org.erlide.engine.new_model.internal.ErlLibrary;
import org.erlide.engine.new_model.internal.ErlModel;
import org.erlide.engine.new_model.internal.ErlModule;
import org.erlide.engine.new_model.internal.ErlProjectBody;
import org.erlide.engine.new_model.internal.ErlSource;

@Data
@SuppressWarnings("all")
public class ErlProject extends ErlLibrary implements IErlProject {
  private transient IProject workspaceProject;
  
  public ErlProject(final ErlModel parent, final IProject workspaceProject, final ErlangLibraryProperties properties) {
    super(parent, workspaceProject.getName(), properties);
    this.workspaceProject = workspaceProject;
  }
  
  public IProject getWorkspaceProject() {
    return this.workspaceProject;
  }
  
  public void create(final IProgressMonitor monitor) throws CoreException {
    this.create(null, monitor);
  }
  
  public void create(final URI location, final IProgressMonitor monitor) {
    try {
      IHandle _parent = this.getParent();
      final IWorkspace workspace = ((ErlModel) _parent).getWorkspace();
      workspace.run(
        new IWorkspaceRunnable() {
          public void run(final IProgressMonitor monitor0) {
            try {
              IProgressMonitor _elvis = null;
              if (monitor0 != null) {
                _elvis = monitor0;
              } else {
                NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
                _elvis = _nullProgressMonitor;
              }
              final IProgressMonitor monitor = _elvis;
              try {
                monitor.beginTask("", 4);
                final IProjectDescription description = workspace.newProjectDescription(ErlProject.this.name);
                description.setLocationURI(location);
                SubProgressMonitor _subProgressMonitor = new SubProgressMonitor(monitor, 1);
                ErlProject.this.workspaceProject.create(description, _subProgressMonitor);
                SubProgressMonitor _subProgressMonitor_1 = new SubProgressMonitor(monitor, 1);
                ErlProject.this.workspaceProject.open(_subProgressMonitor_1);
                description.setNatureIds(new String[] { IErlProject.NATURE_ID });
                SubProgressMonitor _subProgressMonitor_2 = new SubProgressMonitor(monitor, 1);
                ErlProject.this.workspaceProject.setDescription(description, _subProgressMonitor_2);
                SubProgressMonitor _subProgressMonitor_3 = new SubProgressMonitor(monitor, 1);
                ErlProject.this.workspaceProject.setDefaultCharset("UTF-8", _subProgressMonitor_3);
              } finally {
                monitor.done();
              }
            } catch (Throwable _e) {
              throw Exceptions.sneakyThrow(_e);
            }
          }
        }, monitor);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IResource getResource() {
    return this.workspaceProject;
  }
  
  public ErlangProjectProperties getProjectProperties() {
    ErlangLibraryProperties _properties = this.getProperties();
    return ((ErlangProjectProperties) _properties);
  }
  
  public void buildStructure(final Body body, final Map<IHandle, Body> newElements) throws CoreException {
    final IResource[] members = this.workspaceProject.members();
    final List<IErlSource> erlFiles = CollectionLiterals.<IErlSource>newArrayList();
    for (final IResource file : members) {
      if ((file instanceof IFile)) {
        final IErlSource source = this.createSourceFile(((IFile)file));
        boolean _tripleNotEquals = (source != null);
        if (_tripleNotEquals) {
          erlFiles.add(source);
        }
      }
    }
    body.setChildren(((IHandle[])Conversions.unwrapArray(erlFiles, IHandle.class)));
  }
  
  private IErlSource createSourceFile(final IFile file) {
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
  
  public IResource[] getNonErlResources() {
    try {
      Body _body = this.getBody();
      return ((IResource[])Conversions.unwrapArray(((ErlProjectBody) _body).getNonErlResources(this), IResource.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IErlSource getSourceFile(final String name) {
    IFile _file = this.workspaceProject.getFile(name);
    return this.createSourceFile(_file);
  }
  
  public Iterable<IErlModule> getModules() {
    Iterable<IErlFolder> _sourceFolders = this.getSourceFolders();
    return this.<IErlModule>getDeepChildren(_sourceFolders, IErlModule.class);
  }
  
  public IErlOtpLibrary getOtpLibrary() {
    return null;
  }
  
  public Iterable<IErlLibrary> getLibraries() {
    return null;
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
  
  public Body newBody() {
    return new ErlProjectBody();
  }
  
  public <T extends IHandle> Iterable<T> getDeepChildren(final Iterable<IErlFolder> folders, final Class<T> clazz) {
    try {
      List<T> _xblockexpression = null;
      {
        final List<T> result = CollectionLiterals.<T>newArrayList();
        for (final IErlFolder folder : folders) {
          T[] _children = folder.<T>getChildren(clazz);
          CollectionExtensions.<T>addAll(result, _children);
        }
        _xblockexpression = result;
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
    ErlProject other = (ErlProject) obj;
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
