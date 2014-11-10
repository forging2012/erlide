package org.erlide.engine.new_model.internal;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlFolder;
import org.erlide.engine.new_model.IErlLibrary;
import org.erlide.engine.new_model.internal.ErlElement;

@Data
@SuppressWarnings("all")
public class ErlLibrary extends ErlElement implements IErlLibrary {
  private transient ErlangLibraryProperties properties;
  
  public ErlLibrary(final ErlElement parent, final String name, final ErlangLibraryProperties properties) {
    super(parent, name);
    boolean _tripleEquals = (parent == null);
    if (_tripleEquals) {
      throw new IllegalArgumentException();
    }
    this.properties = properties;
  }
  
  public ErlangLibraryProperties getProperties() {
    return this.properties;
  }
  
  protected void buildStructure(final Body body, final Map<IHandle, Body> newElements) throws CoreException {
  }
  
  protected void validateExistence() throws CoreException {
  }
  
  public IResource getResource() {
    return null;
  }
  
  public Iterable<IErlFolder> getFolders() {
    return null;
  }
  
  public Iterable<IErlFolder> getSourceFolders() {
    List<IErlFolder> _xblockexpression = null;
    {
      final List<IErlFolder> result = CollectionLiterals.<IErlFolder>newArrayList();
      Collection<IPath> _sourceDirs = this.properties.getSourceDirs();
      for (final IPath path : _sourceDirs) {
        {
          IWorkspace _workspace = ResourcesPlugin.getWorkspace();
          IWorkspaceRoot _root = _workspace.getRoot();
          IPath _baseDir = this.properties.getBaseDir();
          IPath _append = _baseDir.append(path);
          IResource _findMember = _root.findMember(_append);
          final IErlElement element = ErlModelCore.create(_findMember);
          if ((element instanceof IErlFolder)) {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("ADD FOLDER ");
            _builder.append(((IErlFolder)element), "");
            InputOutput.<String>println(_builder.toString());
            result.add(((IErlFolder)element));
          }
        }
      }
      _xblockexpression = result;
    }
    return _xblockexpression;
  }
  
  public Iterable<IErlFolder> getIncludeFolders() {
    return null;
  }
  
  public IErlFolder getBinaryFolder() {
    return null;
  }
  
  public IErlLibrary getLibrary(final String name) {
    return new ErlLibrary(this, name, ErlangProjectProperties.DEFAULT);
  }
  
  public Iterable<IErlLibrary> getLibraries() {
    return null;
  }
  
  public Iterable<IResource> getNonErlangResources() {
    return null;
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
    ErlLibrary other = (ErlLibrary) obj;
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
