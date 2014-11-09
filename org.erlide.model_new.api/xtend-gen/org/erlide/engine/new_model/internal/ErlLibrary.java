package org.erlide.engine.new_model.internal;

import java.util.Map;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.new_model.IErlFolder;
import org.erlide.engine.new_model.IErlLibrary;
import org.erlide.engine.new_model.internal.ErlElement;
import org.erlide.engine.new_model.internal.ErlModel;

@Data
@SuppressWarnings("all")
public class ErlLibrary extends ErlElement implements IErlLibrary {
  private final ErlangLibraryProperties properties;
  
  public ErlLibrary(final ErlModel parent, final String name, final ErlangLibraryProperties properties) {
    super(parent, name);
    boolean _tripleEquals = (parent == null);
    if (_tripleEquals) {
      throw new IllegalArgumentException();
    }
    this.properties = properties;
  }
  
  protected void buildStructure(final Body body, final Map<IHandle, Body> newElements) throws CoreException {
    throw new UnsupportedOperationException("auto-generated method stub");
  }
  
  protected void validateExistence() throws CoreException {
  }
  
  public IResource getResource() {
    return null;
  }
  
  public Iterable<IErlFolder> getFolders() {
    throw new UnsupportedOperationException("auto-generated method stub");
  }
  
  public Iterable<IErlFolder> getSourceFolders() {
    throw new UnsupportedOperationException("auto-generated method stub");
  }
  
  public Iterable<IErlFolder> getIncludeFolders() {
    throw new UnsupportedOperationException("auto-generated method stub");
  }
  
  public IErlFolder getBinaryFolder() {
    throw new UnsupportedOperationException("auto-generated method stub");
  }
  
  public Iterable<IErlLibrary> getLibraries() {
    throw new UnsupportedOperationException("auto-generated method stub");
  }
  
  public Iterable<IResource> getNonErlangResources() {
    throw new UnsupportedOperationException("auto-generated method stub");
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.properties== null) ? 0 : this.properties.hashCode());
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
    if (this.properties == null) {
      if (other.properties != null)
        return false;
    } else if (!this.properties.equals(other.properties))
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
  public ErlangLibraryProperties getProperties() {
    return this.properties;
  }
}
