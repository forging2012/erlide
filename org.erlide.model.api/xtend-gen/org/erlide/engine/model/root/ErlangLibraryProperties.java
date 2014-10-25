package org.erlide.engine.model.root;

import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collection;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend.lib.annotations.EqualsHashCode;
import org.eclipse.xtend.lib.annotations.ToString;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

@Accessors
@EqualsHashCode
@ToString
@SuppressWarnings("all")
public class ErlangLibraryProperties {
  private IPath baseDir;
  
  private Collection<IPath> sourceDirs;
  
  private Collection<IPath> includeDirs;
  
  private RuntimeVersion requiredRuntimeVersion;
  
  public ErlangLibraryProperties() {
    Path _path = new Path("");
    this.baseDir = _path;
    ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList();
    this.sourceDirs = _newArrayList;
    ArrayList<IPath> _newArrayList_1 = CollectionLiterals.<IPath>newArrayList();
    this.includeDirs = _newArrayList_1;
    this.requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION;
  }
  
  public ErlangLibraryProperties(final IPath baseDir, final Collection<IPath> sourceDirs, final Collection<IPath> includeDirs, final RuntimeVersion requiredRuntimeVersion) {
    this.baseDir = baseDir;
    this.sourceDirs = sourceDirs;
    this.includeDirs = includeDirs;
    this.requiredRuntimeVersion = requiredRuntimeVersion;
  }
  
  public void setSourceDirs(final Collection<IPath> dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.sourceDirs = _newArrayList;
  }
  
  public void setSourceDirs(final IPath... dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.sourceDirs = _newArrayList;
  }
  
  public void setIncludeDirs(final Collection<IPath> dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.includeDirs = _newArrayList;
  }
  
  public void setIncludeDirs(final IPath... dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.includeDirs = _newArrayList;
  }
  
  @Pure
  public IPath getBaseDir() {
    return this.baseDir;
  }
  
  public void setBaseDir(final IPath baseDir) {
    this.baseDir = baseDir;
  }
  
  @Pure
  public Collection<IPath> getSourceDirs() {
    return this.sourceDirs;
  }
  
  @Pure
  public Collection<IPath> getIncludeDirs() {
    return this.includeDirs;
  }
  
  @Pure
  public RuntimeVersion getRequiredRuntimeVersion() {
    return this.requiredRuntimeVersion;
  }
  
  public void setRequiredRuntimeVersion(final RuntimeVersion requiredRuntimeVersion) {
    this.requiredRuntimeVersion = requiredRuntimeVersion;
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
    ErlangLibraryProperties other = (ErlangLibraryProperties) obj;
    if (this.baseDir == null) {
      if (other.baseDir != null)
        return false;
    } else if (!this.baseDir.equals(other.baseDir))
      return false;
    if (this.sourceDirs == null) {
      if (other.sourceDirs != null)
        return false;
    } else if (!this.sourceDirs.equals(other.sourceDirs))
      return false;
    if (this.includeDirs == null) {
      if (other.includeDirs != null)
        return false;
    } else if (!this.includeDirs.equals(other.includeDirs))
      return false;
    if (this.requiredRuntimeVersion == null) {
      if (other.requiredRuntimeVersion != null)
        return false;
    } else if (!this.requiredRuntimeVersion.equals(other.requiredRuntimeVersion))
      return false;
    return true;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.baseDir== null) ? 0 : this.baseDir.hashCode());
    result = prime * result + ((this.sourceDirs== null) ? 0 : this.sourceDirs.hashCode());
    result = prime * result + ((this.includeDirs== null) ? 0 : this.includeDirs.hashCode());
    result = prime * result + ((this.requiredRuntimeVersion== null) ? 0 : this.requiredRuntimeVersion.hashCode());
    return result;
  }
  
  @Override
  @Pure
  public String toString() {
    ToStringBuilder b = new ToStringBuilder(this);
    b.add("baseDir", this.baseDir);
    b.add("sourceDirs", this.sourceDirs);
    b.add("includeDirs", this.includeDirs);
    b.add("requiredRuntimeVersion", this.requiredRuntimeVersion);
    return b.toString();
  }
}
