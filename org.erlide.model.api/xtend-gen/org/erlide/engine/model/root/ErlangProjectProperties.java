package org.erlide.engine.model.root;

import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collection;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend.lib.annotations.AccessorType;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend.lib.annotations.EqualsHashCode;
import org.eclipse.xtend.lib.annotations.ToString;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.model.root.ExternalLibrariesHelper;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

@Accessors
@EqualsHashCode
@ToString
@SuppressWarnings("all")
public class ErlangProjectProperties extends ErlangLibraryProperties {
  private Collection<IPath> testDirs;
  
  @Accessors(AccessorType.NONE)
  private final transient ExternalLibrariesHelper externalLibrariesHelper;
  
  public final static ErlangProjectProperties DEFAULT = ObjectExtensions.<ErlangProjectProperties>operator_doubleArrow(new ErlangProjectProperties(), new Procedure1<ErlangProjectProperties>() {
    public void apply(final ErlangProjectProperties it) {
      Collection<IPath> _unpackList = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
      it.setSourceDirs(_unpackList);
      Path _path = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
      it.setEbinDir(_path);
      Collection<IPath> _unpackList_1 = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
      it.setIncludeDirs(_unpackList_1);
      Collection<IPath> _unpackList_2 = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_TEST_DIRS);
      it.testDirs = _unpackList_2;
      it.setRequiredRuntimeVersion(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
      final String externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
      final String externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
      it.externalLibrariesHelper.setExternalModulesFile(externalModulesFile);
      it.externalLibrariesHelper.setExternalIncludesFile(externalIncludesFile);
    }
  });
  
  public ErlangProjectProperties() {
    super();
    ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList();
    this.testDirs = _newArrayList;
    ExternalLibrariesHelper _externalLibrariesHelper = new ExternalLibrariesHelper();
    this.externalLibrariesHelper = _externalLibrariesHelper;
  }
  
  public void copyFrom(final ErlangProjectProperties props) {
    Collection<IPath> _includeDirs = props.getIncludeDirs();
    this.setIncludeDirs(_includeDirs);
    this.testDirs = props.testDirs;
    Collection<IPath> _sourceDirs = props.getSourceDirs();
    this.setSourceDirs(_sourceDirs);
    IPath _ebinDir = props.getEbinDir();
    this.setEbinDir(_ebinDir);
    RuntimeVersion _requiredRuntimeVersion = props.getRequiredRuntimeVersion();
    this.setRequiredRuntimeVersion(_requiredRuntimeVersion);
    String _externalIncludesFile = props.getExternalIncludesFile();
    this.setExternalIncludesFile(_externalIncludesFile);
    String _externalModulesFile = props.getExternalModulesFile();
    this.setExternalModulesFile(_externalModulesFile);
  }
  
  public void setTestDirs(final Collection<IPath> dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.testDirs = _newArrayList;
  }
  
  public void setTestDirs(final IPath... dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.testDirs = _newArrayList;
  }
  
  public String getExternalIncludes() {
    return this.externalLibrariesHelper.getExternalIncludes();
  }
  
  public String getExternalModules() {
    return this.externalLibrariesHelper.getExternalModules();
  }
  
  public void setExternalModulesFile(final String mods) {
    this.externalLibrariesHelper.setExternalModulesFile(mods);
  }
  
  public void setExternalIncludesFile(final String incs) {
    this.externalLibrariesHelper.setExternalIncludesFile(incs);
  }
  
  public String getExternalModulesFile() {
    return this.externalLibrariesHelper.getExternalModulesFile();
  }
  
  public String getExternalIncludesFile() {
    return this.externalLibrariesHelper.getExternalIncludesFile();
  }
  
  public void setBaseDir(final IPath baseDir) {
    super.setBaseDir(baseDir);
  }
  
  public Collection<ErlangLibraryProperties> getLibraries() {
    IPath _baseDir = this.getBaseDir();
    return this.externalLibrariesHelper.build(_baseDir);
  }
  
  public IPath getOutputDir() {
    return this.getEbinDir();
  }
  
  public void setOutputDir(final IPath dir) {
    this.setEbinDir(dir);
  }
  
  @Pure
  public Collection<IPath> getTestDirs() {
    return this.testDirs;
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
    ErlangProjectProperties other = (ErlangProjectProperties) obj;
    if (this.testDirs == null) {
      if (other.testDirs != null)
        return false;
    } else if (!this.testDirs.equals(other.testDirs))
      return false;
    return true;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.testDirs== null) ? 0 : this.testDirs.hashCode());
    return result;
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
