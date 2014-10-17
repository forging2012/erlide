package org.erlide.engine.model.root;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import java.util.Collection;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend.lib.annotations.EqualsHashCode;
import org.eclipse.xtend.lib.annotations.ToString;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.model.root.ExternalKind;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.PreferencesUtils;

@Accessors
@EqualsHashCode
@ToString
@SuppressWarnings("all")
public class ErlangProjectProperties extends ErlangLibraryProperties {
  private IPath outputDir;
  
  private String externalIncludesFile;
  
  private String externalModulesFile;
  
  public final static ErlangProjectProperties DEFAULT = ObjectExtensions.<ErlangProjectProperties>operator_doubleArrow(new ErlangProjectProperties(), new Procedure1<ErlangProjectProperties>() {
    public void apply(final ErlangProjectProperties it) {
      Collection<IPath> _unpackList = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
      it.setSourceDirs(_unpackList);
      Path _path = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
      it.outputDir = _path;
      Collection<IPath> _unpackList_1 = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
      it.setIncludeDirs(_unpackList_1);
      Collection<IPath> _unpackList_2 = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_TEST_DIRS);
      it.setTestDirs(_unpackList_2);
      it.externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
      it.externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
      it.setRequiredRuntimeVersion(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    }
  });
  
  public ErlangProjectProperties() {
    super();
    Path _path = new Path("");
    this.outputDir = _path;
    this.externalIncludesFile = "";
    this.externalModulesFile = "";
  }
  
  public void copyFrom(final ErlangProjectProperties props) {
    Collection<IPath> _includeDirs = props.getIncludeDirs();
    this.setIncludeDirs(_includeDirs);
    Collection<IPath> _testDirs = props.getTestDirs();
    this.setTestDirs(_testDirs);
    Collection<IPath> _sourceDirs = props.getSourceDirs();
    this.setSourceDirs(_sourceDirs);
    this.outputDir = props.outputDir;
    RuntimeVersion _requiredRuntimeVersion = props.getRequiredRuntimeVersion();
    this.setRequiredRuntimeVersion(_requiredRuntimeVersion);
    this.externalIncludesFile = props.externalIncludesFile;
    this.externalModulesFile = props.externalModulesFile;
  }
  
  public String getExternalIncludes() {
    final String externalIncludesString = this.getExternal(ExternalKind.EXTERNAL_INCLUDES);
    return externalIncludesString;
  }
  
  public String getExternalModules() {
    final String externalModulesString = this.getExternal(ExternalKind.EXTERNAL_MODULES);
    return externalModulesString;
  }
  
  private String getExternal(final ExternalKind external) {
    final IPreferencesService service = Platform.getPreferencesService();
    String _xifexpression = null;
    boolean _equals = Objects.equal(external, ExternalKind.EXTERNAL_INCLUDES);
    if (_equals) {
      _xifexpression = "default_external_includes";
    } else {
      _xifexpression = "default_external_modules";
    }
    final String key = _xifexpression;
    String result = this.getExternal(external, service, key, "org.erlide.ui");
    boolean _isNullOrEmpty = Strings.isNullOrEmpty(result);
    if (_isNullOrEmpty) {
      String _external = this.getExternal(external, service, key, "org.erlide.core");
      result = _external;
    }
    return result;
  }
  
  private String getExternal(final ExternalKind external, final IPreferencesService service, final String key, final String pluginId) {
    final String global = service.getString(pluginId, key, "", null);
    String _xifexpression = null;
    boolean _equals = Objects.equal(external, ExternalKind.EXTERNAL_INCLUDES);
    if (_equals) {
      _xifexpression = this.externalIncludesFile;
    } else {
      _xifexpression = this.externalModulesFile;
    }
    final String projprefs = _xifexpression;
    return PreferencesUtils.packArray(new String[] { projprefs, global });
  }
  
  @Pure
  public IPath getOutputDir() {
    return this.outputDir;
  }
  
  public void setOutputDir(final IPath outputDir) {
    this.outputDir = outputDir;
  }
  
  @Pure
  public String getExternalIncludesFile() {
    return this.externalIncludesFile;
  }
  
  public void setExternalIncludesFile(final String externalIncludesFile) {
    this.externalIncludesFile = externalIncludesFile;
  }
  
  @Pure
  public String getExternalModulesFile() {
    return this.externalModulesFile;
  }
  
  public void setExternalModulesFile(final String externalModulesFile) {
    this.externalModulesFile = externalModulesFile;
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
    if (this.outputDir == null) {
      if (other.outputDir != null)
        return false;
    } else if (!this.outputDir.equals(other.outputDir))
      return false;
    if (this.externalIncludesFile == null) {
      if (other.externalIncludesFile != null)
        return false;
    } else if (!this.externalIncludesFile.equals(other.externalIncludesFile))
      return false;
    if (this.externalModulesFile == null) {
      if (other.externalModulesFile != null)
        return false;
    } else if (!this.externalModulesFile.equals(other.externalModulesFile))
      return false;
    return true;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.outputDir== null) ? 0 : this.outputDir.hashCode());
    result = prime * result + ((this.externalIncludesFile== null) ? 0 : this.externalIncludesFile.hashCode());
    result = prime * result + ((this.externalModulesFile== null) ? 0 : this.externalModulesFile.hashCode());
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
