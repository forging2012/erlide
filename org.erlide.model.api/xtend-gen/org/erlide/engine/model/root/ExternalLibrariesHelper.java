package org.erlide.engine.model.root;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import java.util.ArrayList;
import java.util.Collection;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pure;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.model.root.ExternalKind;
import org.erlide.util.PreferencesUtils;

@Accessors
@SuppressWarnings("all")
public class ExternalLibrariesHelper {
  private String externalModulesFile;
  
  private String externalIncludesFile;
  
  public ExternalLibrariesHelper(final String mods, final String incs) {
    this.externalModulesFile = mods;
    this.externalIncludesFile = incs;
  }
  
  public String getExternalIncludes() {
    return this.getExternal(ExternalKind.EXTERNAL_INCLUDES);
  }
  
  public String getExternalModules() {
    return this.getExternal(ExternalKind.EXTERNAL_MODULES);
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
    String result = this.getExternal(external, service, key, "org.erlide.model");
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
  
  public Collection<ErlangLibraryProperties> build() {
    ArrayList<ErlangLibraryProperties> _xblockexpression = null;
    {
      final String mods = this.getExternalModules();
      final String incs = this.getExternalIncludes();
      _xblockexpression = CollectionLiterals.<ErlangLibraryProperties>newArrayList();
    }
    return _xblockexpression;
  }
  
  private void expand(final String string) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  @Pure
  public String getExternalModulesFile() {
    return this.externalModulesFile;
  }
  
  public void setExternalModulesFile(final String externalModulesFile) {
    this.externalModulesFile = externalModulesFile;
  }
  
  @Pure
  public String getExternalIncludesFile() {
    return this.externalIncludesFile;
  }
  
  public void setExternalIncludesFile(final String externalIncludesFile) {
    this.externalIncludesFile = externalIncludesFile;
  }
}
