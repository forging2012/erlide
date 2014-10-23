package org.erlide.engine.model.root;

import com.google.common.base.Charsets;
import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.io.Files;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
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
      final Collection<String> allMods = this.expand_it(mods);
      final Collection<String> allIncs = this.expand_it(incs);
      Collection<ErlangLibraryProperties> _merge = this.merge(allMods, allIncs);
      _xblockexpression = CollectionLiterals.<ErlangLibraryProperties>newArrayList(((ErlangLibraryProperties[])Conversions.unwrapArray(_merge, ErlangLibraryProperties.class)));
    }
    return _xblockexpression;
  }
  
  public Collection<String> expand_it(final String fileName) {
    try {
      Collection<String> _xblockexpression = null;
      {
        boolean _isNullOrEmpty = Strings.isNullOrEmpty(fileName);
        if (_isNullOrEmpty) {
          return CollectionLiterals.<String>newArrayList();
        }
        File _file = new File(fileName);
        final List<String> input = Files.readLines(_file, Charsets.UTF_8);
        final Function1<String, List<String>> _function = new Function1<String, List<String>>() {
          public List<String> apply(final String it) {
            return input;
          }
        };
        _xblockexpression = this.expand_it(fileName, _function);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public Collection<String> expand_it(final String fileName, final Function1<? super String, ? extends Collection<String>> xexpander) {
    ArrayList<String> _xblockexpression = null;
    {
      final Collection<String> content = xexpander.apply(fileName);
      final Function1<String, Collection<String>> _function = new Function1<String, Collection<String>>() {
        public Collection<String> apply(final String it) {
          return ExternalLibrariesHelper.this.expand(it, xexpander);
        }
      };
      Iterable<Collection<String>> _map = IterableExtensions.<String, Collection<String>>map(content, _function);
      Iterable<String> _flatten = Iterables.<String>concat(_map);
      _xblockexpression = CollectionLiterals.<String>newArrayList(((String[])Conversions.unwrapArray(_flatten, String.class)));
    }
    return _xblockexpression;
  }
  
  public Collection<String> expand(final String fileName, final Function1<? super String, ? extends Collection<String>> xexpander) {
    ArrayList<String> _xblockexpression = null;
    {
      final ArrayList<String> result = CollectionLiterals.<String>newArrayList();
      boolean _isNullOrEmpty = Strings.isNullOrEmpty(fileName);
      if (_isNullOrEmpty) {
        return result;
      }
      boolean _endsWith = fileName.endsWith(".erlidex");
      if (_endsWith) {
        final Collection<String> expanded = this.expand_it(fileName, xexpander);
        result.addAll(expanded);
      } else {
        result.add(fileName);
      }
      _xblockexpression = result;
    }
    return _xblockexpression;
  }
  
  public Collection<ErlangLibraryProperties> merge(final Iterable<String> mods, final Iterable<String> incs) {
    return CollectionLiterals.<ErlangLibraryProperties>newArrayList();
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
