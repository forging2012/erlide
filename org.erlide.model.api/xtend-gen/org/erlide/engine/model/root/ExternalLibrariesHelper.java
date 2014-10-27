package org.erlide.engine.model.root;

import com.google.common.base.Charsets;
import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.io.Files;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
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
  
  public Collection<ErlangLibraryProperties> build(final IPath baseDir) {
    ArrayList<ErlangLibraryProperties> _xblockexpression = null;
    {
      final String mods = this.getExternalModules();
      final String incs = this.getExternalIncludes();
      Collection<String> _expand_it = this.expand_it(mods, baseDir);
      final Collection<IPath> allMods = this.justFolders(_expand_it);
      Collection<String> _expand_it_1 = this.expand_it(incs, baseDir);
      final Collection<IPath> allIncs = this.justFolders(_expand_it_1);
      Collection<ErlangLibraryProperties> _merge = this.merge(allMods, allIncs);
      _xblockexpression = CollectionLiterals.<ErlangLibraryProperties>newArrayList(((ErlangLibraryProperties[])Conversions.unwrapArray(_merge, ErlangLibraryProperties.class)));
    }
    return _xblockexpression;
  }
  
  public Collection<String> expand_it(final String fileName, final IPath baseDir) {
    Collection<String> _xblockexpression = null;
    {
      boolean _isNullOrEmpty = Strings.isNullOrEmpty(fileName);
      if (_isNullOrEmpty) {
        return CollectionLiterals.<String>newArrayList();
      }
      final Function1<String, List<String>> _function = new Function1<String, List<String>>() {
        public List<String> apply(final String it) {
          try {
            List<String> _xblockexpression = null;
            {
              final Path p = new Path(fileName);
              String _xifexpression = null;
              boolean _isAbsolute = p.isAbsolute();
              if (_isAbsolute) {
                _xifexpression = fileName;
              } else {
                IPath _append = baseDir.append(fileName);
                _xifexpression = _append.toPortableString();
              }
              final String name = _xifexpression;
              final File f = new File(name);
              _xblockexpression = Files.readLines(f, Charsets.UTF_8);
            }
            return _xblockexpression;
          } catch (Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
          }
        }
      };
      _xblockexpression = this.expand_it(fileName, baseDir, _function);
    }
    return _xblockexpression;
  }
  
  public Collection<String> expand_it(final String fileName, final IPath baseDir, final Function1<? super String, ? extends Collection<String>> xexpander) {
    LinkedHashSet<String> _xblockexpression = null;
    {
      final Collection<String> content = xexpander.apply(fileName);
      final Function1<String, Collection<String>> _function = new Function1<String, Collection<String>>() {
        public Collection<String> apply(final String it) {
          return ExternalLibrariesHelper.this.expand(it, baseDir, xexpander);
        }
      };
      Iterable<Collection<String>> _map = IterableExtensions.<String, Collection<String>>map(content, _function);
      Iterable<String> _flatten = Iterables.<String>concat(_map);
      _xblockexpression = CollectionLiterals.<String>newLinkedHashSet(((String[])Conversions.unwrapArray(_flatten, String.class)));
    }
    return _xblockexpression;
  }
  
  public Collection<String> expand(final String fileName, final IPath baseDir, final Function1<? super String, ? extends Collection<String>> xexpander) {
    ArrayList<String> _xblockexpression = null;
    {
      final ArrayList<String> result = CollectionLiterals.<String>newArrayList();
      boolean _isNullOrEmpty = Strings.isNullOrEmpty(fileName);
      if (_isNullOrEmpty) {
        return result;
      }
      boolean _endsWith = fileName.endsWith(".erlidex");
      if (_endsWith) {
        final Collection<String> expanded = this.expand_it(fileName, baseDir, xexpander);
        result.addAll(expanded);
      } else {
        result.add(fileName);
      }
      _xblockexpression = result;
    }
    return _xblockexpression;
  }
  
  public Collection<IPath> justFolders(final Iterable<String> files) {
    final Function1<String, IPath> _function = new Function1<String, IPath>() {
      public IPath apply(final String it) {
        Path _path = new Path(it);
        return _path.removeLastSegments(1);
      }
    };
    Iterable<IPath> _map = IterableExtensions.<String, IPath>map(files, _function);
    return CollectionLiterals.<IPath>newLinkedHashSet(((IPath[])Conversions.unwrapArray(_map, IPath.class)));
  }
  
  public Map<IPath, List<IPath>> group(final Iterable<IPath> paths) {
    final Function1<IPath, IPath> _function = new Function1<IPath, IPath>() {
      public IPath apply(final IPath it) {
        return it.removeLastSegments(1);
      }
    };
    return IterableExtensions.<IPath, IPath>groupBy(paths, _function);
  }
  
  public Collection<ErlangLibraryProperties> merge(final Iterable<IPath> mods, final Iterable<IPath> incs) {
    ArrayList<ErlangLibraryProperties> _xblockexpression = null;
    {
      final Map<IPath, List<IPath>> grouped = CollectionLiterals.<IPath, List<IPath>>newHashMap();
      for (final IPath mod : mods) {
        {
          final IPath mroot = mod.removeLastSegments(1);
          final List<IPath> matching = CollectionLiterals.<IPath>newArrayList();
          for (final IPath inc : incs) {
            {
              final IPath iroot = inc.removeLastSegments(1);
              boolean _equals = Objects.equal(mroot, iroot);
              if (_equals) {
                matching.add(inc);
              }
            }
          }
          grouped.put(mod, matching);
        }
      }
      _xblockexpression = CollectionLiterals.<ErlangLibraryProperties>newArrayList();
    }
    return _xblockexpression;
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
