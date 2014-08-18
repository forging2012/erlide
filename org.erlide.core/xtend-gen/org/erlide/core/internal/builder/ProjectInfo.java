package org.erlide.core.internal.builder;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.collect.Iterables;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtend.lib.Data;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.util.ToStringHelper;
import org.erlide.core.builder.CompilerOptions;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.erlang.OtpErlang;

@Data
@SuppressWarnings("all")
public class ProjectInfo {
  private final String _rootDir;
  
  public String getRootDir() {
    return this._rootDir;
  }
  
  private final Collection<IPath> _srcDirs;
  
  public Collection<IPath> getSrcDirs() {
    return this._srcDirs;
  }
  
  private final Collection<IPath> _incDirs;
  
  public Collection<IPath> getIncDirs() {
    return this._incDirs;
  }
  
  private final IPath _outDir;
  
  public IPath getOutDir() {
    return this._outDir;
  }
  
  private final OtpErlangList _opts;
  
  public OtpErlangList getOpts() {
    return this._opts;
  }
  
  private final RuntimeVersion _minOtpVsn;
  
  public RuntimeVersion getMinOtpVsn() {
    return this._minOtpVsn;
  }
  
  private final Collection<String> _libs;
  
  public Collection<String> getLibs() {
    return this._libs;
  }
  
  private final Collection<String> _libIncs;
  
  public Collection<String> getLibIncs() {
    return this._libIncs;
  }
  
  public ProjectInfo(final IErlProject project) {
    IProject _workspaceProject = project.getWorkspaceProject();
    IPath _location = _workspaceProject.getLocation();
    String _portableString = _location.toPortableString();
    this._rootDir = _portableString;
    final ErlangProjectProperties props = project.getProperties();
    Collection<IPath> _sourceDirs = props.getSourceDirs();
    this._srcDirs = _sourceDirs;
    Collection<IPath> _includeDirs = props.getIncludeDirs();
    this._incDirs = _includeDirs;
    IPath _outputDir = props.getOutputDir();
    this._outDir = _outputDir;
    IProject _workspaceProject_1 = project.getWorkspaceProject();
    OtpErlangList _get = CompilerOptions.get(_workspaceProject_1);
    this._opts = _get;
    RuntimeVersion _requiredRuntimeVersion = props.getRequiredRuntimeVersion();
    RuntimeVersion _asMajor = _requiredRuntimeVersion.asMajor();
    this._minOtpVsn = _asMajor;
    ArrayList<String> _newArrayList = CollectionLiterals.<String>newArrayList();
    this._libs = _newArrayList;
    ArrayList<String> _newArrayList_1 = CollectionLiterals.<String>newArrayList();
    this._libIncs = _newArrayList_1;
  }
  
  public OtpErlangObject asErlangObject() {
    OtpErlangAtom _otpErlangAtom = new OtpErlangAtom("project_info");
    String _rootDir = this.getRootDir();
    OtpErlangString _otpErlangString = new OtpErlangString(_rootDir);
    Collection<IPath> _srcDirs = this.getSrcDirs();
    final Function1<IPath, OtpErlangString> _function = new Function1<IPath, OtpErlangString>() {
      public OtpErlangString apply(final IPath it) {
        String _portableString = it.toPortableString();
        return new OtpErlangString(_portableString);
      }
    };
    Iterable<OtpErlangString> _map = IterableExtensions.<IPath, OtpErlangString>map(_srcDirs, _function);
    OtpErlangList _mkList = OtpErlang.mkList(((OtpErlangObject[])Conversions.unwrapArray(_map, OtpErlangObject.class)));
    Collection<IPath> _incDirs = this.getIncDirs();
    final Function1<IPath, OtpErlangString> _function_1 = new Function1<IPath, OtpErlangString>() {
      public OtpErlangString apply(final IPath it) {
        String _portableString = it.toPortableString();
        return new OtpErlangString(_portableString);
      }
    };
    Iterable<OtpErlangString> _map_1 = IterableExtensions.<IPath, OtpErlangString>map(_incDirs, _function_1);
    Collection<String> _libIncs = this.getLibIncs();
    final Function1<String, OtpErlangString> _function_2 = new Function1<String, OtpErlangString>() {
      public OtpErlangString apply(final String it) {
        return new OtpErlangString(it);
      }
    };
    Iterable<OtpErlangString> _map_2 = IterableExtensions.<String, OtpErlangString>map(_libIncs, _function_2);
    Iterable<OtpErlangString> _plus = Iterables.<OtpErlangString>concat(_map_1, _map_2);
    OtpErlangList _mkList_1 = OtpErlang.mkList(((OtpErlangObject[])Conversions.unwrapArray(_plus, OtpErlangObject.class)));
    IPath _outDir = this.getOutDir();
    String _portableString = _outDir.toPortableString();
    OtpErlangString _otpErlangString_1 = new OtpErlangString(_portableString);
    OtpErlangList _opts = this.getOpts();
    RuntimeVersion _minOtpVsn = this.getMinOtpVsn();
    String _string = _minOtpVsn.toString();
    OtpErlangString _otpErlangString_2 = new OtpErlangString(_string);
    Collection<String> _libs = this.getLibs();
    final Function1<String, OtpErlangString> _function_3 = new Function1<String, OtpErlangString>() {
      public OtpErlangString apply(final String it) {
        return new OtpErlangString(it);
      }
    };
    Iterable<OtpErlangString> _map_3 = IterableExtensions.<String, OtpErlangString>map(_libs, _function_3);
    OtpErlangList _mkList_2 = OtpErlang.mkList(((OtpErlangObject[])Conversions.unwrapArray(_map_3, OtpErlangObject.class)));
    final List<OtpErlangObject> result = CollectionLiterals.<OtpErlangObject>newArrayList(_otpErlangAtom, _otpErlangString, _mkList, _mkList_1, _otpErlangString_1, _opts, _otpErlangString_2, _mkList_2);
    return OtpErlang.mkTuple(((OtpErlangObject[])Conversions.unwrapArray(result, OtpErlangObject.class)));
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this._rootDir== null) ? 0 : this._rootDir.hashCode());
    result = prime * result + ((this._srcDirs== null) ? 0 : this._srcDirs.hashCode());
    result = prime * result + ((this._incDirs== null) ? 0 : this._incDirs.hashCode());
    result = prime * result + ((this._outDir== null) ? 0 : this._outDir.hashCode());
    result = prime * result + ((this._opts== null) ? 0 : this._opts.hashCode());
    result = prime * result + ((this._minOtpVsn== null) ? 0 : this._minOtpVsn.hashCode());
    result = prime * result + ((this._libs== null) ? 0 : this._libs.hashCode());
    result = prime * result + ((this._libIncs== null) ? 0 : this._libIncs.hashCode());
    return result;
  }
  
  @Override
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    ProjectInfo other = (ProjectInfo) obj;
    if (this._rootDir == null) {
      if (other._rootDir != null)
        return false;
    } else if (!this._rootDir.equals(other._rootDir))
      return false;
    if (this._srcDirs == null) {
      if (other._srcDirs != null)
        return false;
    } else if (!this._srcDirs.equals(other._srcDirs))
      return false;
    if (this._incDirs == null) {
      if (other._incDirs != null)
        return false;
    } else if (!this._incDirs.equals(other._incDirs))
      return false;
    if (this._outDir == null) {
      if (other._outDir != null)
        return false;
    } else if (!this._outDir.equals(other._outDir))
      return false;
    if (this._opts == null) {
      if (other._opts != null)
        return false;
    } else if (!this._opts.equals(other._opts))
      return false;
    if (this._minOtpVsn == null) {
      if (other._minOtpVsn != null)
        return false;
    } else if (!this._minOtpVsn.equals(other._minOtpVsn))
      return false;
    if (this._libs == null) {
      if (other._libs != null)
        return false;
    } else if (!this._libs.equals(other._libs))
      return false;
    if (this._libIncs == null) {
      if (other._libIncs != null)
        return false;
    } else if (!this._libIncs.equals(other._libIncs))
      return false;
    return true;
  }
  
  @Override
  public String toString() {
    String result = new ToStringHelper().toString(this);
    return result;
  }
}
