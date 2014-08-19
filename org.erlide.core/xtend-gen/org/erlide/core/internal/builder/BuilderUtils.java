package org.erlide.core.internal.builder;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.collect.Iterables;
import java.util.Collection;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.core.builder.CompilerOptions;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.erlang.OtpErlang;

@SuppressWarnings("all")
public class BuilderUtils {
  public static OtpErlangObject createProjectInfo(final IErlProject project) {
    final ErlangProjectProperties props = project.getProperties();
    IProject _workspaceProject = project.getWorkspaceProject();
    IPath _location = _workspaceProject.getLocation();
    final String rootDir = _location.toPortableString();
    final Collection<IPath> srcDirs = props.getSourceDirs();
    final Collection<IPath> incDirs = props.getIncludeDirs();
    final IPath outDir = props.getOutputDir();
    IProject _workspaceProject_1 = project.getWorkspaceProject();
    final OtpErlangList opts = CompilerOptions.get(_workspaceProject_1);
    RuntimeVersion _requiredRuntimeVersion = props.getRequiredRuntimeVersion();
    final RuntimeVersion minOtpVsn = _requiredRuntimeVersion.asMajor();
    final Collection<String> libs = CollectionLiterals.<String>newArrayList();
    final Collection<String> libIncs = CollectionLiterals.<String>newArrayList();
    OtpErlangAtom _otpErlangAtom = new OtpErlangAtom("project_info");
    OtpErlangString _otpErlangString = new OtpErlangString(rootDir);
    final Function1<IPath, OtpErlangString> _function = new Function1<IPath, OtpErlangString>() {
      public OtpErlangString apply(final IPath it) {
        String _portableString = it.toPortableString();
        return new OtpErlangString(_portableString);
      }
    };
    Iterable<OtpErlangString> _map = IterableExtensions.<IPath, OtpErlangString>map(srcDirs, _function);
    OtpErlangList _mkList = OtpErlang.mkList(((OtpErlangObject[])Conversions.unwrapArray(_map, OtpErlangObject.class)));
    final Function1<IPath, OtpErlangString> _function_1 = new Function1<IPath, OtpErlangString>() {
      public OtpErlangString apply(final IPath it) {
        String _portableString = it.toPortableString();
        return new OtpErlangString(_portableString);
      }
    };
    Iterable<OtpErlangString> _map_1 = IterableExtensions.<IPath, OtpErlangString>map(incDirs, _function_1);
    final Function1<String, OtpErlangString> _function_2 = new Function1<String, OtpErlangString>() {
      public OtpErlangString apply(final String it) {
        return new OtpErlangString(it);
      }
    };
    Iterable<OtpErlangString> _map_2 = IterableExtensions.<String, OtpErlangString>map(libIncs, _function_2);
    Iterable<OtpErlangString> _plus = Iterables.<OtpErlangString>concat(_map_1, _map_2);
    OtpErlangList _mkList_1 = OtpErlang.mkList(((OtpErlangObject[])Conversions.unwrapArray(_plus, OtpErlangObject.class)));
    String _portableString = outDir.toPortableString();
    OtpErlangString _otpErlangString_1 = new OtpErlangString(_portableString);
    String _string = minOtpVsn.toString();
    OtpErlangString _otpErlangString_2 = new OtpErlangString(_string);
    final Function1<String, OtpErlangString> _function_3 = new Function1<String, OtpErlangString>() {
      public OtpErlangString apply(final String it) {
        return new OtpErlangString(it);
      }
    };
    Iterable<OtpErlangString> _map_3 = IterableExtensions.<String, OtpErlangString>map(libs, _function_3);
    OtpErlangList _mkList_2 = OtpErlang.mkList(((OtpErlangObject[])Conversions.unwrapArray(_map_3, OtpErlangObject.class)));
    final Collection<OtpErlangObject> result = CollectionLiterals.<OtpErlangObject>newArrayList(_otpErlangAtom, _otpErlangString, _mkList, _mkList_1, _otpErlangString_1, opts, _otpErlangString_2, _mkList_2);
    return OtpErlang.mkTuple(((OtpErlangObject[])Conversions.unwrapArray(result, OtpErlangObject.class)));
  }
}
