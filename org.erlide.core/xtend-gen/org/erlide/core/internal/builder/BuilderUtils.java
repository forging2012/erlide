package org.erlide.core.internal.builder;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.collect.Iterables;
import com.google.common.io.Files;
import java.io.File;
import java.util.Collection;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.core.builder.CompilerOptions;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.erlang.ErlUtils;

@SuppressWarnings("all")
public class BuilderUtils {
  public static OtpErlangObject createProjectInfo(final IErlProject project) {
    try {
      OtpErlangObject _xblockexpression = null;
      {
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
        File _createTempDir = Files.createTempDir();
        final String tmpDir = _createTempDir.getAbsolutePath();
        final Function1<IPath, OtpErlangString> _function = new Function1<IPath, OtpErlangString>() {
          public OtpErlangString apply(final IPath it) {
            String _portableString = it.toPortableString();
            return new OtpErlangString(_portableString);
          }
        };
        Iterable<OtpErlangString> _map = IterableExtensions.<IPath, OtpErlangString>map(srcDirs, _function);
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
        String _portableString = outDir.toPortableString();
        String _string = minOtpVsn.toString();
        final Function1<String, OtpErlangString> _function_3 = new Function1<String, OtpErlangString>() {
          public OtpErlangString apply(final String it) {
            return new OtpErlangString(it);
          }
        };
        Iterable<OtpErlangString> _map_3 = IterableExtensions.<String, OtpErlangString>map(libs, _function_3);
        _xblockexpression = ErlUtils.format(
          "{project_info,~s,~ls,~ls,~s,~x,~s,~ls,~s}", rootDir, _map, _plus, _portableString, opts, _string, _map_3, tmpDir);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
