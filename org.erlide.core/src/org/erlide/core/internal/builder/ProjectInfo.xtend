package org.erlide.core.internal.builder

import com.ericsson.otp.erlang.OtpErlangList
import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangString
import java.util.Collection
import java.util.List
import org.eclipse.core.runtime.IPath
import org.erlide.core.builder.CompilerOptions
import org.erlide.engine.model.root.IErlProject
import org.erlide.util.erlang.OtpErlang
import com.ericsson.otp.erlang.OtpErlangAtom
import org.erlide.runtime.runtimeinfo.RuntimeVersion

@Data
class ProjectInfo {

    String rootDir
    Collection<IPath> srcDirs
    Collection<IPath> incDirs
    IPath outDir
    OtpErlangList opts
    RuntimeVersion minOtpVsn
    Collection<String> libs
    Collection<String> libIncs

    new(IErlProject project) {
        _rootDir = project.workspaceProject.location.toPortableString
        val props = project.properties
        // TODO make sure path variables are replaced!
        _srcDirs = props.sourceDirs
        _incDirs = props.includeDirs
        _outDir = props.outputDir
        _opts = CompilerOptions.get(project.workspaceProject)
        _minOtpVsn = props.requiredRuntimeVersion.asMajor
        _libs = newArrayList // TODO get libs from externalModules
        _libIncs = newArrayList // TODO get lib incs from externalIncludes
    }

    def OtpErlangObject asErlangObject() {
        val List<OtpErlangObject> result = newArrayList(
            new OtpErlangAtom("project_info"),
            new OtpErlangString(rootDir),
            OtpErlang.mkList(srcDirs.map[new OtpErlangString(it.toPortableString)]),
            OtpErlang.mkList(
                incDirs.map[new OtpErlangString(it.toPortableString)] +
                    libIncs.map[new OtpErlangString(it)]),
            new OtpErlangString(outDir.toPortableString),
            opts,
            new OtpErlangString(minOtpVsn.toString),
            OtpErlang.mkList(libs.map[new OtpErlangString(it)])
        )
        return OtpErlang.mkTuple(result)
    }

}
