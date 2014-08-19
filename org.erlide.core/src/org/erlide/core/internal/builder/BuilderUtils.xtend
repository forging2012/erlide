package org.erlide.core.internal.builder

import com.ericsson.otp.erlang.OtpErlangAtom
import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangString
import java.util.Collection
import org.erlide.core.builder.CompilerOptions
import org.erlide.engine.model.root.IErlProject
import org.erlide.util.erlang.OtpErlang

class BuilderUtils {
    def static OtpErlangObject createProjectInfo(IErlProject project) {

        // TODO make sure path variables are replaced!
        val props = project.properties
        val rootDir = project.workspaceProject.location.toPortableString
        val srcDirs = props.sourceDirs
        val incDirs = props.includeDirs
        val outDir = props.outputDir
        val opts = CompilerOptions.get(project.workspaceProject)
        val minOtpVsn = props.requiredRuntimeVersion.asMajor
        val Collection<String> libs = newArrayList // TODO get libs from externalModules
        val Collection<String> libIncs = newArrayList // TODO get lib incs from externalIncludes

        val Collection<OtpErlangObject> result = newArrayList(
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
