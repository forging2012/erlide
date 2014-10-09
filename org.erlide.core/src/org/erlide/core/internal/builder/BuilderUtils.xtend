package org.erlide.core.internal.builder

import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangString
import com.google.common.io.Files
import java.util.Collection
import org.erlide.core.builder.CompilerOptions
import org.erlide.engine.model.root.IErlProject
import org.erlide.util.erlang.ErlUtils

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
        val String tmpDir = Files.createTempDir.absolutePath

        ErlUtils.format(
            "{project_info,~s,~ls,~ls,~s,~x,~s,~ls,~s}",
            rootDir,
            srcDirs.map[new OtpErlangString(it.toPortableString)],
            incDirs.map[new OtpErlangString(it.toPortableString)] +
                libIncs.map[new OtpErlangString(it)],
            outDir.toPortableString,
            opts,
            minOtpVsn.toString,
            libs.map[new OtpErlangString(it)],
            tmpDir
        )
    }
}
