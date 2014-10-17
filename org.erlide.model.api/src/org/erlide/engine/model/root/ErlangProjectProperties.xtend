package org.erlide.engine.model.root

import com.google.common.base.Strings
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.preferences.IPreferencesService
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.EqualsHashCode
import org.eclipse.xtend.lib.annotations.ToString
import org.erlide.util.PreferencesUtils

@Accessors
@EqualsHashCode
@ToString
class ErlangProjectProperties extends ErlangLibraryProperties {

    IPath outputDir

    String externalIncludesFile
    String externalModulesFile

    val public static ErlangProjectProperties DEFAULT = new ErlangProjectProperties() => [
        sourceDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS)
        outputDir = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR)
        includeDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS)
        testDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_TEST_DIRS)
        externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES
        externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES
        requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
    ]

    new() {
        super()
        outputDir = new Path("")
        externalIncludesFile = ""
        externalModulesFile = ""
    }

    def void copyFrom(ErlangProjectProperties props) {
        includeDirs = props.includeDirs
        testDirs = props.testDirs
        sourceDirs = props.sourceDirs
        outputDir = props.outputDir
        requiredRuntimeVersion = props.requiredRuntimeVersion
        externalIncludesFile = props.externalIncludesFile
        externalModulesFile = props.externalModulesFile
    }

    def String getExternalIncludes() {
        val externalIncludesString = getExternal(ExternalKind.EXTERNAL_INCLUDES);
        return externalIncludesString;
    }

    def String getExternalModules() {
        val externalModulesString = getExternal(ExternalKind.EXTERNAL_MODULES);
        return externalModulesString;

    }

    def private String getExternal(ExternalKind external) {
        val IPreferencesService service = Platform.getPreferencesService()
        val String key = if (external == ExternalKind.EXTERNAL_INCLUDES) "default_external_includes" else "default_external_modules"
        var String result = getExternal(external, service, key, "org.erlide.ui")
        if (Strings.isNullOrEmpty(result)) {

            // FIXME this is kind of an indirect dep on core plugin (needs to be started)
            result = getExternal(external, service, key, "org.erlide.core")
        }
        return result
    }

    def private String getExternal(ExternalKind external, IPreferencesService service, String key, String pluginId) {
        val String global = service.getString(pluginId, key, "", null)
        val String projprefs = if (external == ExternalKind.EXTERNAL_INCLUDES)
                externalIncludesFile
            else
                externalModulesFile
        return PreferencesUtils.packArray(#[projprefs, global])
    }

}

