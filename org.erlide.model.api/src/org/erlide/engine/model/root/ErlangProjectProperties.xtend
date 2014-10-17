package org.erlide.engine.model.root

import com.google.common.base.Strings
import com.google.common.collect.Lists
import java.util.Collection
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
    Collection<IPath> testDirs

    Collection<ErlangLibraryProperties> libraries

    @Accessors(NONE)
    val transient ExternalLibrariesHelper externalLibrariesHelper

    val public static ErlangProjectProperties DEFAULT = new ErlangProjectProperties() => [
        sourceDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS)
        outputDir = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR)
        includeDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS)
        testDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_TEST_DIRS)
        requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
        val externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES
        val externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES
        externalLibrariesHelper.externalModulesFile = externalModulesFile
        externalLibrariesHelper.externalIncludesFile = externalIncludesFile
        libraries = externalLibrariesHelper.build()
    ]

    new() {
        super()
        outputDir = new Path("")
        testDirs = newArrayList()
        libraries = newArrayList()
        externalLibrariesHelper = new ExternalLibrariesHelper("", "")
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

    def void setTestDirs(Collection<IPath> dirs) {
        testDirs = Lists.newArrayList(dirs)
    }

    def void setTestDirs(IPath... dirs) {
        testDirs = Lists.newArrayList(dirs)
    }

    def String getExternalIncludes() {
        externalLibrariesHelper.externalIncludes
    }

    def String getExternalModules() {
        externalLibrariesHelper.externalModules
    }

    def setExternalModulesFile(String mods) {
        externalLibrariesHelper.externalModulesFile = mods
    }

    def setExternalIncludesFile(String incs) {
        externalLibrariesHelper.externalIncludesFile = incs
    }

    def getExternalModulesFile() {
        externalLibrariesHelper.externalModulesFile
    }

    def getExternalIncludesFile() {
        externalLibrariesHelper.externalIncludesFile
    }
}

@Accessors
class ExternalLibrariesHelper {

    String externalModulesFile
    String externalIncludesFile

    new(String mods, String incs) {
        externalModulesFile = mods
        externalIncludesFile = incs
    }

    def String getExternalIncludes() {
        getExternal(ExternalKind.EXTERNAL_INCLUDES)
    }

    def String getExternalModules() {
        getExternal(ExternalKind.EXTERNAL_MODULES)
    }

    private def String getExternal(ExternalKind external) {
        val IPreferencesService service = Platform.preferencesService
        val String key = if (external == ExternalKind.EXTERNAL_INCLUDES) "default_external_includes" else "default_external_modules"
        var String result = getExternal(external, service, key, "org.erlide.ui")
        if (Strings.isNullOrEmpty(result)) {

            // FIXME this is kind of an indirect dep on core plugin (needs to be started)
            result = getExternal(external, service, key, "org.erlide.core")
        }
        return result
    }

    private def String getExternal(ExternalKind external, IPreferencesService service, String key, String pluginId) {
        val String global = service.getString(pluginId, key, "", null)
        val String projprefs = if (external == ExternalKind.EXTERNAL_INCLUDES)
                externalIncludesFile
            else
                externalModulesFile
        return PreferencesUtils.packArray(#[projprefs, global])
    }

    def Collection<ErlangLibraryProperties> build() {

        // TODO fill from files + global
        val mods = externalModules
        val incs = externalIncludes

        //        val allMods = expand(mods)
        //        val allIncs = expand(incs)
        // expand values and look for common prefixes to merge mod+inc in libraries
        newArrayList()
    }

    private def expand(String string) {
        throw new UnsupportedOperationException("TODO: auto-generated method stub")
    }
}
