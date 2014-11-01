package org.erlide.engine.model.root

import com.google.common.collect.Lists
import java.util.Collection
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.EqualsHashCode
import org.eclipse.xtend.lib.annotations.ToString

@Accessors
@EqualsHashCode
@ToString
class ErlangProjectProperties extends ErlangLibraryProperties {

    Collection<IPath> testDirs

    @Accessors(NONE)
    val transient ExternalLibrariesHelper externalLibrariesHelper

    val public static ErlangProjectProperties DEFAULT = new ErlangProjectProperties() => [
        sourceDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS)
        ebinDir = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR)
        includeDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS)
        testDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_TEST_DIRS)
        requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
        val externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES
        val externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES
        externalLibrariesHelper.externalModulesFile = externalModulesFile
        externalLibrariesHelper.externalIncludesFile = externalIncludesFile
    ]

    new() {
        super()
        testDirs = newArrayList()
        externalLibrariesHelper = new ExternalLibrariesHelper()
    }

    def void copyFrom(ErlangProjectProperties props) {
        includeDirs = props.includeDirs
        testDirs = props.testDirs
        sourceDirs = props.sourceDirs
        ebinDir = props.ebinDir
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

    override setBaseDir(IPath baseDir) {
        super.setBaseDir(baseDir)
    }

    def Collection<ErlangLibraryProperties> getLibraries() {
        externalLibrariesHelper.build(baseDir)
    }

    def IPath getOutputDir() {
        ebinDir
    }

    def void setOutputDir(IPath dir) {
        ebinDir = dir
    }

}
