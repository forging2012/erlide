package org.erlide.engine.model.root

import com.google.common.collect.Lists
import java.util.Collection
import org.eclipse.core.runtime.IPath
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.EqualsHashCode
import org.eclipse.xtend.lib.annotations.ToString
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import org.eclipse.core.runtime.Path

@Accessors
@EqualsHashCode
@ToString
class ErlangLibraryProperties {
    IPath baseDir
    Collection<IPath> sourceDirs
    Collection<IPath> includeDirs
    IPath ebinDir

    RuntimeVersion requiredRuntimeVersion

    new() {
        baseDir = new Path("")
        sourceDirs = newArrayList()
        includeDirs = newArrayList()
        ebinDir = new Path("")
        requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
    }

    new(IPath baseDir, Collection<IPath> sourceDirs, Collection<IPath> includeDirs, IPath ebinDir,
        RuntimeVersion requiredRuntimeVersion) {
        this.baseDir = baseDir
        this.sourceDirs = sourceDirs
        this.includeDirs = includeDirs
        this.ebinDir = ebinDir
        this.requiredRuntimeVersion = requiredRuntimeVersion
    }

    def void setSourceDirs(Collection<IPath> dirs) {
        sourceDirs = Lists.newArrayList(dirs)
    }

    def void setSourceDirs(IPath... dirs) {
        sourceDirs = Lists.newArrayList(dirs)
    }

    def void setIncludeDirs(Collection<IPath> dirs) {
        includeDirs = Lists.newArrayList(dirs)
    }

    def void setIncludeDirs(IPath... dirs) {
        includeDirs = Lists.newArrayList(dirs)
    }

}
