package org.erlide.engine.model.root

import com.google.common.collect.Lists
import java.util.Collection
import org.eclipse.core.runtime.IPath
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtend.lib.annotations.EqualsHashCode
import org.eclipse.xtend.lib.annotations.ToString
import org.erlide.runtime.runtimeinfo.RuntimeVersion

@Accessors
@EqualsHashCode
@ToString
class ErlangLibraryProperties {
    Collection<IPath> sourceDirs
    Collection<IPath> includeDirs

    RuntimeVersion requiredRuntimeVersion

    new() {
        sourceDirs = newArrayList()
        includeDirs = newArrayList()
        requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
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
