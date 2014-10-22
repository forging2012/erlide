package org.erlide.engine.model.root

import com.google.common.base.Strings
import java.util.Collection
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.preferences.IPreferencesService
import org.eclipse.xtend.lib.annotations.Accessors
import org.erlide.util.PreferencesUtils

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
        var String result = getExternal(external, service, key, "org.erlide.model")
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
        //        val mods = externalModules
        //        val incs = externalIncludes
        //        val allMods = expand(mods)
        //        val allIncs = expand(incs)
        // expand values and look for common prefixes to merge mod+inc in libraries
        newArrayList()
    }

//    private def expand(String string) {
//        throw new UnsupportedOperationException("TODO: auto-generated method stub")
//    }
}
