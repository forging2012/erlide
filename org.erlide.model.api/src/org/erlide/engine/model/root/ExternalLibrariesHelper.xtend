package org.erlide.engine.model.root

import com.google.common.base.Charsets
import com.google.common.base.Strings
import com.google.common.io.Files
import java.io.File
import java.util.Collection
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.preferences.IPreferencesService
import org.eclipse.xtend.lib.annotations.Accessors
import org.erlide.util.PreferencesUtils
import java.util.List
import java.util.Map

@Accessors
class ExternalLibrariesHelper {

    String externalModulesFile
    String externalIncludesFile

    new() {
        externalModulesFile = ""
        externalIncludesFile = ""
    }

    def String getExternalIncludes() {
        getExternal(ExternalKind.EXTERNAL_INCLUDES)
    }

    def String getExternalModules() {
        getExternal(ExternalKind.EXTERNAL_MODULES)
    }

    private def String getExternal(ExternalKind external) {
        val IPreferencesService service = Platform.preferencesService
        val String key = if (external == ExternalKind.EXTERNAL_INCLUDES)
                "default_external_includes"
            else
                "default_external_modules"
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

    def Collection<ErlangLibraryProperties> build(IPath baseDir) {

        // TODO fill from global settings too
        val mods = externalModules
        val incs = externalIncludes
        val allMods = justFolders(expand_it(mods, baseDir))
        val allIncs = justFolders(expand_it(incs, baseDir))

        newArrayList(merge(allMods, allIncs))
    }

    def Collection<String> expand_it(String fileName, IPath baseDir) {
        if (Strings.isNullOrEmpty(fileName))
            return newArrayList

        expand_it(fileName, baseDir) [
            val p = new Path(fileName)
            val name = if (p.isAbsolute) fileName else baseDir.append(fileName).toPortableString
            val f = new File(name)
            Files.readLines(f, Charsets.UTF_8)
        ]
    }

    def Collection<String> expand_it(String fileName, IPath baseDir, (String)=>Collection<String> xexpander) {
        val content = xexpander.apply(fileName)
        newLinkedHashSet(content.map[expand(baseDir, xexpander)].flatten)
    }

    def Collection<String> expand(String fileName, IPath baseDir, (String)=>Collection<String> xexpander) {
        val result = newArrayList
        if (Strings.isNullOrEmpty(fileName))
            return result

        if (fileName.endsWith(".erlidex")) {
            val expanded = expand_it(fileName, baseDir, xexpander)
            result.addAll(expanded)
        } else {
            result.add(fileName)
        }
        result
    }

    def Collection<IPath> justFolders(Iterable<String> files) {
        newLinkedHashSet(files.map[new Path(it).removeLastSegments(1)])
    }

    def group(Iterable<IPath> paths) {
        paths.groupBy [
            removeLastSegments(1)
        ]
    }

    def Collection<ErlangLibraryProperties> merge(Iterable<IPath> mods, Iterable<IPath> incs) {

        // TODO implement
        // - look for common folders in mods and incs and create a single library
        // - library points to folders, not files
        val Map<IPath, List<IPath>> grouped = newHashMap()
        for (IPath mod : mods) {
            val mroot = mod.removeLastSegments(1)
            val List<IPath> matching = newArrayList
            for (IPath inc : incs) {
                val iroot = inc.removeLastSegments(1)
                if (mroot == iroot) {
                    matching.add(inc)
                }
            }
            grouped.put(mod, matching)
        }

        // TODO what about non-matched includes?
        newArrayList
    }

}
