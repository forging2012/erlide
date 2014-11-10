package org.erlide.engine.new_model.internal

import java.util.List
import java.util.Map
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.model.root.ErlangLibraryProperties
import org.erlide.engine.new_model.ErlModelCore
import org.erlide.engine.new_model.IErlFolder
import org.erlide.engine.new_model.IErlLibrary
import org.erlide.engine.model.root.ErlangProjectProperties

@Data
class ErlLibrary extends ErlElement implements IErlLibrary {

    transient ErlangLibraryProperties properties

    new(ErlElement parent, String name, ErlangLibraryProperties properties) {
        super(parent, name)
        if (parent === null)
            throw new IllegalArgumentException()
        this.properties = properties
    }

    override getProperties() {
        properties
    }

    override protected buildStructure(Body body, Map<IHandle, Body> newElements) throws CoreException {
        //TODO: auto-generated method stub
    }

    override protected validateExistence() throws CoreException {
        // nothing for now
    }

    override getResource() {
        null
    }

    override getFolders() {

        //TODO: auto-generated method stub
        null
    }

    override getSourceFolders() {
        val List<IErlFolder> result = newArrayList()
        for (path : properties.sourceDirs) {
            val element = ErlModelCore.create(
                ResourcesPlugin.getWorkspace().root.findMember(properties.baseDir.append(path)))
            if (element instanceof IErlFolder) {
                println('''ADD FOLDER «element»''')
                result.add(element)
            }
        }
        result
    }

    override getIncludeFolders() {

        //TODO: auto-generated method stub
        null
    }

    override getBinaryFolder() {

        //TODO: auto-generated method stub
        null
    }

    override getLibrary(String name) {
        new ErlLibrary(this, name, ErlangProjectProperties.DEFAULT)
    }

    override getLibraries() {

        //TODO: auto-generated method stub
        null
    }

    override getNonErlangResources() {

        //TODO: auto-generated method stub
        null
    }

}
