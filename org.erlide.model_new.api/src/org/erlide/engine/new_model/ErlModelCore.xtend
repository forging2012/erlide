package org.erlide.engine.new_model

import org.eclipse.core.resources.IProject
import org.erlide.engine.new_model.internal.ErlModelManager

class ErlModelCore {

    def static IErlModel getErlModel() {
        ErlModelManager.INSTANCE.erlModel
    }

    def static IErlProject create(IProject project) {
        if (project === null)
            null
        else
            erlModel.getErlProject(project.name)
    }

    private new() {
    }
}
