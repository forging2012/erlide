package org.erlide.engine.new_model

import org.eclipse.handly.model.impl.HandleManager
import org.erlide.engine.new_model.IErlModel

interface IErlModelManager {
    def void startup()

    def void shutdown()

    def IErlModel getErlModel()

    def HandleManager getHandleManager()
}
