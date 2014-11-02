package org.erlide.engine.new_model.internal

import java.util.HashMap
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.IBodyCache

class ErlModelCache implements IBodyCache {
    val static int DEFAULT_PROJECT_SIZE = 5

    var Body modelBody;
    val HashMap<IHandle, Body> projectCache;

    public new() {
        projectCache = new HashMap<IHandle, Body>(DEFAULT_PROJECT_SIZE)
    }

    override get(IHandle handle) {
        if (handle instanceof org.erlide.engine.new_model.IErlModel)
            modelBody
        else if (handle instanceof org.erlide.engine.new_model.IErlProject)
            projectCache.get(handle)
        else
            null
    }

    override peek(IHandle handle) {
        if (handle instanceof org.erlide.engine.new_model.IErlModel)
            modelBody
        else if (handle instanceof org.erlide.engine.new_model.IErlProject)
            projectCache.get(handle)
        else
            null
    }

    override put(IHandle handle, Body body) {
        if (handle instanceof org.erlide.engine.new_model.IErlModel)
            modelBody = body
        else if (handle instanceof org.erlide.engine.new_model.IErlProject)
            projectCache.put(handle, body)
    }

    override remove(IHandle handle) {
        if (handle instanceof org.erlide.engine.new_model.IErlModel)
            modelBody = null
        else if (handle instanceof org.erlide.engine.new_model.IErlProject)
            projectCache.remove(handle)
    }

}
