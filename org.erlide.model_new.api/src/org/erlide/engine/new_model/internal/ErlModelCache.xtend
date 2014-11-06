package org.erlide.engine.new_model.internal

import java.util.HashMap
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.ElementCache
import org.eclipse.handly.model.impl.IBodyCache
import org.erlide.engine.new_model.IErlModel
import org.erlide.engine.new_model.IErlProject
import org.erlide.engine.new_model.IErlSource

class ErlModelCache implements IBodyCache {
	val static int DEFAULT_PROJECT_SIZE = 5
	val static int DEFAULT_FILE_SIZE = 100;
	val static int DEFAULT_CHILDREN_SIZE = DEFAULT_FILE_SIZE * 20; // average 20 children per file

	var Body modelBody;
	val HashMap<IHandle, Body> projectCache;
	private ElementCache fileCache; // erl files
	private HashMap<IHandle, Body> childrenCache; // children of erl files

	public new() {
		projectCache = new HashMap<IHandle, Body>(DEFAULT_PROJECT_SIZE)
		fileCache = new ElementCache(DEFAULT_FILE_SIZE);
		childrenCache = new HashMap<IHandle, Body>(DEFAULT_CHILDREN_SIZE);
	}

	override get(IHandle handle) {
		if (handle instanceof IErlModel)
			modelBody
		else if (handle instanceof IErlProject)
			projectCache.get(handle)
		else if (handle instanceof IErlSource)
			return fileCache.get(handle)
		else
			return childrenCache.get(handle)
	}

	override peek(IHandle handle) {
		if (handle instanceof IErlModel)
			modelBody
		else if (handle instanceof IErlProject)
			projectCache.get(handle)
		else if (handle instanceof IErlSource)
			return fileCache.peek(handle)
		else
			return childrenCache.get(handle)
	}

	override put(IHandle handle, Body body) {
		if (handle instanceof IErlModel)
			modelBody = body
		else if (handle instanceof IErlProject) {
			projectCache.put(handle, body)
			fileCache.ensureSpaceLimit(body, handle)
		} else if (handle instanceof IErlSource)
			fileCache.put(handle, body)
		else
			childrenCache.put(handle, body)
	}

	override remove(IHandle handle) {
		if (handle instanceof IErlModel)
			modelBody = null
		else if (handle instanceof IErlProject) {
			projectCache.remove(handle)
			fileCache.resetSpaceLimit(DEFAULT_FILE_SIZE, handle)
		} else if (handle instanceof IErlSource)
			fileCache.remove(handle)
		else
			childrenCache.remove(handle)
	}

}
