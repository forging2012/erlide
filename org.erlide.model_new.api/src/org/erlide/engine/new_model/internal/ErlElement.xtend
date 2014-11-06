package org.erlide.engine.new_model.internal

import org.eclipse.handly.model.impl.Handle

abstract class ErlElement extends Handle {

	new(Handle parent, String name) {
		super(parent, name)
	}

	override protected getHandleManager() {
		ErlModelManager.INSTANCE.getHandleManager()
	}

}
