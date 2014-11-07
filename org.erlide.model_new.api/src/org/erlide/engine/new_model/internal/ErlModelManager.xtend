package org.erlide.engine.new_model.internal

import org.eclipse.core.resources.IResourceChangeEvent
import org.eclipse.core.resources.IResourceChangeListener
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.ISafeRunnable
import org.eclipse.core.runtime.ListenerList
import org.eclipse.core.runtime.SafeRunner
import org.eclipse.handly.model.IElementChangeEvent
import org.eclipse.handly.model.IElementChangeListener
import org.eclipse.handly.model.impl.ElementChangeEvent
import org.eclipse.handly.model.impl.HandleManager
import org.erlide.engine.new_model.IErlModel
import org.erlide.engine.new_model.IErlModelManager
import org.erlide.util.ErlLogger

/**
 * The manager for the Erlang model.
 *
 * @threadsafe This class is intended to be thread-safe
 */
class ErlModelManager implements IErlModelManager, IResourceChangeListener {
    public val static ErlModelManager INSTANCE = new ErlModelManager()

    var IErlModel erlModel
    var HandleManager handleManager
    var ListenerList listenerList

    override void startup() {
        erlModel = new ErlModel();
        handleManager = new HandleManager(new ErlModelCache());
        listenerList = new ListenerList()
        erlModel.getWorkspace().addResourceChangeListener(this, IResourceChangeEvent.POST_CHANGE)
    }

    override void shutdown() {
        erlModel.getWorkspace().removeResourceChangeListener(this)
        listenerList = null
        handleManager = null
        erlModel = null
    }

    override IErlModel getErlModel() {
        if (erlModel === null)
            throw new IllegalStateException()
        return erlModel
    }

    override HandleManager getHandleManager() {
        if (handleManager === null)
            throw new IllegalStateException()
        return handleManager
    }

    private new() {
    }

    override resourceChanged(IResourceChangeEvent event) {
        if (event.getType() != IResourceChangeEvent.POST_CHANGE)
            return
        val ErlDeltaProcessor deltaProcessor = new ErlDeltaProcessor()
        try {
            event.getDelta().accept(deltaProcessor)
        } catch (CoreException e) {
            ErlLogger.error(e)
        }
        if (!deltaProcessor.getDelta().isEmpty()) {
            fireElementChangeEvent(new ElementChangeEvent(ElementChangeEvent.POST_CHANGE, deltaProcessor.getDelta()))
        }
    }

    def void addElementChangeListener(IElementChangeListener listener) {
        if (listenerList === null)
            throw new IllegalStateException();
        listenerList.add(listener);
    }

    def void removeElementChangeListener(IElementChangeListener listener) {
        if (listenerList === null)
            throw new IllegalStateException();
        listenerList.remove(listener);
    }

    def void fireElementChangeEvent(IElementChangeEvent event) {
        if (listenerList === null)
            throw new IllegalStateException();
        val listeners = listenerList.getListeners();
        for (listener : listeners) {

            SafeRunner.run(
                new ISafeRunnable() {
                    override void handleException(Throwable exception) {
                        // already logged by Platform
                    }

                    override void run() {
                        (listener as IElementChangeListener).elementChanged(event)
                    }
                });
        }
    }
}
