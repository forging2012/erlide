package org.erlide.ui.navigator

import org.eclipse.handly.model.IElementChangeEvent
import org.eclipse.handly.model.IElementChangeListener
import org.eclipse.ui.IViewSite
import org.eclipse.ui.PartInitException
import org.eclipse.ui.navigator.CommonNavigator
import org.erlide.engine.new_model.ErlModelCore
import org.eclipse.swt.custom.BusyIndicator
import org.erlide.util.SystemConfiguration
import org.erlide.util.SystemConfiguration.Features

class ErlangNavigator extends CommonNavigator implements IElementChangeListener {
	val public static final String ID = "org.erlide.ui.views.navigator.view"

	override protected Object getInitialInput() {
		if (SystemConfiguration.hasFeatureEnabled(Features.NEW_MODEL))
			ErlModelCore.getErlModel()
		else
			super.initialInput
	}

	override void init(IViewSite site) throws PartInitException
    {
		super.init(site)
		println('''INIT---------''')
		ErlModelCore.getErlModel().addElementChangeListener(this)
	}

	override def void dispose() {
		ErlModelCore.getErlModel().removeElementChangeListener(this)
		super.dispose()
	}

	override void elementChanged(IElementChangeEvent event) {

		// NOTE: don't hold on the event or its delta.
		// The delta is only valid during the dynamic scope
		// of the notification. In particular, don't pass it
		// to another thread (e.g. via asyncExec).
		val control = getCommonViewer().getControl()
		control.getDisplay().asyncExec(
			new Runnable() {
				override void run() {
					if (! control.isDisposed()) {

						// TODO full refresh should suffice for our example,
						// but not for production code!
						refresh()
					}
				}
			})
	}

	def private void refresh() {
		val control = commonViewer.getControl()
		control.setRedraw(false)
		BusyIndicator.showWhile(control.getDisplay(),
			new Runnable() {
				override void run() {
					val treePaths = commonViewer.getExpandedTreePaths()
					commonViewer.refresh()
					commonViewer.setExpandedTreePaths(treePaths)
				}
			})
		control.setRedraw(true)
	}
}
