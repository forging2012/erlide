package org.erlide.ui.navigator;

import org.eclipse.handly.model.IElementChangeEvent;
import org.eclipse.handly.model.IElementChangeListener;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.CommonViewer;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlModel;

@SuppressWarnings("all")
public class ErlangNavigator extends CommonNavigator implements IElementChangeListener {
  public final static String ID = "org.erlide.ui.views.navigator.view";
  
  protected Object getInitialInput() {
    return ErlModelCore.getErlModel();
  }
  
  public void init(final IViewSite site) throws PartInitException {
    super.init(site);
    IErlModel _erlModel = ErlModelCore.getErlModel();
    _erlModel.addElementChangeListener(this);
  }
  
  public void dispose() {
    IErlModel _erlModel = ErlModelCore.getErlModel();
    _erlModel.removeElementChangeListener(this);
    super.dispose();
  }
  
  public void elementChanged(final IElementChangeEvent event) {
    CommonViewer _commonViewer = this.getCommonViewer();
    final Control control = _commonViewer.getControl();
    Display _display = control.getDisplay();
    _display.asyncExec(
      new Runnable() {
        public void run() {
          boolean _isDisposed = control.isDisposed();
          boolean _not = (!_isDisposed);
          if (_not) {
            ErlangNavigator.this.refresh();
          }
        }
      });
  }
  
  private void refresh() {
    CommonViewer _commonViewer = this.getCommonViewer();
    final Control control = _commonViewer.getControl();
    control.setRedraw(false);
    Display _display = control.getDisplay();
    BusyIndicator.showWhile(_display, 
      new Runnable() {
        public void run() {
          CommonViewer _commonViewer = ErlangNavigator.this.getCommonViewer();
          final TreePath[] treePaths = _commonViewer.getExpandedTreePaths();
          CommonViewer _commonViewer_1 = ErlangNavigator.this.getCommonViewer();
          _commonViewer_1.refresh();
          CommonViewer _commonViewer_2 = ErlangNavigator.this.getCommonViewer();
          _commonViewer_2.setExpandedTreePaths(treePaths);
        }
      });
    control.setRedraw(true);
  }
}
