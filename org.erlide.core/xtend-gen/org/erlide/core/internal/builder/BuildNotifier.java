/**
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse def License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 */
package org.erlide.core.internal.builder;

import java.text.NumberFormat;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.osgi.util.NLS;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.internal.builder.BuilderMessages;
import org.erlide.util.ErlLogger;

@SuppressWarnings("all")
public class BuildNotifier {
  private final IProgressMonitor monitor;
  
  private boolean cancelling;
  
  private float percentComplete;
  
  private float progressPerCompilationUnit;
  
  private int workDone;
  
  private int totalWork;
  
  private String previousSubtask;
  
  public BuildNotifier(final IProgressMonitor monitor, final IProject project) {
    IProgressMonitor _elvis = null;
    if (monitor != null) {
      _elvis = monitor;
    } else {
      NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
      _elvis = _nullProgressMonitor;
    }
    this.monitor = _elvis;
    this.cancelling = false;
  }
  
  public void begin() {
    this.workDone = 0;
    this.totalWork = 100000;
    this.monitor.beginTask("building", this.totalWork);
    this.previousSubtask = null;
  }
  
  public void newPhase(final String string) {
  }
  
  public void newStep(final String string, final int i) {
  }
  
  /**
   * Notification before a compile that a unit is about to be compiled.
   */
  public void aboutToCompile(final IResource unit) {
    this.checkCancel();
    IPath _fullPath = unit.getFullPath();
    final String message = NLS.bind(BuilderMessages.build_compiling, _fullPath);
    this.subTask(message);
    boolean _isDebugging = BuilderHelper.isDebugging();
    if (_isDebugging) {
      ErlLogger.debug((">>" + message));
    }
  }
  
  /**
   * Check whether the build has been canceled.
   */
  public void checkCancel() {
    boolean _isCanceled = this.monitor.isCanceled();
    if (_isCanceled) {
      throw new OperationCanceledException();
    }
  }
  
  /**
   * Check whether the build has been canceled. Must use this call instead of
   * checkCancel() when within the compiler.
   */
  public void checkCancelWithinCompiler() {
    boolean _and = false;
    boolean _isCanceled = this.monitor.isCanceled();
    if (!_isCanceled) {
      _and = false;
    } else {
      _and = (!this.cancelling);
    }
    if (_and) {
      this.setCancelling(true);
    }
  }
  
  public void compiled(final IResource unit) {
    IPath _fullPath = unit.getFullPath();
    this.compiled(_fullPath);
  }
  
  public void compiled(final IPath path) {
    String _portableString = path.toPortableString();
    this.compiled(_portableString);
  }
  
  public void compiled(final String path) {
    final String message = NLS.bind(BuilderMessages.build_compiled, path);
    this.subTask(message);
    boolean _isDebugging = BuilderHelper.isDebugging();
    if (_isDebugging) {
      ErlLogger.debug(("<< " + message));
    }
    this.updateProgressDelta(this.progressPerCompilationUnit);
    this.checkCancelWithinCompiler();
  }
  
  public void done() {
    this.updateProgress(1.0f);
    this.subTask(BuilderMessages.build_done);
    this.monitor.done();
    this.previousSubtask = null;
  }
  
  public void setCancelling(final boolean cancelling) {
    this.cancelling = cancelling;
  }
  
  public void subTask(final String message) {
    boolean _equals = message.equals(this.previousSubtask);
    if (_equals) {
      return;
    }
    this.monitor.subTask(message);
    this.previousSubtask = message;
  }
  
  public void updateProgress(final float newPercentComplete) {
    if ((newPercentComplete > this.percentComplete)) {
      float _min = Math.min(newPercentComplete, 1.0f);
      this.percentComplete = _min;
      final int work = Math.round((this.percentComplete * this.totalWork));
      if ((work > this.workDone)) {
        this.monitor.worked((work - this.workDone));
        boolean _isDebugging = BuilderHelper.isDebugging();
        if (_isDebugging) {
          NumberFormat _percentInstance = NumberFormat.getPercentInstance();
          String _format = _percentInstance.format(this.percentComplete);
          ErlLogger.debug(_format);
        }
        this.workDone = work;
      }
    }
  }
  
  public void updateProgressDelta(final float percentWorked) {
    this.updateProgress((this.percentComplete + percentWorked));
  }
  
  public boolean isCanceled() {
    this.monitor.isCanceled();
    return false;
  }
  
  public void worked(final int i) {
    this.monitor.worked(i);
  }
  
  public void beginTask(final String name, final int length) {
    this.monitor.beginTask(name, length);
  }
  
  public void doneTask() {
    this.monitor.done();
  }
}
