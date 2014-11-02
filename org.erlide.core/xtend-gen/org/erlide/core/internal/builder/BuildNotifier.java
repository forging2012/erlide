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

import com.google.common.base.Objects;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.osgi.util.NLS;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.internal.builder.BuildPhase;
import org.erlide.core.internal.builder.BuilderCanceledException;
import org.erlide.core.internal.builder.BuilderMessages;
import org.erlide.util.ErlLogger;

/**
 * Wrapper for a progress monitor.
 * 
 * We this is matching the return from the rebar builder, assuming the following:
 * - 5 phases: clean, compile, eunit_compile*, eunit*, xref* (the last three are optional)
 * - phases work: clean 5%, compile 65%, eunit_compile 5%, eunit 20%, xref 5%
 * - compile phase has 10 steps
 * - '.erl' step takes 91% of the progress, the rest 1% each
 */
@SuppressWarnings("all")
public class BuildNotifier {
  private final static int ERL_STEP_WORK = 91;
  
  private final static int OTHER_STEP_WORK = 1;
  
  private boolean cancelling;
  
  private final SubMonitor monitor;
  
  private SubMonitor phaseMonitor;
  
  private SubMonitor stepMonitor;
  
  /**
   * @param monitor the progress monitor to use for reporting progress to the user.
   *  It is the caller's responsibility to call done() on the given monitor.
   *  Accepts null, indicating that no progress should be reported and that the
   *  operation cannot be cancelled.
   */
  public BuildNotifier(final IProgressMonitor monitor, final IProject project) {
    SubMonitor _convert = SubMonitor.convert(monitor, 100);
    this.monitor = _convert;
    this.cancelling = false;
  }
  
  public void begin() {
  }
  
  public void newPhase(final String name) {
    String _upperCase = name.toUpperCase();
    final BuildPhase phase = BuildPhase.valueOf(_upperCase);
    int _work = phase.getWork();
    SubMonitor _newChild = this.monitor.newChild(_work);
    this.phaseMonitor = _newChild;
    this.phaseMonitor.setWorkRemaining(100);
  }
  
  public void newStep(final String name, final int items) {
    int _xifexpression = (int) 0;
    boolean _equals = Objects.equal(name, ".erl");
    if (_equals) {
      _xifexpression = BuildNotifier.ERL_STEP_WORK;
    } else {
      _xifexpression = BuildNotifier.OTHER_STEP_WORK;
    }
    final int work = _xifexpression;
    SubMonitor _newChild = this.phaseMonitor.newChild(work);
    this.stepMonitor = _newChild;
    this.stepMonitor.setWorkRemaining(items);
  }
  
  public void compiled(final String path) {
    final String message = NLS.bind(BuilderMessages.build_compiled, path);
    this.stepMonitor.subTask(message);
    boolean _isDebugging = BuilderHelper.isDebugging();
    if (_isDebugging) {
      ErlLogger.debug(("<< " + message));
    }
    this.stepMonitor.worked(1);
    this.checkCancelWithinCompiler();
  }
  
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
    try {
      boolean _and = false;
      boolean _isCanceled = this.monitor.isCanceled();
      if (!_isCanceled) {
        _and = false;
      } else {
        _and = (!this.cancelling);
      }
      if (_and) {
        this.setCancelling(true);
        throw new BuilderCanceledException();
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public void done() {
    this.monitor.done();
  }
  
  public void setCancelling(final boolean cancelling) {
    this.cancelling = cancelling;
  }
  
  public boolean isCanceled() {
    return this.monitor.isCanceled();
  }
  
  public void worked(final int work) {
    this.monitor.worked(work);
  }
  
  public IProgressMonitor getMonitor() {
    return this.monitor;
  }
}
