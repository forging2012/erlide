/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.erlang.ErlangIncludeFile;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ResourceUtil;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public final class BuilderHelper {

    private static final String ERL = "erl";
    private static final String BEAM = "beam";
    private static final String ERLIDE_BUILDER = "erlide_builder";

    public BuilderHelper() {
    }

    public static boolean isDebugging() {
        return true;
        // ErlangPlugin.getDefault().isDebugging()
        // && "true".equalsIgnoreCase(Platform
        // .getDebugOption("org.erlide.core/debug/builder"));
    }

    public Collection<IPath> getAllIncludeDirs(final IProject project) {
        Collection<IPath> includeDirs = getIncludeDirs(project, new ArrayList<IPath>());

        try {
            final IProject[] referencedProjects = project.getReferencedProjects();
            for (final IProject p : referencedProjects) {
                if (p.isAccessible()) {
                    includeDirs = getIncludeDirs(p, includeDirs);
                }
            }
        } catch (final CoreException e1) {
        }
        return includeDirs;
    }

    public Collection<IPath> getIncludeDirs(final IProject project,
            final Collection<IPath> includeDirs) {
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        if (erlProject == null) {
            return includeDirs;
        }
        final Collection<IPath> projectIncludeDirs = erlProject.getProperties()
                .getIncludeDirs();
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        for (final IPath inc : projectIncludeDirs) {
            final IPath incPath = URIUtil.toPath(pvm.resolveURI(URIUtil.toURI(inc)));
            if (incPath.isAbsolute()) {
                includeDirs.add(incPath);
            } else {
                final IFolder folder = project.getFolder(incPath);
                if (folder != null) {
                    final IPath location = folder.getLocation();
                    if (location != null) {
                        includeDirs.add(location);
                    } else {
                        ErlLogger.warn("No location for %s", folder);
                    }
                }
            }
        }
        return includeDirs;
    }

    boolean isInCodePath(final IResource resource, final IErlProject erlProject) {
        final IPath projectPath = resource.getProject().getFullPath();
        final Collection<IPath> srcs = erlProject.getProperties().getSourceDirs();
        final IPath exceptLastSegment = resource.getFullPath().removeLastSegments(1);
        for (final IPath element : srcs) {
            final IPath sp = projectPath.append(element);
            if (sp.equals(exceptLastSegment)) {
                return true;
            }
        }

        return false;
    }

    public void addDependents(final IResource resource, final IProject my_project,
            final Set<BuildResource> result) throws ErlModelException {
        final IErlProject eprj = ErlangEngine.getInstance().getModel()
                .findProject(my_project);
        if (eprj != null) {
            final Collection<IErlModule> ms = eprj.getModules();
            for (final IErlModule m : ms) {
                final Collection<ErlangIncludeFile> incs = m.getIncludeFiles();
                for (final ErlangIncludeFile ifile : incs) {
                    if (ResourceUtil.samePath(ifile.getFilename(), resource.getName())) {
                        if (m.getSourceKind() == SourceKind.ERL) {
                            final BuildResource bres = new BuildResource(m.getResource());
                            result.add(bres);
                        }
                        break;
                    }
                }
            }
        }
    }

    public void checkForClashes(final IRpcSite backend, final IProject project) {
        createMarkersForDuplicateModuleNames(backend, project);
    }

    private void createMarkersForDuplicateModuleNames(final IRpcSite backend,
            final IProject project) {
        try {
            final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                    .getErlangProject(project);
            final Collection<IPath> sd = erlProject.getProperties().getSourceDirs();
            final String[] dirList = new String[sd.size()];
            int j = 0;
            for (final IPath sp : sd) {
                dirList[j++] = project.getLocation().toPortableString() + "/" + sp;
            }
            final OtpErlangList res = getSourceClashes(backend, dirList);
            for (int i = 0; i < res.arity(); i++) {
                final OtpErlangTuple t = (OtpErlangTuple) res.elementAt(i);
                final String f1 = ((OtpErlangString) t.elementAt(0)).stringValue();
                final String f2 = ((OtpErlangString) t.elementAt(1)).stringValue();
                MarkerUtils.createProblemMarker(project, null,
                        "duplicated module name in " + f1 + " and " + f2, 0,
                        IMarker.SEVERITY_WARNING);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public void ensureDirExists(final String outputDir) {
        final File f = new File(outputDir);
        f.mkdir();
    }

    public void refreshOutputDir(final IProject project) throws CoreException {
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        final IPath outputDir = erlProject.getProperties().getOutputDir();
        final IResource ebinDir = project.findMember(outputDir);
        if (ebinDir != null) {
            ebinDir.refreshLocal(IResource.DEPTH_ONE, null);
        }
    }

    public IPath getBeamForErl(final IResource source) {
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(source.getProject());
        IPath p = erlProject.getProperties().getOutputDir();
        p = p.append(source.getName());
        if (!ERL.equals(p.getFileExtension())) {
            return null;
        }
        final IPath module = p.removeFileExtension();
        final IPath beamPath = module.addFileExtension(BEAM).setDevice(null);
        return beamPath;
    }

    public static void loadModule(final @NonNull IProject project, final String module) {
        try {
            final IBackendManager backendManager = BackendCore.getBackendManager();
            for (final IBackend b : backendManager.getExecutionBackends(project)) {
                ErlLogger.debug(":: loading %s in %s", module, b.getName());
                b.getRpcSite().call("erlide_util", "load", "ao", module,
                        b.getData().shouldLoadOnAllNodes());
                backendManager.moduleLoaded(b, project, module);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static OtpErlangList getSourceClashes(final IRpcSite backend,
            final String[] dirList) throws RpcException {
        final OtpErlangObject res = backend.call(ERLIDE_BUILDER, "source_clash", "ls",
                (Object) dirList);
        if (res instanceof OtpErlangList) {
            return (OtpErlangList) res;
        }
        throw new RpcException("bad result from erlide_builder:source_clash: " + res);
    }

    public static OtpErlangList getCodeClashes(final IRpcSite b) throws RpcException {
        final OtpErlangList res = (OtpErlangList) b.call(ERLIDE_BUILDER, "code_clash",
                null);
        return res;
    }

    public class SearchVisitor implements IResourceVisitor {

        private IResource fResult;
        String fName;

        public SearchVisitor(final String name, final IProgressMonitor monitor) {
            setResult(null);
            fName = name;
        }

        @Override
        public boolean visit(final IResource resource) throws CoreException {
            if (fName == null) {
                return false;
            }
            if (getResult() != null) {
                return false;
            }
            final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                    .getErlangProject(resource.getProject());
            if (resource.getType() == IResource.FILE
                    && resource.getFileExtension() != null
                    && ERL.equals(resource.getFileExtension())
                    && isInCodePath(resource, erlProject)) {
                final String[] p = resource.getName().split("\\.");
                if (p[0].equals(fName)) {
                    setResult(resource);
                    return false;
                }
            }
            return true;
        }

        public void setResult(final IResource fResult) {
            this.fResult = fResult;
        }

        public IResource getResult() {
            return fResult;
        }
    }

}
