package org.erlide.core.search;

import java.util.Collection;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.erlide.model.ErlModelException;
import org.erlide.model.IParent;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.ModuleKind;
import org.erlide.model.root.ErlModelManager;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlElement.AcceptFlags;
import org.erlide.model.root.IErlElement.Kind;
import org.erlide.model.root.IErlElementVisitor;
import org.erlide.model.root.IErlExternal;
import org.erlide.model.root.IErlModel;
import org.erlide.model.root.IErlProject;
import org.erlide.model.services.search.ErlSearchScope;
import org.erlide.model.util.NatureUtil;

public class SearchCoreUtil {

    static public ErlSearchScope getProjectsScope(
            final Collection<IProject> projects, final boolean addExternals,
            final boolean addOtp) throws CoreException {
        final ErlSearchScope result = new ErlSearchScope();
        final Set<String> externalModulePaths = new HashSet<String>();
        final IErlModel model = ErlModelManager.getErlangModel();
        for (final IProject project : projects) {
            SearchCoreUtil.addProjectToScope(project, result);
            if (NatureUtil.hasErlangNature(project)) {
                final IErlProject erlProject = model.getErlangProject(project);
                addExternalModules(erlProject, result, externalModulePaths,
                        addExternals, addOtp);
            }
        }
        return result;
    }

    static void addProjectToScope(final IProject project,
            final ErlSearchScope result) throws CoreException {
        if (project == null) {
            return;
        }
        final IErlProject erlProject = ErlModelManager.getErlangModel()
                .getErlangProject(project);
        if (erlProject != null) {
            final Collection<IPath> sourcePaths = erlProject.getSourceDirs();
            for (final IPath path : sourcePaths) {
                final IFolder folder = project.getFolder(path);
                SearchCoreUtil.addFolderToScope(folder, result);
            }
        }
    }

    static void addFolderToScope(final IFolder folder,
            final ErlSearchScope result) throws CoreException {
        if (folder != null) {
            for (final IResource r : folder.members()) {
                if (r instanceof IFile) {
                    final IFile f = (IFile) r;
                    SearchCoreUtil.addFileToScope(f, result);
                }
            }
        }
    }

    static void addFileToScope(final IFile file, final ErlSearchScope result) {
        if (ModuleKind.hasModuleExtension(file.getName())) {
            final IErlModule module = ErlModelManager.getErlangModel()
                    .findModule(file);
            result.addModule(module);
        }
    }

    public static void addExternalModules(final IParent element,
            final ErlSearchScope result, final Set<String> externalModulePaths,
            final boolean addExternals, final boolean addOtp)
            throws ErlModelException {
        final Collection<IErlElement> externals = element
                .getChildrenOfKind(Kind.EXTERNAL);
        for (final IErlElement external : externals) {
            external.accept(new IErlElementVisitor() {

                @Override
                public boolean visit(final IErlElement theElement)
                        throws ErlModelException {
                    if (theElement instanceof IErlExternal) {
                        final IErlExternal theExternal = (IErlExternal) theElement;
                        if (theExternal.isOTP()) {
                            if (!addOtp) {
                                return false;
                            }
                        } else {
                            if (!addExternals) {
                                return false;
                            }
                        }
                        theExternal.open(null);
                    }
                    if (theElement instanceof IErlModule) {
                        final IErlModule module = (IErlModule) theElement;
                        if (externalModulePaths.add(module.getFilePath())) {
                            result.addModule(module);
                        }
                    }
                    return true;
                }
            }, EnumSet.noneOf(AcceptFlags.class), Kind.MODULE);
        }
    }

    public static void addResourceToScope(final ErlSearchScope result,
            final IResource r) throws CoreException {
        if (r instanceof IProject) {
            final IProject project = (IProject) r;
            addProjectToScope(project, result);
        } else if (r instanceof IFile) {
            final IFile file = (IFile) r;
            addFileToScope(file, result);
        } else if (r instanceof IFolder) {
            final IFolder folder = (IFolder) r;
            addFolderToScope(folder, result);
        }
    }

}
