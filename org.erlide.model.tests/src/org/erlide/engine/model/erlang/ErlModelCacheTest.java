package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Test;

public class ErlModelCacheTest extends WorkspaceTest {

    @Test
    public void checkThatCachesAreEmptyWhenProjectIsRemoved() throws CoreException {
        IErlProject project = null;
        try {
            // given
            // a project with a module, and some searches that fills the model
            // cache
            final String projectName = "testprojectx";
            project = createProject(
                    projectName, getTmpPath(projectName));
            final String moduleName = "f.erl";
            final IErlModule module = createModule(
                            project,
                            moduleName,
                            "-module(f).\n-include(\"a.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
                                    + "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
            module.open(null);
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();
            final IErlModule module2 = model.findModuleFromProject(project, moduleName,
                    null, IErlElementLocator.Scope.PROJECT_ONLY);
            // final ErlModelCache cache = ErlModelCache.getDefault();
            // final Set<IErlModule> modulesByName2 = cache
            // .getModulesByName(ListsUtils.withoutExtension(moduleName));
            // when
            // deleting the project
            deleteProject(project);
            // then
            // the model cache shouldn't know about the module anymore
            assertEquals(module2, module);
            // final Set<IErlModule> modulesByName = cache
            // .getModulesByName(ListsUtils.withoutExtension(moduleName));
            // assertTrue(!modulesByName2.isEmpty());
            // assertTrue(modulesByName.isEmpty());
        } finally {
            if (project != null && project.exists()) {
                deleteProject(project);
            }
        }
    }

    @Test
    public void checkThatNewModulesInNewProjectsAreCorrect() throws Exception {
        IErlProject project = null;
        IErlProject project2 = null;
        try {
            // given
            // a project with an external module and searching for it so the
            // cache
            // is updated
            final String projectName = "testprojecta";
            project = createProject(
                    projectName, getTmpPath(projectName));
            final String externalName = "xyz.erl";
            final File externalFile = createTmpFile(externalName,
                    "-module(xyz).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final File externalsFile = createTmpFile("x.erlidex",
                    absolutePath);
            ((ErlProject) project)
                    .setExternalModulesFile(externalsFile.getAbsolutePath());
            project.open(null);
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();
            final IErlModule findModule = model.findModuleFromProject(project,
                    externalName, null, IErlElementLocator.Scope.PROJECT_ONLY);
            // final ErlModelCache cache = ErlModelCache.getDefault();
            // final Set<IErlModule> modulesByName = cache
            // .getModulesByName(ListsUtils
            // .withoutExtension(externalName));
            // when
            // creating a new project with a module with the same name and
            // searching
            // for it
            final String projectName2 = "testprojectb";
            project2 = createProject(
                    projectName2, getTmpPath(projectName2));
            final IErlModule module = createModule(project2,
                    externalName, "-module(xyz).\n");
            final IErlModule findModule2 = model.findModuleFromProject(project,
                    externalName, null, IErlElementLocator.Scope.ALL_PROJECTS);
            // final Set<IErlModule> modulesByName2 = cache
            // .getModulesByName(ListsUtils
            // .withoutExtension(externalName));
            // then
            // the new module should be found
            assertNotNull(findModule);
            assertEquals(module, findModule2);
            // assertTrue(modulesByName2.contains(module));
            // final SetView<IErlModule> difference = Sets.difference(
            // modulesByName2, modulesByName);
            // assertEquals(1, difference.size());
            // assertEquals(module, difference.toArray()[0]);
        } finally {
            if (project != null && project.exists()) {
                deleteProject(project);
            }
            if (project2 != null && project2.exists()) {
                deleteProject(project2);
            }
        }
    }

}
