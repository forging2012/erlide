package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.util.ResourceUtil;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ErlProjectTest extends WorkspaceTest {

    private static IErlProject[] projects;

    @Before
    public void setup() throws Exception {
        // We set up projects here, it's quite costly
        final String name1 = "testproject1";
        final IErlProject erlProject1 = createProject(name1, getTmpPath(name1));
        final String name2 = "testproject2";
        final IErlProject erlProject2 = createProject(name2, getTmpPath(name2));
        projects = new IErlProject[] { erlProject1, erlProject2 };
    }

    @Test
    public void findIncludeFile() throws Exception {
        // given
        // a project with a module and an include including file.hrl
        final IErlProject project = projects[0];
        final String includeName = "a.hrl";
        final IErlModule include = createModule(
                project,
                includeName,
                "-include_lib(\"kernel/include/file.hrl\").\n-record(rec1, {field, another=def}).\n-define(MACRO(A), lists:reverse(A)).\n");
        include.open(null);
        final IErlModule module = createModule(
                project,
                "f.erl",
                "-module(f).\n-include(\"a.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
                        + "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
        module.open(null);
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        // when
        // looking for the include
        final IErlModule include1 = model.findIncludeFromModule(module, includeName,
                null, IErlElementLocator.Scope.PROJECT_ONLY);
        final IErlModule include2 = model.findIncludeFromProject(project, "file.hrl",
                null, IErlElementLocator.Scope.PROJECT_ONLY);
        // then
        // it should be found
        assertEquals(include, include1);
        assertNotNull(include2);
    }

    @Test
    public void findExternalIncludeFileOnIncludePath() throws Exception {
        File externalInclude = null;
        IErlProject project = null;
        // given
        // a project with an include dir outside the model
        try {
            final String projectName = "testprojectx";
            project = createProject(projectName, getTmpPath(projectName));
            final String includeName = "x01.hrl";
            externalInclude = createTmpFile(includeName,
                    "-record(rec2, {field, another=def}.");
            final String includePath = externalInclude.getAbsolutePath();
            final IPath p = new Path(includePath).removeLastSegments(1);
            ((ErlProject) project).setIncludeDirs(Lists.newArrayList(p));
            // when
            // looking for the include file
            // String includeFile =
            // ErlangEngine.getInstance().getModelUtilService().findIncludeFile(erlProject,
            // "x.hrl", "");
            project.open(null);
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();
            final IErlModule module = model.findIncludeFromProject(project, null,
                    includePath, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            final IErlModule module2 = model.findModuleFromProject(project, includeName,
                    null, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            // then
            // it should be found in the model
            assertNotNull(module);
            assertEquals(module, module2);
        } finally {
            if (project != null) {
                deleteProject(project);
            }
            if (externalInclude != null && externalInclude.exists()) {
                externalInclude.delete();
            }
        }
    }

    @Test
    public void findIncludeFileOnIncludePathInOtherProject() throws Exception {
        // http://www.assembla.com/spaces/erlide/tickets/756-navigation--external-include-files-are-not-found
        IErlModule externalInclude = null;
        IErlProject project = null;
        IErlProject project2 = null;
        // given
        // a project with an include dir outside the model
        try {
            final String projectName = "testprojectx";
            project = createProject(projectName, getTmpPath(projectName));
            final String projectName2 = "testprojecty";
            project2 = createProject(projectName2, getTmpPath(projectName2));

            final String includeName = "x.hrl";
            externalInclude = createInclude(project2, "x.hrl",
                    "-record(rec2, {field, another=def}.");
            final String includePath = externalInclude.getFilePath();
            final IPath p = new Path(includePath).removeLastSegments(1);
            ((ErlProject) project).setIncludeDirs(Lists.newArrayList(p));
            // when
            // looking for the include file
            project.open(null);
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();
            final IErlModule module = model.findIncludeFromProject(project, includeName,
                    null, IErlElementLocator.Scope.ALL_PROJECTS);
            // then
            // it should be found in the project defining it
            assertNotNull(module);
            assertEquals(project2, ErlangEngine.getInstance().getModelUtilService()
                    .getProject(module));
        } finally {
            if (project != null) {
                deleteProject(project);
            }
            if (project2 != null) {
                deleteProject(project2);
            }
        }
    }

    @Test
    public void findFunctionInExternalFilesTest() throws Exception {
        // given
        // a module with calls to the lists module
        final IErlProject project = projects[0];
        final IErlModule moduleE = createModule(
                project,
                "e.erl",
                "-module(e).\n-export([f/0]).\nf() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
        final ScannerService scanner = moduleE.getScanner();
        try {
            moduleE.open(null);
            // when
            // looking for lists:reverse/2 and lists:reverse/1
            final IErlModel model = ErlangEngine.getInstance().getModel();
            final OpenResult res = ErlangEngine
                    .getInstance()
                    .getService(OpenService.class)
                    .open(moduleE.getScannerName(),
                            49,
                            ErlangEngine.getInstance().getModelUtilService()
                                    .getImportsAsList(moduleE),
                            project.getProperties().getExternalModules(),
                            model.getPathVars(project.getWorkspaceProject()));
            final IErlFunction function = ErlangEngine
                    .getInstance()
                    .getModelFindService()
                    .findFunction(model, project, moduleE, res.getName(), res.getPath(),
                            res.getFunction(), IErlElementLocator.Scope.PROJECT_ONLY);
            assertNotNull(function);

            final IErlElement module = model.findModuleFromProject(project,
                    function.getModuleName(), res.getPath(),
                    IErlElementLocator.Scope.PROJECT_ONLY);
            // then
            // the function should be returned and the module, in External Files
            assertNotNull(module);
            assertEquals(function.getParent(), module);
            assertEquals(
                    ErlangEngine.getInstance().getModelUtilService().getProject(function),
                    project);
        } finally {
            scanner.dispose();
        }
    }

    @Test
    public void findExternalModule() throws Exception {
        File externalFile = null;
        IErlProject project = null;
        try {
            // given
            // an erlang project with an external file
            final String projectName = "testproject";
            project = createTmpErlProject(projectName);
            final String externalFileName = "external.erl";
            externalFile = createTmpFile(externalFileName,
                    "-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final String externalsFileName = "x.erlidex";
            final File externalsFile = createTmpFile(externalsFileName, absolutePath);
            ((ErlProject) project)
                    .setExternalModulesFile(externalsFile.getAbsolutePath());
            project.open(null);
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();
            // when
            // looking for it
            final IErlModule externalModule = model.findModuleFromProject(project,
                    externalFileName, null, IErlElementLocator.Scope.PROJECT_ONLY);
            // then
            // we should find it
            assertNotNull(externalModule);
            assertTrue(ResourceUtil.samePath(absolutePath, externalModule.getFilePath()));
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (project != null) {
                deleteProject(project);
            }
        }

    }
}
