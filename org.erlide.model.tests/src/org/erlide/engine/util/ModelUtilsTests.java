package org.erlide.engine.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlRecordDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ModelUtilsTests extends WorkspaceTest {

    private IErlProject project1;
    private IErlProject project2;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        setUpProject("testproject2");
        project1 = getErlProject("testproject1");
        project2 = getErlProject("testproject2");

        modelUtilService = ErlangEngine.getInstance().getModelUtilService();
        modelFindService = ErlangEngine.getInstance().getModelFindService();
    }

    private ModelUtilService modelUtilService;
    private ModelFindService modelFindService;

    @Test
    public void getImportsAsListTest() throws Exception {
        // given
        // an Erlang module with imports
        final IErlModule moduleA = createModule(project1, "ax.erl",
                "-module(ax).\n-import(lists, [reverse/1, foldl/3].\n");
        moduleA.open(null);
        // when
        // fetching imports as list of OtpErlangTuple
        final Collection<IErlElement> children = moduleA.getChildren();
        final Collection<IErlImport> imports2 = moduleA.getImports();
        final List<OtpErlangObject> imports = modelUtilService.getImportsAsList(moduleA);
        // then
        // they should be returned
        assertEquals(2, children.size());
        assertEquals(1, imports2.size());
        assertEquals(1, imports.size());
        final OtpErlangAtom listAtom = new OtpErlangAtom("lists");
        assertEquals(new OtpErlangTuple(new OtpErlangObject[] {
                listAtom,
                new OtpErlangList(new OtpErlangObject[] { makeTuple2("reverse", 1),
                        makeTuple2("foldl", 3) }) }), imports.get(0));
    }

    @Test
    public void findExternalTypeTest() throws Exception {
        // given
        // an Erlang module with typedef
        final IErlModule moduleB = createModule(project1, "bx.erl",
                "-module(bx).\n-type concat_thing() :: atom() | integer() | float() | string().\n");
        // final IErlModule moduleC =
        // createErlModule(projects[1],
        // "c.erl", "-module(c).\n-type cc() :: b:concat_thing().\n");
        final ScannerService scanner = moduleB.getScanner();
        try {
            moduleB.open(null);
            project1.open(null);
            // moduleC.open(null);
            // when
            // looking for it
            // within project
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();

            final IErlElement element1 = modelFindService.findTypeDef(model, project1,
                    moduleB, "bx", "concat_thing", moduleB.getResource().getLocation()
                            .toPortableString(), IErlElementLocator.Scope.PROJECT_ONLY);
            // in other project but path given
            final IErlElement element2 = modelFindService.findTypeDef(model, project2,
                    moduleB, "bx", "concat_thing", moduleB.getResource().getLocation()
                            .toPortableString(), IErlElementLocator.Scope.PROJECT_ONLY);
            // in other project no path given, search all projects true
            final IErlElement element3 = modelFindService.findTypeDef(model, project2,
                    moduleB, "bx", "concat_thing", null,
                    IErlElementLocator.Scope.ALL_PROJECTS);
            // in other project no path given, search all projects false, ->
            // null
            final IErlElement element4 = modelFindService.findTypeDef(model, project2,
                    moduleB, "bx", "concat_thing", null,
                    IErlElementLocator.Scope.PROJECT_ONLY);

            // then
            // it should be returned if found
            assertTrue(element1 instanceof IErlTypespec);
            assertNull(element2);
            assertTrue(element3 instanceof IErlTypespec);
            assertNull(element4);
        } finally {
            scanner.dispose();
        }
    }

    @Test
    public void findExternalFunctionModuleTest() throws Exception {
        // given
        // a module with functions and functions
        final IErlModule moduleD = createModule(project1, "d.erl",
                "-module(d).\n-export([f/0]).\nf() ->\n    ok.\ng() ->\n    ?MODULE:f().\n");
        moduleD.open(null);
        // when
        // looking for it with ?MODULE
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlElement element1 = modelFindService.findFunction(model, project1,
                moduleD, "?MODULE", null, new ErlangFunction("f", 0),
                IErlElementLocator.Scope.PROJECT_ONLY);
        // then
        // it should be found
        assertTrue(element1 instanceof IErlFunction);
    }

    @Test
    public void findPreprocessorDefTest() throws Exception {
        // given
        // a module with includes and record
        final IErlProject project = project1;
        final IErlModule include = createInclude(project, "a.hrl",
                "-record(rec1, {field, another=def}).\n-define(MACRO(A), lists:reverse(A)).\n");
        final IErlModule module = createModule(
                project,
                "f.erl",
                "-module(f).\n-include(\"a.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
                        + "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
        module.open(null);
        project.open(null);
        final IErlPreprocessorDef preprocessorDef1 = modelFindService
                .findPreprocessorDef(module, "rec1", ErlElementKind.RECORD_DEF);
        final IErlPreprocessorDef preprocessorDef2 = modelFindService
                .findPreprocessorDef(include, "rec1", ErlElementKind.RECORD_DEF);
        final IErlPreprocessorDef preprocessorDef3 = modelFindService
                .findPreprocessorDef(Lists.newArrayList(project1, project2), "f.erl",
                        "rec2", ErlElementKind.RECORD_DEF);
        // then
        // the record should be returned
        assertNotNull(module);
        assertNotNull(preprocessorDef1);
        assertTrue(preprocessorDef1 instanceof IErlRecordDef);
        assertEquals(preprocessorDef1, preprocessorDef2);
        assertEquals(preprocessorDef1.getParent(), include);
        assertNotNull(preprocessorDef3);
        assertEquals(preprocessorDef3.getParent(), module);
    }

    @Test
    public void findPreprocessorDefOtpIncludeTest() throws Exception {
        // given
        // a module with includes and record
        final IErlProject project = project1;
        final IErlModule module = createModule(
                project,
                "g.erl",
                "-module(g).\n-include_lib(\"kernel/include/file.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
                        + "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
        module.open(null);
        // when
        // looking for the record
        final IErlPreprocessorDef preprocessorDef = modelFindService.findPreprocessorDef(
                module, "file_info", ErlElementKind.RECORD_DEF);
        // then
        // the record should be returned
        assertNotNull(module);
        assertNotNull(preprocessorDef);
        assertTrue(preprocessorDef instanceof IErlRecordDef);
        assertEquals(
                ErlangEngine.getInstance().getModelUtilService()
                        .getProject(preprocessorDef), project);
    }

    private OtpErlangTuple makeTuple2(final String functionName, final int arity) {
        return new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom(functionName), new OtpErlangLong(arity) });
    }

    @Test
    public void getPreprocessorDefs() throws Exception {
        final IErlProject project = project1;
        final IErlModule module = createModule(project, "a.erl", "-module(g).\n"
                + "-include_lib(\"kernel/include/file.hrl\").\n" + "-export([f/0]).\n"
                + "-define(A(B), '++B++').\n" + "-record(rec2, {a, b}).\n" + "f() ->\n"
                + "    lists:reverse([1, 0]),\n" + "    lists:reverse([1, 0], [2]).\n");
        module.open(null);
        final List<IErlPreprocessorDef> macroDefs = modelUtilService
                .getAllPreprocessorDefs(module, ErlElementKind.MACRO_DEF);
        final List<IErlPreprocessorDef> recordDefs = modelUtilService
                .getAllPreprocessorDefs(module, ErlElementKind.RECORD_DEF);
        assertEquals(2, macroDefs.size());
        assertEquals(3, recordDefs.size());
    }

    @Test
    public void getExternalModuleWithPrefix() throws Exception {
        File externalFile = null;
        IErlProject project = null;
        try {
            // given
            // an erlang project and an external file not in any project
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
            // when
            // looking via prefix
            final List<String> moduleNames0 = modelUtilService.findUnitsWithPrefix("ex",
                    project, false, false);
            final List<String> modules1 = modelUtilService.findUnitsWithPrefix("ex",
                    project, true, false);
            final List<String> listModules = modelUtilService.findUnitsWithPrefix("list",
                    project, true, false);
            // then
            // we should find it iff we check externals
            assertEquals(0, moduleNames0.size());
            assertEquals(1, modules1.size());
            assertEquals(SystemConfiguration.withoutExtension(externalFileName),
                    modules1.get(0));
            assertEquals(1, listModules.size());
            assertEquals("lists", listModules.get(0));
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (project != null) {
                deleteProject(project);
            }
        }
    }

    @Test
    public void findExternalModuleFromPath() throws Exception {
        File externalFile = null;
        IErlProject project = null;
        try {
            // given
            // an erlang project and an external file not in any project
            final String projectName = "testproject";
            project = createTmpErlProject(projectName);
            final String externalFileName = "external1.erl";
            externalFile = createTmpFile(externalFileName,
                    "-module(external1).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final String externalsFileName = "x.erlidex";
            final File externalsFile = createTmpFile(externalsFileName, absolutePath);
            ((ErlProject) project)
                    .setExternalModulesFile(externalsFile.getAbsolutePath());
            project.open(null);
            // when
            // looking for it
            final IErlElementLocator model = getModel();
            final IErlModule module = modelFindService.findModule(model, null, null,
                    absolutePath, IErlElementLocator.Scope.ALL_PROJECTS);
            // then
            // we should find it
            assertNotNull(module);
            assertEquals(externalFileName, module.getName());
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (project != null) {
                deleteProject(project);
            }
        }
    }

    @Test
    public void findExternalIncludeFromPath() throws Exception {
        File externalFile = null;
        IErlProject project = null;
        try {
            // given
            // an erlang project and an external file not in any project
            final String projectName = "testproject";
            project = createTmpErlProject(projectName);
            final String externalFileName = "external.hrl";
            externalFile = createTmpFile(externalFileName,
                    "-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final String externalsFileName = "x.erlidex";
            final File externalsFile = createTmpFile(externalsFileName, absolutePath);
            ((ErlProject) project).setExternalIncludesFile(externalsFile
                    .getAbsolutePath());
            project.open(null);
            // when
            // looking for it
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();
            final IErlModule module = modelFindService.findInclude(model, project, null,
                    externalFileName, absolutePath);
            // then
            // we should find it
            assertNotNull(module);
            assertEquals(externalFileName, module.getName());
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (project != null) {
                deleteProject(project);
            }
        }
    }

    @Test
    public void getModulesWithReferencedProjectsWithPrefix() throws Exception {
        // given
        // two erlang projects, the first references the second, second has
        // an erlang module
        final IProject project = project1.getWorkspaceProject();
        final IProjectDescription description = project.getDescription();
        final IProject[] refs = new IProject[] { project2.getWorkspaceProject() };
        description.setReferencedProjects(refs);
        project.setDescription(description, null);
        final IErlModule module = createModule(project2, "abc.erl",
                "-module(abc).\n-export(f/0)\nf() ->\n   {abc, ok}.\n");
        createModule(project1, "bbc.erl",
                "-module(bbc).\n-export(f/0)\nf() ->\n   {abc, ok}.\n");
        // when
        // looking for module with prefix, it should be found
        final List<String> moduleNames = modelUtilService.findUnitsWithPrefix("a",
                project1, false, false);
        // then
        // we should find it
        assertNotNull(moduleNames);
        assertEquals(1, moduleNames.size());
        assertEquals(module.getModuleName(), moduleNames.get(0));
    }

    @Test
    public void getModuleFromExternalModulePath() throws Exception {
        File externalFile = null;
        IErlProject project = null;
        try {
            // given
            // an erlang project and an external file not in any project
            final String projectName = "testproject";
            project = createTmpErlProject(projectName);
            final String externalFileName = "external2.erl";
            externalFile = createTmpFile(externalFileName,
                    "-module(external2).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final String externalsFileName = "x.erlidex";
            final File externalsFile = createTmpFile(externalsFileName, absolutePath);
            ((ErlProject) project)
                    .setExternalModulesFile(externalsFile.getAbsolutePath());
            project.open(null);
            // when
            // looking for it with its external module path
            final IErlModel model = ErlangEngine.getInstance().getModel();
            final IErlModule module = modelFindService.findModule(model, null, null,
                    absolutePath, IErlElementLocator.Scope.ALL_PROJECTS);
            assertNotNull(module);
            final String externalModulePath = ErlangEngine.getInstance()
                    .getModelUtilService().getExternalModulePath(model, module);
            ErlLogger.debug(" >> %s", externalModulePath);
            final IErlModule module2 = modelUtilService.getModuleFromExternalModulePath(
                    model, externalModulePath);
            // then
            // we should find it
            assertNotNull(module2);
            assertEquals(externalFileName, module.getName());
            assertEquals(module, module2);
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (project != null) {
                deleteProject(project);
            }
        }
    }

    @Test
    public void findTypespec() throws Exception {
        // given
        // a project with a module and an include with a typespec
        final IErlProject project = project1;
        final String includeName = "a.hrl";
        final IErlModule include = createModule(project, includeName,
                "-type date() :: {pos_integer(), pos_integer(), pos_integer()}.\n");
        include.open(null);
        final IErlModule module = createModule(
                project,
                "f.erl",
                "-module(f).\n-include(\"a.hrl\").\n-export([f/0]).\n-record(rec2, {a, b}).\n"
                        + "f() ->\n    lists:reverse([1, 0]),\n    lists:reverse([1, 0], [2]).\n");
        module.open(null);
        // when
        // looking for the typespec
        final IErlTypespec typespec = modelFindService.findTypespec(module, "date");
        // then
        // it should be found
        assertNotNull(typespec);
        assertEquals(typespec.getParent(), include);
    }

    @Test
    public void findPreprocessorDefExternalIncludeOnIncludePathTest() throws Exception {
        File externalInclude = null;
        IErlProject project = null;
        // given
        // a project with an include dir outside the model, the include file
        // contains a record def
        try {
            // ErlModelCache.getDefault().setNoModelCache(true);
            // ErlModelCache.getDefault().clearModelCache();
            final String projectName = "testprojectx";
            project = createProject(getTmpPath(projectName), projectName);
            final IErlModule module = createModule(project, "a.erl",
                    "-include(\"x.hrl\").\n");
            final String includeName = "x.hrl";
            externalInclude = createTmpFile(includeName,
                    "-record(rec2, {field, another=def}.");
            final String includePath = externalInclude.getAbsolutePath();
            final IPath p = new Path(includePath).removeLastSegments(1);
            ((ErlProject) project).setIncludeDirs(Lists.newArrayList(p));
            project.open(null);
            // when
            // looking for the record def
            final IErlPreprocessorDef preprocessorDef = modelFindService
                    .findPreprocessorDef(module, "rec2", ErlElementKind.RECORD_DEF);
            final Collection<IErlProject> myprojects = Lists.newArrayList(project);
            ErlangEngine
                    .getInstance()
                    .getModelFindService()
                    .findPreprocessorDef(myprojects, "a.erl", "rec2",
                            ErlElementKind.RECORD_DEF);
            // then
            // it should be found
            assertNotNull(preprocessorDef);
        } finally {
            if (project != null) {
                deleteProject(project);
            }
            if (externalInclude != null && externalInclude.exists()) {
                externalInclude.delete();
            }
            // ErlModelCache.getDefault().setNoModelCache(false);
        }
    }

    @Test
    public void findPreprocessorDefExternalIncludePathTest() throws Exception {
        File externalInclude = null;
        IErlProject project = null;
        // given
        // a project with an include dir outside the model, the include file
        // contains a record def
        try {
            final String projectName = "testprojectx";
            project = createProject(getTmpPath(projectName), projectName);
            final IErlModule module = createModule(project, "a.erl",
                    "-include(\"x.hrl\").\n");
            final String includeName = "x.hrl";
            externalInclude = createTmpFile(includeName,
                    "-record(rec2, {field, another=def}.");
            final String includePath = externalInclude.getAbsolutePath();
            final String externalsFileName = "x.erlidex";
            final File externalsFile = createTmpFile(externalsFileName, includePath);
            ((ErlProject) project).setExternalIncludesFile(externalsFile
                    .getAbsolutePath());
            project.open(null);
            // when
            // looking for the record def
            final IErlPreprocessorDef preprocessorDef = modelFindService
                    .findPreprocessorDef(module, "rec2", ErlElementKind.RECORD_DEF);
            final Collection<IErlProject> myprojects = Lists.newArrayList(project);
            modelFindService.findPreprocessorDef(myprojects, "a.erl", "rec2",
                    ErlElementKind.RECORD_DEF);
            // then
            // it should be found
            assertNotNull(preprocessorDef);
        } finally {
            if (project != null) {
                deleteProject(project);
            }
            if (externalInclude != null && externalInclude.exists()) {
                externalInclude.delete();
            }
        }
    }

}
