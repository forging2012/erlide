package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ErlFolderTests extends WorkspaceTest {

    protected IErlModule module;
    protected IErlProject project;
    private IErlFolder srcFolder;
    private IErlFolder includeFolder;
    private IErlFolder ebinFolder;
    private static IErlProject project2;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        setUpProject("testproject2");

        project = getErlProject("testproject1");
        project2 = getErlProject("testproject2");
        module = getErlModule("xx.erl");

        final IProject workspaceProject = project2.getWorkspaceProject();
        final String src = "src";
        final IFolder folder = workspaceProject.getFolder(src);
        final String below = "below";
        final IFolder folder2 = folder.getFolder(below);
        createFolder(folder2);
        final IPath belowPath = new Path(src).append(below);
        final Collection<IPath> sourceDirs = Lists.newArrayList(belowPath);
        ((ErlProject) project2).setSourceDirs(sourceDirs);

        srcFolder = (IErlFolder) project.getChildNamed("src");
        includeFolder = (IErlFolder) project.getChildNamed("include");
        ebinFolder = (IErlFolder) project.getChildNamed("ebin");
    }

    // Collection<IErlModule> getModules() throws ErlModelException;
    @Test
    public void getModules() throws Exception {
        final Collection<IErlModule> modules = srcFolder.getModules();
        assertEquals(2, modules.size());
        final Collection<IErlModule> modules2 = includeFolder.getModules();
        assertEquals(0, modules2.size());
    }

    // boolean isOnSourcePath();
    @Test
    public void isOnSourcePath() throws Exception {
        assertTrue(srcFolder.isOnSourcePath());
        assertFalse(includeFolder.isOnSourcePath());
        assertFalse(ebinFolder.isOnSourcePath());
    }

    // boolean isOnIncludePath();
    @Test
    public void isOnIncludePath() throws Exception {
        assertFalse(srcFolder.isOnIncludePath());
        assertTrue(includeFolder.isOnIncludePath());
        assertFalse(ebinFolder.isOnIncludePath());
    }

    // boolean isSourcePathParent();
    @Test
    public void isSourcePathParent() throws Exception {
        final IErlFolder srcFolder2 = (IErlFolder) project2.getChildNamed("src");
        assertTrue(srcFolder2.isSourcePathParent());
        final IErlFolder includeFolder2 = (IErlFolder) project2.getChildNamed("include");
        assertFalse(includeFolder2.isSourcePathParent());
        final IErlFolder ebinFolder2 = (IErlFolder) project2.getChildNamed("ebin");
        assertFalse(ebinFolder2.isSourcePathParent());
    }

    // IErlModule findModule(String moduleName, String modulePath)
    // throws ErlModelException;
    @Test
    public void findModule() throws Exception {
        final String moduleName = module.getModuleName();
        final String name = module.getName();
        final String filePath = module.getFilePath();
        final IErlModule include = createInclude(project, "yyr.hrl", "-define(ME, yy).\n");
        final IErlModule findModule = srcFolder.findModule(moduleName, null);
        assertEquals(module, findModule);
        final IErlModule findModule2 = srcFolder.findModule(null, filePath);
        assertEquals(module, findModule2);
        final IErlModule findModule3 = includeFolder.findModule(moduleName, null);
        assertNull(findModule3);
        final IErlModule findModule4 = includeFolder.findModule(null, filePath);
        assertNull(findModule4);
        final IErlModule findModule5 = srcFolder.findModule(name, null);
        assertEquals(module, findModule5);
        // path overrides name
        final IErlModule findModule6 = srcFolder.findModule("xxaa", filePath);
        assertEquals(module, findModule6);
        final IErlModule findModule7 = includeFolder.findModule("yyr.hrl", null);
        assertEquals(include, findModule7);
        final IErlModule findModule8 = includeFolder.findModule(null,
                include.getFilePath());
        assertEquals(include, findModule8);
        final IErlModule findModule9 = includeFolder.findModule("yyr", null);
        assertNull(findModule9);
    }

    // IErlModule findInclude(String includeName, String includePath)
    // throws ErlModelException;
    @Test
    public void findInclude() throws Exception {
        final IErlModule include = createInclude(project, "yyq.hrl", "-define(ME, yy).\n");
        final IErlModule module2 = createInclude(project, "zz0.erl", "-module(zz0).\n");
        final String moduleName = include.getModuleName();
        final String name = include.getName();
        final String filePath = include.getFilePath();
        final IErlModule findInclude = srcFolder.findInclude(moduleName, null);
        assertNull(findInclude);
        final IErlModule findInclude2 = includeFolder.findInclude(moduleName, null);
        assertEquals(include, findInclude2);
        final IErlModule findInclude3 = includeFolder.findInclude(null, filePath);
        assertEquals(include, findInclude3);
        final IErlModule findInclude4 = includeFolder.findInclude(name, null);
        assertEquals(include, findInclude4);
        final IErlModule findInclude5 = includeFolder.findInclude("xxaa", filePath);
        assertEquals(include, findInclude5);
        final IErlModule findInclude6 = includeFolder.findInclude("zz0.erl", null);
        assertEquals(module2, findInclude6);
        final IErlModule findInclude7 = includeFolder.findInclude("zz0", null);
        assertNull(findInclude7);
    }

}
