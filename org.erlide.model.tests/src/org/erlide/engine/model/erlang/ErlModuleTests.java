package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ISourceUnit;
import org.erlide.engine.services.parsing.ErlToken;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ErlModuleTests extends WorkspaceTest {

    private IErlModule module;
    private IErlProject project;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        project = getErlProject("testproject1");
        module = getErlModule("xx.erl");
    }

    // IErlElement getElementAt(int position) throws ErlModelException;
    @Test
    public void getElementAt() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAt(0);
        final IErlElement element1 = module.getElementAt(14);
        final IErlElement element2 = module.getElementAt(1000);
        final IErlElement element3 = module.getElementAt(50);
        assertNotNull(element);
        assertNotNull(element1);
        assertTrue(element instanceof IErlAttribute);
        assertTrue(element1 instanceof IErlAttribute);
        assertEquals("include: \"yy.hrl\"", element1.toString());
        assertNull(element2);
        assertNotNull(element3);
        assertTrue(element3 instanceof IErlFunction);
    }

    // IErlElement getElementAtLine(int lineNumber);
    @Test
    public void getElementAtLine() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(0);
        final IErlElement element1 = module.getElementAtLine(1);
        final IErlElement element2 = module.getElementAtLine(4);
        final IErlElement element3 = module.getElementAtLine(3);
        assertNotNull(element);
        assertNotNull(element1);
        assertTrue(element instanceof IErlAttribute);
        assertTrue(element1 instanceof IErlAttribute);
        assertEquals("include: \"yy.hrl\"", element1.toString());
        assertNull(element2);
        assertNotNull(element3);
        assertTrue(element3 instanceof IErlFunction);
    }

    // ModuleKind getModuleKind();
    @Test
    public void getModuleKind() throws Exception {
        // TODO more tests
        assertEquals(SourceKind.ERL, module.getSourceKind());
    }

    // Collection<IErlComment> getComments();
    @Test
    public void getComments() throws Exception {
        final IErlModule commentModule = createModule(project, "cmt.erl",
                "-module(cmt).\n% comment\n% same\nf(x) -> y.\n% last");
        commentModule.open(null);
        final Collection<IErlComment> comments = commentModule.getComments();
        final String c1 = "% comment\n% same";
        final String c2 = "% last";
        assertEquals(2, comments.size());
        final Iterator<IErlComment> iterator = comments.iterator();
        assertEquals(c1, iterator.next().getName());
        assertEquals(c2, iterator.next().getName());
    }

    // IErlImport findImport(ErlangFunction function);
    @Test
    public void findImport() throws Exception {
        final IErlModule importModule = createModule(project, "imp.erl",
                "-module(imp).\n-import(lists, [reverse/1]).\nf(L) -> reverse(L).\n");
        module.open(null);
        importModule.open(null);
        final ErlangFunction reverse_1 = new ErlangFunction("reverse", 1);
        final IErlImport import1 = module.findImport(reverse_1);
        final IErlImport import2 = importModule.findImport(reverse_1);
        final ErlangFunction reverse_2 = new ErlangFunction("reverse", 2);
        final IErlImport import3 = importModule.findImport(reverse_2);
        assertNull(import1);
        assertNotNull(import2);
        assertNull(import3);
        assertEquals(import2.getFunctions().iterator().next(), reverse_1);
    }

    // Collection<IErlImport> getImports();
    @Test
    public void getImports() throws Exception {
        final IErlModule importModule = createModule(project, "imp2.erl",
                "-module(imp2). -import(lists, [reverse/1]). f(L) -> reverse(L).");
        module.open(null);
        importModule.open(null);
        final Collection<IErlImport> imports = module.getImports();
        final Collection<IErlImport> imports2 = importModule.getImports();
        assertEquals(0, imports.size());
        assertEquals(1, imports2.size());
    }

    // IErlPreprocessorDef findPreprocessorDef(String definedName, Kind kind);
    @Test
    public void findPreprocessorDef() throws Exception {
        final IErlModule preprocessorDefModule = createModule(project, "defs.erl",
                "-module(defs).\n-define(A, hej).\n-define(B(x), x).\n"
                        + "-record(?MODULE, {a, b}).\nf(L) -> reverse(L).\n");
        preprocessorDefModule.open(null);
        final IErlPreprocessorDef def1 = preprocessorDefModule.findPreprocessorDef("A",
                ErlElementKind.MACRO_DEF);
        final IErlPreprocessorDef def2 = preprocessorDefModule.findPreprocessorDef("A",
                ErlElementKind.RECORD_DEF);
        final IErlPreprocessorDef def3 = preprocessorDefModule.findPreprocessorDef("B",
                ErlElementKind.MACRO_DEF);
        final IErlPreprocessorDef def4 = preprocessorDefModule.findPreprocessorDef(
                "?MODULE", ErlElementKind.RECORD_DEF);
        assertNotNull(def1);
        assertNull(def2);
        assertNotNull(def3);
        assertEquals("B", def3.getDefinedName());
        assertNotNull(def4);
    }

    // public Collection<IErlPreprocessorDef> getPreprocessorDefs(final Kind
    // kind);
    @Test
    public void getPreprocessorDefs() throws Exception {
        final IErlModule preprocessorDefModule = createModule(project, "yy77.erl",
                "-module(yy77).\n-define(A, hej).\n-define(B(x), x).\n"
                        + "-record(?MODULE, {a, b}).\nf(L) -> reverse(L).\n");
        preprocessorDefModule.open(null);
        final Collection<IErlPreprocessorDef> records = preprocessorDefModule
                .getPreprocessorDefs(ErlElementKind.RECORD_DEF);
        final Collection<IErlPreprocessorDef> macros = preprocessorDefModule
                .getPreprocessorDefs(ErlElementKind.MACRO_DEF);
        assertEquals(1, records.size());
        assertEquals(2, macros.size());
        final Iterator<IErlPreprocessorDef> iterator = macros.iterator();
        assertEquals("A", iterator.next().getDefinedName());
        assertEquals("B", iterator.next().getDefinedName());
    }

    // Collection<ErlangIncludeFile> getIncludedFiles() throws
    // ErlModelException;
    @Test
    public void getIncludedFiles() throws Exception {
        // createInclude(project, "yy.hrl",
        // "-define(A, hej).\n");
        final IErlModule includeLibModule = createModule(project, "zz0.erl",
                "-module(zz0).\n-include_lib(\"kernel/include/file.hrl\").\nf(_) -> ok");
        module.open(null);
        includeLibModule.open(null);
        final Collection<ErlangIncludeFile> includeFiles = module.getIncludeFiles();
        final Collection<ErlangIncludeFile> includeFiles2 = includeLibModule
                .getIncludeFiles();
        assertEquals(1, includeFiles.size());
        assertEquals("yy.hrl", includeFiles.iterator().next().getFilenameLastPart());
        assertEquals(1, includeFiles2.size());
        assertEquals("file.hrl", includeFiles2.iterator().next().getFilenameLastPart());
    }

    // void initialReconcile();
    // Empty method

    // void reconcileText(int offset, int removeLength, String newText,
    // IProgressMonitor mon);
    // void postReconcile(IProgressMonitor mon);
    @Test
    public void reconcileText() throws Exception {
        module = createModule(project, "xy.erl",
                "-module(xy). -include(\"yy.hrl\"). f(A) ->     lists:reverse(A). ");
        final ErlangFunction f_1 = new ErlangFunction("f", 1);
        final ErlangFunction abc_1 = new ErlangFunction("abc", 1);
        final ScannerService scanner = module.getScanner();
        try {
            module.open(null);
            IErlFunction function1 = module.findFunction(f_1);
            IErlFunction function2 = module.findFunction(abc_1);
            assertNotNull(function1);
            assertNull(function2);

            module.reconcileText(33, 1, "abc", null);
            function1 = module.findFunction(f_1);
            function2 = module.findFunction(abc_1);
            assertNotNull(function1);
            assertNull(function2);

            module.postReconcile(null);
            function1 = module.findFunction(f_1);
            function2 = module.findFunction(abc_1);
            assertNull(function1);
            assertNotNull(function2);
        } finally {
            scanner.dispose();
        }
    }

    // void finalReconcile();
    // Empty method

    // Set<IErlModule> getDirectDependentModules() throws ErlModelException;
    // Set<IErlModule> getAllDependentModules() throws CoreException;
    @Test
    public void getXDependentModules() throws Exception {
        module.open(null);
        final IErlModule include = getModel().findInclude("yy.hrl", null);
        final IErlModule include2 = getModel().findInclude("zz.hrl", null);
        final Set<ISourceUnit> directDependents = module.getDirectDependentModules();
        final Set<ISourceUnit> allDependents = module.getAllDependentModules();
        final Set<ISourceUnit> directDependents2 = include.getDirectDependentModules();
        final Set<ISourceUnit> allDependents2 = include.getAllDependentModules();
        final Set<ISourceUnit> directDependents3 = include2.getDirectDependentModules();
        final Set<ISourceUnit> allDependents3 = include2.getAllDependentModules();
        final Set<ISourceUnit> dependentModules = module.getDirectDependentModules();
        assertEquals(0, directDependents.size());
        assertEquals(0, allDependents.size());
        assertEquals(1, directDependents2.size());
        assertEquals(module, directDependents2.iterator().next());
        assertEquals(1, allDependents2.size());
        assertEquals(module, allDependents2.iterator().next());
        assertEquals(0, directDependents3.size());
        assertEquals(1, allDependents3.size());
        assertEquals(module, allDependents3.iterator().next());
        assertEquals(0, dependentModules.size());
    }

    // void resetAndCacheScannerAndParser(String newText) throws
    // ErlModelException;

    // String getModuleName();
    @Test
    public void getModuleName() throws Exception {
        assertEquals("xx", module.getModuleName());
    }

    // IErlFunction findFunction(ErlangFunction erlangFunction);
    @Test
    public void findFunction() throws Exception {
        module.open(null);
        final String f = "f";
        final IErlFunction function = module.findFunction(new ErlangFunction(f, 0));
        final ErlangFunction f_1 = new ErlangFunction(f, 1);
        final IErlFunction function2 = module.findFunction(f_1);
        final IErlFunction function3 = module.findFunction(new ErlangFunction(f));
        final IErlFunction function4 = module.findFunction(new ErlangFunction(f,
                ErlangFunction.ANY_ARITY));
        assertNull(function);
        assertEquals(f_1, function2.getFunction());
        assertEquals(f_1, function3.getFunction());
        assertEquals(f_1, function4.getFunction());
        assertEquals(f_1, function4.getFunction());
    }

    // IErlTypespec findTypespec(String typeName);
    @Test
    public void findTypespec() throws Exception {
        final IErlModule module2 = createModule(project, "yy4.erl", "-module(yy4).\n"
                + "-spec return_error(integer(), any()) -> no_return().\n"
                + "return_error(Line, Message) ->\n"
                + "    throw({error, {Line, ?MODULE, Message}}).");
        module.open(null);
        module2.open(null);
        final String return_error = "return_error";
        final String no_spec = "no_spec";
        final IErlTypespec typespec = module.findTypespec(return_error);
        final IErlTypespec typespec2 = module.findTypespec(no_spec);
        final IErlTypespec typespec3 = module2.findTypespec(return_error);
        final IErlTypespec typespec4 = module2.findTypespec(no_spec);
        assertNull(typespec);
        assertNull(typespec2);
        assertNotNull(typespec3);
        assertEquals(return_error, typespec3.getName());
        assertNull(typespec4);
    }

    // ErlToken getScannerTokenAt(int offset);
    @Test
    public void getScannerTokenAt() throws Exception {
        final String text = "-module(xyz). -include(\"yy.hrl\"). f(A) -> lists:reverse(A).";
        module = createModule(project, "xyz.erl", text);
        final ScannerService scanner = module.getScanner();

        try {
            module.open(null);
            final ErlToken token = scanner.getTokenAt(-1);
            assertNull(token);
            final ErlToken token2 = scanner.getTokenAt(0);
            assertNotNull(token2);
            assertEquals(ErlToken.KIND_OTHER, token2.getKind());
            final ErlToken token3 = scanner.getTokenAt(1);
            assertNotNull(token3);
            assertEquals(ErlToken.KIND_ATOM, token3.getKind());
            final ErlToken token4 = scanner.getTokenAt(12);
            assertNotNull(token4);
            assertEquals(ErlToken.KIND_OTHER, token4.getKind());
            final ErlToken token5 = scanner.getTokenAt(24);
            assertNotNull(token5);
            assertEquals(ErlToken.KIND_STRING, token5.getKind());
            final ErlToken token6 = scanner.getTokenAt(text.length() - 1);
            assertNotNull(token6);
            assertEquals(ErlToken.KIND_OTHER, token6.getKind());
            final ErlToken token7 = scanner.getTokenAt(text.length());
            assertNull(token7);
        } finally {
            scanner.dispose();
        }
    }

    // void setResource(IFile file);
    @Test
    public void setResource() throws Exception {
        final IResource resource = module.getResource();
        try {
            final String path = module.getFilePath();
            module.setResource(null);
            final String path2 = module.getFilePath();
            assertEquals(resource.getLocation().toString(), path);
            assertNull(path2);
        } finally {
            module.setResource((IFile) resource);
        }
    }

    // void addComment(IErlComment c);
    // List<IErlModule> findAllIncludedFiles() throws CoreException;
    @Test
    public void findAllIncludedFiles() throws Exception {
        module.open(null);
        final List<IErlModule> includedFiles = Lists.newArrayList(ErlangEngine
                .getInstance().getModelFindService().findAllIncludedFiles(module));
        assertEquals(2, includedFiles.size());
        assertEquals("yy.hrl", includedFiles.get(0).getName());
        assertEquals("zz.hrl", includedFiles.get(1).getName());
    }

    @Test
    @SuppressWarnings("unused")
    public void findAllIncludedFiles_infinite_recursion() throws Exception {
        module.open(null);
        final IErlModule include = createInclude(project, "rec1.hrl",
                "-include(\"rec2.hrl\").\n-define(A, hej).\n");
        final IErlModule include2 = createInclude(project, "rec2.hrl",
                "-include(\"rec1.hrl\").\n-define(A, hej).\n");
        module.open(null);
        final Collection<IErlModule> includedFiles2 = ErlangEngine.getInstance()
                .getModelFindService().findAllIncludedFiles(module);
    }

    // boolean isOnSourcePath();
    @Test
    public void isOnSourcePath() throws Exception {
        final IErlModule module2 = createInclude(project, "yy.erl", "-module(yy).\n");
        assertTrue(module.isOnSourcePath());
        assertFalse(module2.isOnSourcePath());
    }

    // boolean isOnIncludePath();
    @Test
    public void isOnIncludePath() throws Exception {
        final IErlModule module2 = createInclude(project, "yy.erl", "-module(yy).\n");
        assertFalse(module.isOnIncludePath());
        assertTrue(module2.isOnIncludePath());
    }

    @Test
    public void resetCachesWorks() throws Exception {
        // TODO reimplement
        // module.open(null);
        // assertTrue(module.getChildCount() > 0);
        // final ScannerService scanner = module.getScanner();
        // try {
        // module.resetAndCacheScannerAndParser(scanner.getText());
        // } finally {
        // scanner.dispose();
        // }
        // assertTrue(module.getChildCount() > 0);
    }
}
