package org.erlide.ui.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ErlSearchScope;
import org.erlide.engine.services.search.ErlangSearchPattern;
import org.erlide.engine.services.search.LimitTo;
import org.erlide.engine.services.search.SearchFor;
import org.erlide.engine.services.search.SearchPatternFactory;
import org.erlide.ui.internal.search.ErlSearchQuery;
import org.erlide.ui.internal.search.ErlangSearchElement;
import org.erlide.ui.internal.search.ErlangSearchResult;
import org.junit.Before;
import org.junit.Test;

public class SearchTest extends WorkspaceTest {

    private IErlProject project;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        project = getErlProject("testproject1");
    }

    @Test
    public void findExternalCallsReferencesTest() throws Exception {
        findExternalCallsTestAux(LimitTo.REFERENCES, 1);
    }

    @Test
    public void findExternalCallsDefinitionsTest() throws Exception {
        findExternalCallsTestAux(LimitTo.DEFINITIONS, 2);
    }

    @Test
    public void findExternalCallsAllOccurencesTest() throws Exception {
        findExternalCallsTestAux(LimitTo.ALL_OCCURRENCES, 3);
    }

    private void findExternalCallsTestAux(final LimitTo limitTo, final int nFoundExpected)
            throws CoreException, ErlModelException, OperationCanceledException {
        // given
        // a module a with an exported function f
        // and a module b which calls a:f()
        final IErlModule moduleA = createModule(project, "a.erl",
                "-module(a). -export([f/0]). f() -> ok.");
        final IErlModule moduleB = createModule(project, "b.erl",
                "-module(b). -export([f/0]). f() -> a:f().");
        moduleA.open(null);
        moduleB.open(null);
        // when
        // searching for the call to a:f
        final ErlangSearchPattern ref = new SearchPatternFactory(ErlangEngine
                .getInstance().getModelUtilService()).getSearchPattern(
                SearchFor.FUNCTION, "a", "f", 0, limitTo, moduleA);
        final ErlSearchScope scope = new ErlSearchScope(moduleA);
        scope.addModule(moduleB);
        final ErlSearchQuery query = new ErlSearchQuery(ref, scope, "");
        query.run(new NullProgressMonitor());
        // then
        // it should be found in module b
        final ErlangSearchResult searchResult = (ErlangSearchResult) query
                .getSearchResult();
        assertEquals(nFoundExpected, searchResult.getMatchCount());
        final List<ErlangSearchElement> result = searchResult.getResult();
        if (limitTo == LimitTo.REFERENCES) {
            // f is only referred in moduleB, but declarations matches in any
            // module as long as arity and name are equal
            assertFalse(hasModule(moduleA, result));
        } else {
            assertTrue(hasModule(moduleA, result));
        }
        assertTrue(hasModule(moduleB, result));
    }

    @Test
    public void findCallAfterRecordRef() throws Exception {
        // given
        // a module a with an exported function f
        // and a module b which calls a:f()
        final IErlModule moduleA = createModule(project, "a.erl",
                "-module(a).\n-export([f/0]).\nf() ->\n    ok.\n");
        final IErlModule moduleB = createModule(project, "b.erl",
                "-module(b).\n-export([f/0]).\nf() ->\n    #a.b,\n    a:f().\n");
        moduleA.open(null);
        moduleB.open(null);
        // when
        // searching for the call to a:f
        final ErlangSearchPattern ref = new SearchPatternFactory(ErlangEngine
                .getInstance().getModelUtilService()).getSearchPattern(
                SearchFor.FUNCTION, "a", "f", 0, LimitTo.REFERENCES, moduleA);
        final ErlSearchScope scope = new ErlSearchScope(moduleA);
        scope.addModule(moduleB);
        final ErlSearchQuery query = new ErlSearchQuery(ref, scope, "");
        query.run(new NullProgressMonitor());
        // then
        // it should be found in module b
        final ErlangSearchResult searchResult = (ErlangSearchResult) query
                .getSearchResult();
        assertEquals(1, searchResult.getMatchCount());
        final List<ErlangSearchElement> result = searchResult.getResult();
        assertTrue(hasModule(moduleB, result));
        assertFalse(hasModule(moduleA, result));
    }

    @Test
    public void findVariableRef() throws Exception {
        // given
        // a module a with an exported function f
        // and a module b which calls a:f()
        final IErlModule moduleA = createModule(project, "az.erl",
                "-module(az).\n-export([f/1]).\nf(A) ->\n    {A}.\n");
        final IErlModule moduleB = createModule(project, "bz.erl",
                "-module(bz).\n-export([f/0]).\nf(A) ->\n    [A].\n");
        moduleA.open(null);
        moduleB.open(null);
        // when
        // searching for the variable A from module a
        final ErlangSearchPattern pattern = new SearchPatternFactory(ErlangEngine
                .getInstance().getModelUtilService()).getSearchPattern(
                SearchFor.VARIABLE, null, "A", 0, LimitTo.ALL_OCCURRENCES, moduleA);
        final ErlSearchScope scope = new ErlSearchScope(moduleA, moduleB);
        final ErlSearchScope reducedScope = pattern.reduceScope(scope);
        final ErlSearchQuery query = new ErlSearchQuery(pattern, reducedScope, "");
        query.run(new NullProgressMonitor());
        // then
        // it should be found in module a
        final ErlangSearchResult searchResult = (ErlangSearchResult) query
                .getSearchResult();
        assertEquals(2, searchResult.getMatchCount());
        final List<ErlangSearchElement> result = searchResult.getResult();
        assertTrue(hasModule(moduleA, result));
        assertFalse(hasModule(moduleB, result));
    }

    private boolean hasModule(final IErlModule module,
            final List<ErlangSearchElement> result) {
        for (final ErlangSearchElement erlangSearchElement : result) {
            if (erlangSearchElement.getModule().equals(module)) {
                return true;
            }
        }
        return false;
    }
}
