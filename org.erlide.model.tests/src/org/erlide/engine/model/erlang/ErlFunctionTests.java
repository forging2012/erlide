package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.List;

import org.junit.Test;

public class ErlFunctionTests extends ErlModelTestBase2 {

    // boolean isExported();
    @Test
    public void isExported() throws Exception {
        assertTrue(functionA.isExported());
        assertTrue(functionB.isExported());
        assertFalse(functionC.isExported());
    }

    // List<IErlFunctionClause> getClauses();
    @Test
    public void getClauses() throws Exception {
        final List<IErlFunctionClause> clauses = functionA.getClauses();
        assertEquals(2, clauses.size());
        final List<IErlFunctionClause> clauses2 = functionB.getClauses();
        assertEquals(0, clauses2.size());
    }

    // ErlangFunction getFunction();
    @Test
    public void getFunction() throws Exception {
        final ErlangFunction function = functionA.getFunction();
        assertEquals(new ErlangFunction("a", 1), function);
        final ErlangFunction function2 = functionB.getFunction();
        assertEquals(new ErlangFunction("b", 0), function2);
        final ErlangFunction function3 = functionC.getFunction();
        assertEquals(new ErlangFunction("c", 3), function3);
    }

    // String getNameWithArity();
    @Test
    public void getNameWithArity() throws Exception {
        final String nameWithArity = functionA.getNameWithArity();
        assertEquals("a/1", nameWithArity);
        final String nameWithArity2 = functionB.getNameWithArity();
        assertEquals("b/0", nameWithArity2);
        final String nameWithArity3 = functionC.getNameWithArity();
        assertEquals("c/3", nameWithArity3);
    }

    // String getNameWithParameters();
    @Test
    public void getNameWithParameters() throws Exception {
        final String nameWithParameters = functionA.getNameWithParameters();
        assertEquals("a(_)", nameWithParameters);
        final String nameWithParameters2 = functionB.getNameWithParameters();
        assertEquals("b()", nameWithParameters2);
        final String nameWithParameters3 = functionC.getNameWithParameters();
        assertEquals("c(_, _, _)", nameWithParameters3);
    }

    // String getComment();
    @Test
    public void getComments() throws Exception {
        final Collection<IErlComment> comments = functionA.getComments();
        assertTrue(comments.isEmpty());
        final Collection<IErlComment> comments2 = functionB.getComments();
        assertTrue(comments2.isEmpty());
        final Collection<IErlComment> comments3 = functionC.getComments();
        assertFalse(comments3.isEmpty());
        assertEquals("% make a tuple", comments3.iterator().next().getSource().trim());
    }

    // String getModuleName();
    @Test
    public void getModuleName() throws Exception {
        final String name = module2.getName();
        final String moduleName = functionA.getModuleName();
        assertEquals(name, moduleName);
        final String moduleName2 = functionB.getModuleName();
        assertEquals(name, moduleName2);
        final String moduleName3 = functionC.getModuleName();
        assertEquals(name, moduleName3);
    }
}
