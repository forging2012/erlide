package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertEquals;

import java.util.List;

import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ErlFunctionClauseTests extends WorkspaceTest {

    protected IErlModule module;
    protected IErlModule module2;
    protected IErlFunction functionA;
    protected IErlFunction functionB;
    protected IErlFunction functionC;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");

        module = getErlModule("xx.erl");
        module2 = getErlModule("zz.erl");
        module2.open(null);
        final List<IErlElement> functions = module2
                .getChildrenOfKind(ErlElementKind.FUNCTION);
        functionA = (IErlFunction) functions.get(0);
        functionB = (IErlFunction) functions.get(1);
        functionC = (IErlFunction) functions.get(2);
    }

    // public String getHead();
    @Test
    public void getHead() throws Exception {
        final String head = functionA.getHead();
        final List<IErlFunctionClause> clauses = functionA.getClauses();
        final String head2 = clauses.get(0).getHead();
        final String head3 = clauses.get(1).getHead();
        final String head4 = functionB.getHead();
        final String head5 = functionC.getHead();
        assertThat(head, is(nullValue()));
        assertThat(head2, is("(L) when is_list(L)"));
        assertEquals("(A) when is_atom(A)", head3);
        assertEquals("", head4);
        assertEquals("(A, B, C)", head5);
    }

    // public String getFunctionName();
    @Test
    public void getFunctionName() throws Exception {
        final List<IErlFunctionClause> clauses = functionA.getClauses();
        final String functionName = clauses.get(0).getFunctionName();
        final String functionName2 = clauses.get(1).getFunctionName();
        assertEquals("a", functionName);
        assertEquals("a", functionName2);
    }

    // public List<String> getParameters();
    @Test
    public void getParameters() throws Exception {
        final List<IErlFunctionClause> clauses = functionA.getClauses();
        final List<String> parameters = clauses.get(0).getParameters();
        final List<String> parameters2 = clauses.get(1).getParameters();
        final List<String> parameters3 = functionC.getParameters();
        assertEquals(Lists.newArrayList("L"), parameters);
        assertEquals(Lists.newArrayList("A"), parameters2);
        assertEquals(Lists.newArrayList("A", "B", "C"), parameters3);
    }

    // int getArity();
    @Test
    public void getArity() throws Exception {
        final int arity = functionA.getArity();
        final List<IErlFunctionClause> clauses = functionA.getClauses();
        final int arity2 = clauses.get(0).getArity();
        final int arity3 = clauses.get(1).getArity();
        final int arity4 = functionB.getArity();
        final int arity5 = functionC.getArity();
        assertEquals(1, arity);
        assertEquals(1, arity2);
        assertEquals(1, arity3);
        assertEquals(0, arity4);
        assertEquals(3, arity5);
    }

}
