/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.util;

import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.BindingsImpl;
import org.erlide.util.erlang.OtpErlang;
import org.erlide.util.erlang.TermParser;
import org.erlide.util.erlang.TermParserException;
import org.junit.Assert;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 *
 *
 * @author Vlad Dumitrescu
 */
public class PatternMatchTest {

    private final TermParser termParser = OtpErlang.getTermParser();

    @Test
    public void testFormatParser_simple_1() throws Exception {
        final OtpErlangObject value = OtpErlang.format("~a", "hej");
        final OtpErlangObject expected = OtpErlang.parse("hej");
        Assert.assertEquals(expected, value);
    }

    @Test
    public void testFormatParser_simple_2() throws Exception {
        final OtpErlangObject value = OtpErlang.format("~s", "hej");
        final OtpErlangObject expected = OtpErlang.parse("\"hej\"");
        Assert.assertEquals(expected, value);
    }

    @Test
    public void testFormatParser_simple_3() throws Exception {
        final OtpErlangObject value = OtpErlang.format("~x");
        final OtpErlangObject expected = OtpErlang.parse("~x");
        Assert.assertEquals(expected, value);
    }

    @Test
    public void testFormatParser_list() throws Exception {
        final OtpErlangObject value = OtpErlang.format("[~a,2,~a]", "hej", "brr");
        final OtpErlangObject expected = OtpErlang.parse("[hej,2,brr]");
        Assert.assertEquals(expected, value);
    }

    @Test
    public void testFormatParser_tuple() throws Exception {
        final OtpErlangObject value = OtpErlang.format("{~a,2,~a}", "hej", "brr");
        final OtpErlangObject expected = OtpErlang.parse("{hej,2,brr}");
        Assert.assertEquals(expected, value);
    }

    @Test
    public void testFormatParser_full() throws Exception {
        final OtpErlangObject value = OtpErlang.format("[~a,{2,~a},5]", "hej", "brr");
        final OtpErlangObject expected = OtpErlang.parse("[hej,{2,brr},5]");
        Assert.assertEquals(expected, value);
    }

    @Test
    public void testMatch_novar() throws Exception {
        final OtpErlangObject p = OtpErlang.parse("[a, {b}]");
        final OtpErlangObject t1 = OtpErlang.parse("[a, {b}]");
        final Bindings r = OtpErlang.match(p, t1);
        Assert.assertNotNull(r);
    }

    @Test
    public void testMatch() throws Exception {
        final Bindings r = OtpErlang.match("[W, V]", "[a, b]");
        Assert.assertEquals(r.get("W"), new OtpErlangAtom("a"));
        Assert.assertEquals(r.get("V"), new OtpErlangAtom("b"));
    }

    @Test
    public void testMatch_0() throws Exception {
        final Bindings b = new BindingsImpl();
        b.put("W", new OtpErlangAtom("a"));
        final Bindings r = OtpErlang.match("[W, V]", "[a, b]", b);
        Assert.assertNotNull(r);
        Assert.assertEquals(r.get("V"), new OtpErlangAtom("b"));
    }

    @Test
    public void testMatch_1() throws Exception {
        final Bindings r = OtpErlang.match("[W, V]", "[\"a\", {[1, 2]}]");
        Assert.assertEquals(r.get("W"), new OtpErlangString("a"));
        Assert.assertEquals(r.get("V"), OtpErlang.parse("{[1, 2]}"));
    }

    @Test
    public void testMatch_same() throws Exception {
        final Bindings r = OtpErlang.match("[W, {V}]", "[a, {a}]");
        Assert.assertEquals(r.get("W"), new OtpErlangAtom("a"));
    }

    @Test
    public void testMatch_any() throws Exception {
        final Bindings r = OtpErlang.match("[_, {_}]", "[a, {b}]");
        Assert.assertNotNull(r);
    }

    @Test
    public void testMatch_same_fail() throws Exception {
        final Bindings r = OtpErlang.match("[W, {W}]", "[a, {b}]");
        Assert.assertNull(r);
    }

    @Test
    public void testMatch_sig_a() throws Exception {
        final Bindings r = OtpErlang.match("W:a", "zzz");
        Assert.assertEquals(r.get("W"), new OtpErlangAtom("zzz"));
    }

    @Test
    public void testMatch_sig_i() throws Exception {
        final Bindings r = OtpErlang.match("W:i", "222");
        Assert.assertEquals(r.get("W"), new OtpErlangLong(222));
    }

    @Test
    public void testMatch_sig_fail() throws Exception {
        final Bindings r = OtpErlang.match("W:i", "zzz");
        Assert.assertNull(r);
    }

    @Test
    public void testMatch_ellipsis_1() throws Exception {
        final OtpErlangObject r = termParser.parse("[x|_]");
        Assert.assertNotNull(r);
    }

    @Test
    public void testMatch_ellipsis_2() throws Exception {
        final Bindings r = OtpErlang.match("[X | T]", "[x,y,z]");
        Assert.assertNotNull(r);
        Assert.assertEquals(new OtpErlangAtom("x"), r.get("X"));
        Assert.assertEquals(termParser.parse("[y,z]"), r.get("T"));
    }

    @Test()
    public void testMatch_ellipsis_4() throws Exception {
        final Bindings r = OtpErlang.match("[X | y]", "[x,y,z]");
        Assert.assertNull(r);
    }

    @Test(expected = TermParserException.class)
    public void testMatch_ellipsis_5() throws Exception {
        OtpErlang.match("[X | Y, Z]", "[x,y,z]");
    }

    @Test
    public void testMatch_t() throws Exception {
        final Bindings r = OtpErlang.match("[W:a, V:i]", "[a, 1]");
        Assert.assertEquals(r.getAs("W", String.class), "a");
        Assert.assertEquals(r.getAs("V", Integer.class), Integer.valueOf(1));
    }

}
