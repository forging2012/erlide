package org.erlide.util;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class Base64Test {

    @Parameters(name = "base64(\"{0}\") = \"{1}\"")
    public static Collection<String[]> data() {
        return Arrays.asList(new String[][] { { "", "" }, { "f", "Zg==" },
                { "fo", "Zm8=" }, { "foo", "Zm9v" }, { "foob", "Zm9vYg==" },
                { "fooba", "Zm9vYmE=" }, { "foobar", "Zm9vYmFy" } });
    }

    private String input;
    private String expected;

    public Base64Test(String input, String expected) {
        this.input = input;
        this.expected = expected;
    }

    @Test
    public void encode() {
        Assert.assertEquals(expected, new String(Base64.encode(input.getBytes())));
    }

    @Test
    public void decode() {
        Assert.assertEquals(input, new String(Base64.decode(expected.getBytes())));
    }

}
