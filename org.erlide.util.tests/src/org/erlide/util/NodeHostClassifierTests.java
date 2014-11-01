package org.erlide.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;

import org.erlide.util.NodeHostClassifier.HostnameType;
import org.erlide.util.NodeHostClassifier.NodeType;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class NodeHostClassifierTests {

    @Parameters(name = "\"{0}\",\"any\",\"any.com\": {1} {2}")
    public static Iterable<Object[]> data() {
        return Arrays.asList(new Object[][] {
                { "", NodeType.LOCAL_STANDALONE, HostnameType.NONE },
                { "main", NodeType.LOCAL_DISTRIBUTED, HostnameType.NONE },
                { "main@foo", NodeType.REMOTE, HostnameType.SHORT },
                { "main@foo.bar", NodeType.REMOTE, HostnameType.LONG },
                { "main@any", NodeType.LOCAL_DISTRIBUTED, HostnameType.SHORT },
                { "main@any.com", NodeType.LOCAL_DISTRIBUTED, HostnameType.LONG } });
    }

    private String input;
    private NodeType node;
    private HostnameType host;

    public NodeHostClassifierTests(String input, NodeType node, HostnameType host) {
        this.input = input;
        this.node = node;
        this.host = host;
    }

    @Test
    public void test() {
        final NodeHostClassifier state = new NodeHostClassifier(input, "any", "any.com");
        assertThat(state.mode, is(node));
        assertThat(state.host, is(host));
    }

}
