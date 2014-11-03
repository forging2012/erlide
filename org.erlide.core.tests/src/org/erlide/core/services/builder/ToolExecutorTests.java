package org.erlide.core.services.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import org.erlide.core.executor.ToolExecutor;
import org.erlide.util.SystemConfiguration;
import org.junit.Test;

public class ToolExecutorTests {

    @Test
    public void test1() {
        final boolean onWindows = SystemConfiguration.getInstance().isOnWindows();
        final String cmd = onWindows ? "where" : "which";
        final String res = ToolExecutor.getToolLocation(cmd);
        assertThat(res, containsString(cmd));
    }

}
