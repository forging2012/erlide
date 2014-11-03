package org.erlide.core.services.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.erlide.core.executor.ToolExecutor;
import org.erlide.util.SystemConfiguration;
import org.junit.Test;

public class ToolExecutorTests {

    @Test
    public void test1() {
        final boolean onWindows = SystemConfiguration.getInstance().isOnWindows();
        final String cmd = onWindows ? "where" : "which";
        for (int i = 0; i < 20; i++) {
            final String res = ToolExecutor.getToolLocation(cmd);
            assertThat(res, containsString(cmd));
        }
    }

    public static String cmdExec(final String cmdLine) {
        String line;
        String output = "";
        try {
            final Process p = Runtime.getRuntime().exec(cmdLine);
            final BufferedReader input = new BufferedReader(new InputStreamReader(
                    p.getInputStream()));
            while ((line = input.readLine()) != null) {
                output += line + '\n';
            }
            input.close();
        } catch (final Exception ex) {
            ex.printStackTrace();
        }
        return output;
    }
}
