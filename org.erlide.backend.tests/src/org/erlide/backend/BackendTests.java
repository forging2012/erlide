package org.erlide.backend;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.runtime.ErtsProcess;
import org.erlide.runtime.api.RuntimeCore;
import org.junit.Test;

public class BackendTests {

    @Test
    public void crashedBackendShouldRestart() {
        final BackendData data = new BackendData(RuntimeCore.getRuntimeInfoCatalog()
                .getDefaultRuntime());
        data.setNodeName("fooz");
        data.setLongName(false);
        final IBackend b = BackendCore.getBackendManager().getFactory()
                .createBackend(data);
        assertThat("backend not running", b.isRunning());
        try {
            Thread.sleep(1000);
        } catch (final InterruptedException e) {
        }
        final Process oldProcess = b.getErtsProcess().getProcess();
        oldProcess.destroy();
        try {
            Thread.sleep(1000);
        } catch (final InterruptedException e) {
        }
        // this is needed to internally reset erts
        b.getErtsProcess().shutDown();
        assertThat("backend not running", b.isRunning());
        final Process newProcess = b.getErtsProcess().getProcess();
        assertThat(newProcess, is(not(oldProcess)));
    }

    @Test
    public void closedBackendShouldNotRestart() {
        final BackendData data = new BackendData(RuntimeCore.getRuntimeInfoCatalog()
                .getDefaultRuntime());
        data.setNodeName("fooz");
        data.setLongName(false);
        final IBackend b = BackendCore.getBackendManager().getFactory()
                .createBackend(data);
        assertThat("backend not running", b.isRunning());
        try {
            Thread.sleep(1000);
        } catch (final InterruptedException e) {
        }
        b.dispose();
        try {
            Thread.sleep(1000);
        } catch (final InterruptedException e) {
        }

        assertThat("backend not running", !b.isRunning());
        final ErtsProcess erts = b.getErtsProcess();
        final Process newProcess = erts.getProcess();
        assertThat(newProcess, is(nullValue()));
    }

}
