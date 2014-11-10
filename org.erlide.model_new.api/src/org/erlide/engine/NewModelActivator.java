package org.erlide.engine;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.engine.new_model.internal.ErlModelManager;
import org.erlide.util.ErlLogger;
import org.erlide.util.services.ExtensionUtils;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class NewModelActivator implements BundleActivator {

    private static final String PLUGIN_ID = "org.erlide.model";
    private static IErlangEngine engine;

    @Override
    public void start(final BundleContext context) throws Exception {
        ErlLogger.debug("Starting new Erlang model api");

        ErlModelManager.INSTANCE.startup();

        engine = ExtensionUtils.getSingletonExtension(
                "org.erlide.model.api.erlangEngine", IErlangEngine.class);
        if (engine == null) {
            ErlLogger.warn("Could not instantiate Erlang engine!");
            final Status status = new Status(IStatus.ERROR, "org.erlide.model",
                    "Could not instantiate Erlang engine");
            throw new CoreException(status);
        }

        ErlLogger.debug("Started new model api");
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        engine = null;
        ErlModelManager.INSTANCE.shutdown();
    }

    public static IErlangEngine getErlangEngine() {
        return engine;
    }

    public static void cleanupStateDir() {
        if (engine == null) {
            return;
        }
        final String ndir = engine.getStateDir();
        final File fdir = new File(ndir);
        for (final File f : fdir.listFiles()) {
            if (f.isFile()) {
                f.delete();
            }
        }
    }

    public static IStatus createErrorStatus(final String msg, final Throwable e) {
        return new Status(IStatus.ERROR, PLUGIN_ID, 0, msg, e);
    }

    public static IStatus createWarningStatus(final String msg) {
        return new Status(IStatus.WARNING, PLUGIN_ID, 0, msg, null);
    }

}
