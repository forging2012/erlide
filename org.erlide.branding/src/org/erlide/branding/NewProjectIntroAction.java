package org.erlide.branding;

import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.intro.IIntroManager;
import org.eclipse.ui.intro.IIntroPart;
import org.eclipse.ui.intro.IIntroSite;
import org.eclipse.ui.intro.config.IIntroAction;
import org.eclipse.ui.wizards.IWizardDescriptor;

public class NewProjectIntroAction implements IIntroAction {

    @Override
    public void run(final IIntroSite site, final Properties params) {
        final IIntroManager introManager = site.getWorkbenchWindow().getWorkbench()
                .getIntroManager();
        final IIntroPart introPart = introManager.getIntro();
        if (introPart != null) {
            introManager.closeIntro(introPart);
        }
        openWizard(site.getShell(), site.getWorkbenchWindow().getWorkbench(), null,
                "org.erlide.ui.wizards.newproject");
    }

    public void openWizard(final Shell shell, final IWorkbench workbench,
            final IStructuredSelection selection, final String id) {
        final IWizardDescriptor descriptor = locateWizard(id);
        try {
            if (descriptor != null) {
                final IWizard wizard = descriptor.createWizard();
                ((IWorkbenchWizard) wizard).init(workbench, selection);
                final WizardDialog wd = new WizardDialog(shell, wizard);
                wd.setTitle(wizard.getWindowTitle());
                wd.open();
            }
        } catch (final CoreException e) {
            e.printStackTrace();
        }
    }

    private IWizardDescriptor locateWizard(final String id) {
        // First see if this is a "new wizard".
        IWizardDescriptor descriptor = PlatformUI.getWorkbench().getNewWizardRegistry()
                .findWizard(id);
        // If not check if it is an "import wizard".
        if (descriptor == null) {
            descriptor = PlatformUI.getWorkbench().getImportWizardRegistry()
                    .findWizard(id);
        }
        // Or maybe an export wizard
        if (descriptor == null) {
            descriptor = PlatformUI.getWorkbench().getExportWizardRegistry()
                    .findWizard(id);
        }
        return descriptor;
    }
}
