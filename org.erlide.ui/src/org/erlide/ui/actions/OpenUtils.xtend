package org.erlide.ui.actions

import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangRangeException
import com.ericsson.otp.erlang.OtpErlangString
import java.util.Collection
import org.eclipse.core.runtime.CoreException
import org.eclipse.jface.text.BadLocationException
import org.eclipse.ui.PartInitException
import org.eclipse.ui.texteditor.ITextEditor
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.ErlModelException
import org.erlide.engine.model.IErlModel
import org.erlide.engine.model.erlang.ErlangFunction
import org.erlide.engine.model.erlang.IErlFunction
import org.erlide.engine.model.erlang.IErlImport
import org.erlide.engine.model.erlang.IErlModule
import org.erlide.engine.model.erlang.IErlRecordDef
import org.erlide.engine.model.erlang.ISourceRange
import org.erlide.engine.model.erlang.ISourceReference
import org.erlide.engine.model.root.ErlElementKind
import org.erlide.engine.model.root.IErlElement
import org.erlide.engine.model.root.IErlElementLocator
import org.erlide.engine.model.root.IErlProject
import org.erlide.engine.services.search.ExternalCallOpenResult
import org.erlide.engine.services.search.FieldOpenResult
import org.erlide.engine.services.search.IncludeOpenResult
import org.erlide.engine.services.search.LocalCallOpenResult
import org.erlide.engine.services.search.MacroOpenResult
import org.erlide.engine.services.search.ModelFindService
import org.erlide.engine.services.search.OpenResult
import org.erlide.engine.services.search.OpenService
import org.erlide.engine.services.search.RecordOpenResult
import org.erlide.engine.services.search.VariableOpenResult
import org.erlide.runtime.rpc.RpcException
import org.erlide.ui.prefs.plugin.NavigationPreferencePage
import org.erlide.ui.util.ErlModelUtils

class OpenUtils {

    val ModelFindService modelFindService

    new() {
        modelFindService = ErlangEngine.getInstance().getModelFindService()
    }

    def void openOpenResult(ITextEditor editor, IErlModule module, int offset, IErlProject erlProject,
        OpenResult openResult, IErlElement element) throws CoreException, ErlModelException,
            PartInitException, BadLocationException, OtpErlangRangeException,
            RpcException {
        if (editor === null) {
            return
        }
        val Object found = findOpenResult(editor, module, erlProject, openResult, element)
        if (found instanceof IErlElement) {
        } else if (found instanceof ISourceRange) {
            ErlModelUtils.openSourceRange(module, found)
        }
    }

    def Object findOpenResult(ITextEditor editor, IErlModule module, IErlProject project, OpenResult openResult,
        IErlElement element) throws CoreException, ErlModelException,
            OtpErlangRangeException, RpcException, BadLocationException {
        val IErlElementLocator.Scope scope = if (NavigationPreferencePage.getCheckAllProjects())
                IErlElementLocator.Scope.ALL_PROJECTS
            else
                IErlElementLocator.Scope.REFERENCED_PROJECTS
        val IErlElementLocator model = ErlangEngine.getInstance().getModel()
        return findOpenResult(openResult, module, project, element, model, scope, editor)
    }

    def dispatch Object findOpenResult(ExternalCallOpenResult openResult, IErlModule module, IErlProject project,
        IErlElement element, IErlElementLocator model, IErlElementLocator.Scope scope, ITextEditor editor) {
        findExternalCallOrType(module, openResult, project, element, scope)
    }

    def dispatch Object findOpenResult(LocalCallOpenResult openResult, IErlModule module, IErlProject project,
        IErlElement element, IErlElementLocator model, IErlElementLocator.Scope scope, ITextEditor editor) {
        findLocalCall(module, project, openResult, element, scope)
    }

    def dispatch Object findOpenResult(IncludeOpenResult openResult, IErlModule module, IErlProject project,
        IErlElement element, IErlElementLocator model, IErlElementLocator.Scope scope, ITextEditor editor) {
        modelFindService.findInclude(model, project, module, openResult.getName(), openResult.getPath())
    }

    def dispatch Object findOpenResult(VariableOpenResult openResult, IErlModule module, IErlProject project,
        IErlElement element, IErlElementLocator model, IErlElementLocator.Scope scope, ITextEditor editor) {
        if (element instanceof ISourceReference) {
            val ISourceRange range = element.getSourceRange()
            val String elementText = editor.getDocumentProvider().getDocument(editor.getEditorInput()).get(
                range.getOffset(), range.getLength())
            return modelFindService.findVariable(range, openResult.getName(), elementText)
        }
        null
    }

    def dispatch Object findOpenResult(RecordOpenResult openResult, IErlModule module, IErlProject project,
        IErlElement element, IErlElementLocator model, IErlElementLocator.Scope scope, ITextEditor editor) {
        val ErlElementKind kind = ErlElementKind.RECORD_DEF
        modelFindService.findPreprocessorDef(module, openResult.getName(), kind)
    }

    def dispatch Object findOpenResult(MacroOpenResult openResult, IErlModule module, IErlProject project,
        IErlElement element, IErlElementLocator model, IErlElementLocator.Scope scope, ITextEditor editor) {
        val ErlElementKind kind = ErlElementKind.MACRO_DEF
        modelFindService.findPreprocessorDef(module, openResult.getName(), kind)
    }

    def dispatch Object findOpenResult(FieldOpenResult openResult, IErlModule module, IErlProject project,
        IErlElement element, IErlElementLocator model, IErlElementLocator.Scope scope, ITextEditor editor) {
        val IErlRecordDef def = modelFindService.findPreprocessorDef(module, openResult.getRecord(),
            ErlElementKind.RECORD_DEF) as IErlRecordDef
        if (def !== null) {
            return def.getFieldNamed(openResult.getName())
        }
        null
    }

    def dispatch Object findOpenResult(OpenResult openResult, IErlModule module, IErlProject project,
        IErlElement element, IErlElementLocator model, IErlElementLocator.Scope scope, ITextEditor editor) {
        return null
    }

    def private IErlElement findLocalCall(IErlModule module, IErlProject erlProject, LocalCallOpenResult res,
        IErlElement element, IErlElementLocator.Scope scope) throws RpcException, CoreException {
        if (isTypeDefOrRecordDef(res, element)) {
            return modelFindService.findTypespec(module, res.getFun())
        }
        val ErlangFunction erlangFunction = new ErlangFunction(res.getFun(), res.getArity())
        val IErlFunction foundElement = module.findFunction(erlangFunction)
        if (foundElement !== null) {
            return foundElement
        }

        // imported functions
        var OtpErlangObject res2 = null
        var String moduleName = null
        val IErlImport ei = module.findImport(erlangFunction)
        if (ei !== null) {
            val IErlModel model = ErlangEngine.getInstance().getModel()
            moduleName = ei.getImportModule()
            res2 = ErlangEngine.getInstance().getService(OpenService).getSourceFromModule(model.getPathVars(module.resource),
                moduleName, erlProject.getProperties().getExternalModules())
        }
        if (res2 instanceof OtpErlangString) {
            if (moduleName !== null) {

                // imported from otp module
                val String modulePath = res2.stringValue()
                val IErlElementLocator model = ErlangEngine.getInstance().getModel()
                return modelFindService.findFunction(model, erlProject, module, moduleName, modulePath, erlangFunction,
                    scope)
            }
        }

        // functions defined in include files
        val Collection<IErlModule> allIncludedFiles = ErlangEngine.getInstance().getModelFindService().
            findAllIncludedFiles(module)
        for (IErlModule includedModule : allIncludedFiles) {
            val IErlFunction function = includedModule.findFunction(erlangFunction)
            if (function !== null) {
                return function
            }
        }
        return null
    }

    def dispatch boolean isTypeDefOrRecordDef(ExternalCallOpenResult res, IErlElement element) {
        if (element !== null) {
            if (element.getKind() == ErlElementKind.RECORD_DEF) {
                return true
            }
            if (element.getKind() == ErlElementKind.TYPESPEC) {
                if (!res.getFun().equals(element.getName())) {
                    return true
                }
            }
        }
        return false
    }

    def dispatch boolean isTypeDefOrRecordDef(LocalCallOpenResult res, IErlElement element) {
        if (element !== null) {
            if (element.getKind() == ErlElementKind.RECORD_DEF) {
                return true
            }
            if (element.getKind() == ErlElementKind.TYPESPEC) {
                if (!res.getFun().equals(element.getName())) {
                    return true
                }
            }
        }
        return false
    }

    def private IErlElement findExternalCallOrType(IErlModule module, ExternalCallOpenResult res, IErlProject project,
        IErlElement element, IErlElementLocator.Scope scope) throws CoreException {
        val IErlElementLocator model = ErlangEngine.getInstance().getModel()
        if (isTypeDefOrRecordDef(res, element)) {
            return modelFindService.findTypeDef(model, project, module, res.getMod(), res.getFun(), res.getPath(), scope)
        }
        val IErlFunction result = modelFindService.findFunction(model, project, module, res.getMod(), res.getPath(),
            new ErlangFunction(res.getFun(), res.getArity()), scope)
        if (result !== null) {
            return result
        }
        return modelFindService.findFunction(model, project, module, res.getMod(), res.getPath(),
            new ErlangFunction(res.getFun(), ErlangFunction.ANY_ARITY), scope)
    }

}
