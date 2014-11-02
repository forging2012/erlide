package org.erlide.ui.actions;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Objects;
import java.util.Arrays;
import java.util.Collection;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlRecordDef;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ExternalCallOpenResult;
import org.erlide.engine.services.search.FieldOpenResult;
import org.erlide.engine.services.search.IncludeOpenResult;
import org.erlide.engine.services.search.LocalCallOpenResult;
import org.erlide.engine.services.search.MacroOpenResult;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.services.search.RecordOpenResult;
import org.erlide.engine.services.search.VariableOpenResult;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.ui.prefs.plugin.NavigationPreferencePage;
import org.erlide.ui.util.ErlModelUtils;

@SuppressWarnings("all")
public class OpenUtils {
  private final ModelFindService modelFindService;
  
  public OpenUtils() {
    IErlangEngine _instance = ErlangEngine.getInstance();
    ModelFindService _modelFindService = _instance.getModelFindService();
    this.modelFindService = _modelFindService;
  }
  
  public void openOpenResult(final ITextEditor editor, final IErlModule module, final int offset, final IErlProject erlProject, final OpenResult openResult, final IErlElement element) throws CoreException, ErlModelException, PartInitException, BadLocationException, OtpErlangRangeException, RpcException {
    boolean _tripleEquals = (editor == null);
    if (_tripleEquals) {
      return;
    }
    final Object found = this.findOpenResult(editor, module, erlProject, openResult, element);
    if ((found instanceof IErlElement)) {
    } else {
      if ((found instanceof ISourceRange)) {
        ErlModelUtils.openSourceRange(module, ((ISourceRange)found));
      }
    }
  }
  
  public Object findOpenResult(final ITextEditor editor, final IErlModule module, final IErlProject project, final OpenResult openResult, final IErlElement element) throws CoreException, ErlModelException, OtpErlangRangeException, RpcException, BadLocationException {
    IErlElementLocator.Scope _xifexpression = null;
    boolean _checkAllProjects = NavigationPreferencePage.getCheckAllProjects();
    if (_checkAllProjects) {
      _xifexpression = IErlElementLocator.Scope.ALL_PROJECTS;
    } else {
      _xifexpression = IErlElementLocator.Scope.REFERENCED_PROJECTS;
    }
    final IErlElementLocator.Scope scope = _xifexpression;
    IErlangEngine _instance = ErlangEngine.getInstance();
    final IErlElementLocator model = _instance.getModel();
    return this.findOpenResult(openResult, module, project, element, model, scope, editor);
  }
  
  protected Object _findOpenResult(final ExternalCallOpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    try {
      return this.findExternalCallOrType(module, openResult, project, element, scope);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  protected Object _findOpenResult(final LocalCallOpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    try {
      return this.findLocalCall(module, project, openResult, element, scope);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  protected Object _findOpenResult(final IncludeOpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    try {
      String _name = openResult.getName();
      String _path = openResult.getPath();
      return this.modelFindService.findInclude(model, project, module, _name, _path);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  protected Object _findOpenResult(final VariableOpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    try {
      Object _xblockexpression = null;
      {
        if ((element instanceof ISourceReference)) {
          final ISourceRange range = ((ISourceReference)element).getSourceRange();
          IDocumentProvider _documentProvider = editor.getDocumentProvider();
          IEditorInput _editorInput = editor.getEditorInput();
          IDocument _document = _documentProvider.getDocument(_editorInput);
          int _offset = range.getOffset();
          int _length = range.getLength();
          final String elementText = _document.get(_offset, _length);
          String _name = openResult.getName();
          return this.modelFindService.findVariable(range, _name, elementText);
        }
        _xblockexpression = null;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  protected Object _findOpenResult(final RecordOpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    try {
      IErlPreprocessorDef _xblockexpression = null;
      {
        final ErlElementKind kind = ErlElementKind.RECORD_DEF;
        String _name = openResult.getName();
        _xblockexpression = this.modelFindService.findPreprocessorDef(module, _name, kind);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  protected Object _findOpenResult(final MacroOpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    try {
      IErlPreprocessorDef _xblockexpression = null;
      {
        final ErlElementKind kind = ErlElementKind.MACRO_DEF;
        String _name = openResult.getName();
        _xblockexpression = this.modelFindService.findPreprocessorDef(module, _name, kind);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  protected Object _findOpenResult(final FieldOpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    try {
      Object _xblockexpression = null;
      {
        String _record = openResult.getRecord();
        IErlPreprocessorDef _findPreprocessorDef = this.modelFindService.findPreprocessorDef(module, _record, 
          ErlElementKind.RECORD_DEF);
        final IErlRecordDef def = ((IErlRecordDef) _findPreprocessorDef);
        boolean _tripleNotEquals = (def != null);
        if (_tripleNotEquals) {
          String _name = openResult.getName();
          return def.getFieldNamed(_name);
        }
        _xblockexpression = null;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  protected Object _findOpenResult(final OpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    return null;
  }
  
  private IErlElement findLocalCall(final IErlModule module, final IErlProject erlProject, final LocalCallOpenResult res, final IErlElement element, final IErlElementLocator.Scope scope) throws RpcException, CoreException {
    boolean _isTypeDefOrRecordDef = this.isTypeDefOrRecordDef(res, element);
    if (_isTypeDefOrRecordDef) {
      String _fun = res.getFun();
      return this.modelFindService.findTypespec(module, _fun);
    }
    String _fun_1 = res.getFun();
    int _arity = res.getArity();
    final ErlangFunction erlangFunction = new ErlangFunction(_fun_1, _arity);
    final IErlFunction foundElement = module.findFunction(erlangFunction);
    boolean _tripleNotEquals = (foundElement != null);
    if (_tripleNotEquals) {
      return foundElement;
    }
    OtpErlangObject res2 = null;
    String moduleName = null;
    final IErlImport ei = module.findImport(erlangFunction);
    boolean _tripleNotEquals_1 = (ei != null);
    if (_tripleNotEquals_1) {
      IErlangEngine _instance = ErlangEngine.getInstance();
      final IErlModel model = _instance.getModel();
      String _importModule = ei.getImportModule();
      moduleName = _importModule;
      IErlangEngine _instance_1 = ErlangEngine.getInstance();
      OpenService _service = _instance_1.<OpenService>getService(OpenService.class);
      IResource _resource = module.getResource();
      OtpErlangList _pathVars = model.getPathVars(_resource);
      ErlangProjectProperties _properties = erlProject.getProperties();
      String _externalModules = _properties.getExternalModules();
      OtpErlangObject _sourceFromModule = _service.getSourceFromModule(_pathVars, moduleName, _externalModules);
      res2 = _sourceFromModule;
    }
    if ((res2 instanceof OtpErlangString)) {
      boolean _tripleNotEquals_2 = (moduleName != null);
      if (_tripleNotEquals_2) {
        final String modulePath = ((OtpErlangString)res2).stringValue();
        IErlangEngine _instance_2 = ErlangEngine.getInstance();
        final IErlElementLocator model_1 = _instance_2.getModel();
        return this.modelFindService.findFunction(model_1, erlProject, module, moduleName, modulePath, erlangFunction, scope);
      }
    }
    IErlangEngine _instance_3 = ErlangEngine.getInstance();
    ModelFindService _modelFindService = _instance_3.getModelFindService();
    final Collection<IErlModule> allIncludedFiles = _modelFindService.findAllIncludedFiles(module);
    for (final IErlModule includedModule : allIncludedFiles) {
      {
        final IErlFunction function = includedModule.findFunction(erlangFunction);
        boolean _tripleNotEquals_3 = (function != null);
        if (_tripleNotEquals_3) {
          return function;
        }
      }
    }
    return null;
  }
  
  protected boolean _isTypeDefOrRecordDef(final ExternalCallOpenResult res, final IErlElement element) {
    boolean _tripleNotEquals = (element != null);
    if (_tripleNotEquals) {
      ErlElementKind _kind = element.getKind();
      boolean _equals = Objects.equal(_kind, ErlElementKind.RECORD_DEF);
      if (_equals) {
        return true;
      }
      ErlElementKind _kind_1 = element.getKind();
      boolean _equals_1 = Objects.equal(_kind_1, ErlElementKind.TYPESPEC);
      if (_equals_1) {
        String _fun = res.getFun();
        String _name = element.getName();
        boolean _equals_2 = _fun.equals(_name);
        boolean _not = (!_equals_2);
        if (_not) {
          return true;
        }
      }
    }
    return false;
  }
  
  protected boolean _isTypeDefOrRecordDef(final LocalCallOpenResult res, final IErlElement element) {
    boolean _tripleNotEquals = (element != null);
    if (_tripleNotEquals) {
      ErlElementKind _kind = element.getKind();
      boolean _equals = Objects.equal(_kind, ErlElementKind.RECORD_DEF);
      if (_equals) {
        return true;
      }
      ErlElementKind _kind_1 = element.getKind();
      boolean _equals_1 = Objects.equal(_kind_1, ErlElementKind.TYPESPEC);
      if (_equals_1) {
        String _fun = res.getFun();
        String _name = element.getName();
        boolean _equals_2 = _fun.equals(_name);
        boolean _not = (!_equals_2);
        if (_not) {
          return true;
        }
      }
    }
    return false;
  }
  
  private IErlElement findExternalCallOrType(final IErlModule module, final ExternalCallOpenResult res, final IErlProject project, final IErlElement element, final IErlElementLocator.Scope scope) throws CoreException {
    IErlangEngine _instance = ErlangEngine.getInstance();
    final IErlElementLocator model = _instance.getModel();
    boolean _isTypeDefOrRecordDef = this.isTypeDefOrRecordDef(res, element);
    if (_isTypeDefOrRecordDef) {
      String _mod = res.getMod();
      String _fun = res.getFun();
      String _path = res.getPath();
      return this.modelFindService.findTypeDef(model, project, module, _mod, _fun, _path, scope);
    }
    String _mod_1 = res.getMod();
    String _path_1 = res.getPath();
    String _fun_1 = res.getFun();
    int _arity = res.getArity();
    ErlangFunction _erlangFunction = new ErlangFunction(_fun_1, _arity);
    final IErlFunction result = this.modelFindService.findFunction(model, project, module, _mod_1, _path_1, _erlangFunction, scope);
    boolean _tripleNotEquals = (result != null);
    if (_tripleNotEquals) {
      return result;
    }
    String _mod_2 = res.getMod();
    String _path_2 = res.getPath();
    String _fun_2 = res.getFun();
    ErlangFunction _erlangFunction_1 = new ErlangFunction(_fun_2, ErlangFunction.ANY_ARITY);
    return this.modelFindService.findFunction(model, project, module, _mod_2, _path_2, _erlangFunction_1, scope);
  }
  
  public Object findOpenResult(final OpenResult openResult, final IErlModule module, final IErlProject project, final IErlElement element, final IErlElementLocator model, final IErlElementLocator.Scope scope, final ITextEditor editor) {
    if (openResult instanceof ExternalCallOpenResult) {
      return _findOpenResult((ExternalCallOpenResult)openResult, module, project, element, model, scope, editor);
    } else if (openResult instanceof FieldOpenResult) {
      return _findOpenResult((FieldOpenResult)openResult, module, project, element, model, scope, editor);
    } else if (openResult instanceof IncludeOpenResult) {
      return _findOpenResult((IncludeOpenResult)openResult, module, project, element, model, scope, editor);
    } else if (openResult instanceof LocalCallOpenResult) {
      return _findOpenResult((LocalCallOpenResult)openResult, module, project, element, model, scope, editor);
    } else if (openResult instanceof MacroOpenResult) {
      return _findOpenResult((MacroOpenResult)openResult, module, project, element, model, scope, editor);
    } else if (openResult instanceof RecordOpenResult) {
      return _findOpenResult((RecordOpenResult)openResult, module, project, element, model, scope, editor);
    } else if (openResult instanceof VariableOpenResult) {
      return _findOpenResult((VariableOpenResult)openResult, module, project, element, model, scope, editor);
    } else if (openResult != null) {
      return _findOpenResult(openResult, module, project, element, model, scope, editor);
    } else {
      throw new IllegalArgumentException("Unhandled parameter types: " +
        Arrays.<Object>asList(openResult, module, project, element, model, scope, editor).toString());
    }
  }
  
  public boolean isTypeDefOrRecordDef(final OpenResult res, final IErlElement element) {
    if (res instanceof ExternalCallOpenResult) {
      return _isTypeDefOrRecordDef((ExternalCallOpenResult)res, element);
    } else if (res instanceof LocalCallOpenResult) {
      return _isTypeDefOrRecordDef((LocalCallOpenResult)res, element);
    } else {
      throw new IllegalArgumentException("Unhandled parameter types: " +
        Arrays.<Object>asList(res, element).toString());
    }
  }
}
