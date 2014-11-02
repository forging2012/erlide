package org.erlide.ui.internal.search;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Objects;
import java.text.Collator;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.search.ui.text.Match;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.IWorkingSetManager;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.IWorkingSetSelectionDialog;
import org.eclipse.ui.progress.IProgressService;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.core.services.search.SearchCoreUtil;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlFunctionClause;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.services.search.ErlSearchScope;
import org.erlide.engine.services.search.ErlangSearchPattern;
import org.erlide.engine.services.search.ExternalCallOpenResult;
import org.erlide.engine.services.search.FieldOpenResult;
import org.erlide.engine.services.search.FunctionPattern;
import org.erlide.engine.services.search.IncludeOpenResult;
import org.erlide.engine.services.search.IncludePattern;
import org.erlide.engine.services.search.LimitTo;
import org.erlide.engine.services.search.LocalCallOpenResult;
import org.erlide.engine.services.search.MacroOpenResult;
import org.erlide.engine.services.search.MacroPattern;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.engine.services.search.ModuleLineFunctionArityRef;
import org.erlide.engine.services.search.OpenResult;
import org.erlide.engine.services.search.RecordFieldPattern;
import org.erlide.engine.services.search.RecordOpenResult;
import org.erlide.engine.services.search.RecordPattern;
import org.erlide.engine.services.search.SearchFor;
import org.erlide.engine.services.search.SearchPatternFactory;
import org.erlide.engine.services.search.TypeRefPattern;
import org.erlide.engine.services.search.VariableOpenResult;
import org.erlide.engine.services.search.VariablePattern;
import org.erlide.ui.actions.OpenUtils;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.internal.search.ErlSearchQuery;
import org.erlide.ui.internal.search.ErlangSearchElement;
import org.erlide.ui.internal.search.LRUWorkingSetsList;
import org.erlide.util.StringUtils;
import org.erlide.util.Util;
import org.osgi.framework.Bundle;

@SuppressWarnings("all")
public class SearchUtil {
  public static class WorkingSetComparator implements Comparator<IWorkingSet> {
    private static Collator collator = Collator.getInstance();
    
    public int compare(final IWorkingSet o1, final IWorkingSet o2) {
      String _label = o1.getLabel();
      String _label_1 = o2.getLabel();
      return SearchUtil.WorkingSetComparator.collator.compare(_label, _label_1);
    }
  }
  
  public static int SEARCH_IN_SOURCES = 1;
  
  public static int SEARCH_IN_EXTERNALS = 2;
  
  public static int SEARCH_IN_OTP_LIBRARIES = 4;
  
  private static int ARI_TYPESPEC = (-2);
  
  private static int ARI_ATTRIBUTE = (-3);
  
  private static int ARI_RECORD_DEF = (-4);
  
  private static int ARI_MACRO_DEF = (-5);
  
  private static int ARI_INCLUDE = (-6);
  
  private static int ARI_RECORD_FIELD_DEF = (-7);
  
  public static ErlSearchScope getSelectionScope(final ISelection selection, final boolean addExternals, final boolean addOtp) throws CoreException {
    final ErlSearchScope result = new ErlSearchScope();
    final Set<String> externalModulePaths = new HashSet<String>();
    if ((selection instanceof IStructuredSelection)) {
      List _list = ((IStructuredSelection)selection).toList();
      for (final Object obj : _list) {
        boolean _matched = false;
        if (!_matched) {
          if (obj instanceof IResource) {
            _matched=true;
            SearchCoreUtil.addResourceToScope(result, ((IResource)obj));
          }
        }
        if (!_matched) {
          if (obj instanceof IErlModule) {
            _matched=true;
            result.addModule(((IErlModule)obj));
          }
        }
        if (!_matched) {
          if (obj instanceof IErlElement) {
            _matched=true;
            SearchCoreUtil.addExternalModules(((IErlElement)obj), result, externalModulePaths, addExternals, addOtp);
          }
        }
      }
    }
    return result;
  }
  
  public static Match createMatch(final ModuleLineFunctionArityRef ref, final Map<String, IErlModule> pathToModuleMap) {
    final ErlangSearchElement ese = SearchUtil.createSearchElementFromRef(ref, pathToModuleMap);
    int _offset = ref.getOffset();
    int _length = ref.getLength();
    return new Match(ese, _offset, _length);
  }
  
  public static ErlangSearchElement createSearchElementFromRef(final ModuleLineFunctionArityRef ref, final Map<String, IErlModule> pathToModuleMap) {
    String _modulePath = ref.getModulePath();
    final IErlModule module = pathToModuleMap.get(_modulePath);
    return SearchUtil.createSearchElement(ref, module);
  }
  
  public static ErlangSearchElement createSearchElement(final ModuleLineFunctionArityRef ref, final IErlModule module) {
    String _modulePath = ref.getModulePath();
    String _name = ref.getName();
    int _arity = ref.getArity();
    String _clauseHead = ref.getClauseHead();
    boolean _isSubClause = ref.isSubClause();
    ErlElementKind _refToKind = SearchUtil.refToKind(ref);
    return new ErlangSearchElement(module, _modulePath, _name, _arity, _clauseHead, _isSubClause, _refToKind);
  }
  
  public static ErlElementKind refToKind(final ModuleLineFunctionArityRef ref) {
    int _arity = ref.getArity();
    boolean _matched = false;
    if (!_matched) {
      if (Objects.equal(_arity, SearchUtil.ARI_TYPESPEC)) {
        _matched=true;
        return ErlElementKind.TYPESPEC;
      }
    }
    if (!_matched) {
      if (Objects.equal(_arity, SearchUtil.ARI_ATTRIBUTE)) {
        _matched=true;
        return ErlElementKind.ATTRIBUTE;
      }
    }
    if (!_matched) {
      if (Objects.equal(_arity, SearchUtil.ARI_RECORD_DEF)) {
        _matched=true;
        return ErlElementKind.RECORD_DEF;
      }
    }
    if (!_matched) {
      if (Objects.equal(_arity, SearchUtil.ARI_MACRO_DEF)) {
        _matched=true;
        return ErlElementKind.MACRO_DEF;
      }
    }
    if (!_matched) {
      if (Objects.equal(_arity, SearchUtil.ARI_INCLUDE)) {
        _matched=true;
        return ErlElementKind.ATTRIBUTE;
      }
    }
    if (!_matched) {
      if (Objects.equal(_arity, SearchUtil.ARI_RECORD_FIELD_DEF)) {
        _matched=true;
        return ErlElementKind.RECORD_FIELD;
      }
    }
    boolean _isSubClause = ref.isSubClause();
    if (_isSubClause) {
      return ErlElementKind.CLAUSE;
    }
    return ErlElementKind.FUNCTION;
  }
  
  protected static ErlangSearchPattern _getSearchPatternFromOpenResultAndLimitTo(final ExternalCallOpenResult res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    String moduleName = res.getMod();
    boolean _tripleEquals = (moduleName == null);
    if (_tripleEquals) {
      return null;
    }
    final String unquoted = StringUtils.unquote(moduleName);
    boolean _and = false;
    boolean _tripleNotEquals = (module != null);
    if (!_tripleNotEquals) {
      _and = false;
    } else {
      _and = (offset != (-1));
    }
    if (_and) {
      final IErlElement e = module.getElementAt(offset);
      boolean _and_1 = false;
      boolean _tripleNotEquals_1 = (e != null);
      if (!_tripleNotEquals_1) {
        _and_1 = false;
      } else {
        boolean _or = false;
        ErlElementKind _kind = e.getKind();
        boolean _equals = Objects.equal(_kind, ErlElementKind.TYPESPEC);
        if (_equals) {
          _or = true;
        } else {
          ErlElementKind _kind_1 = e.getKind();
          boolean _equals_1 = Objects.equal(_kind_1, ErlElementKind.RECORD_DEF);
          _or = _equals_1;
        }
        _and_1 = _or;
      }
      if (_and_1) {
        String _fun = res.getFun();
        return new TypeRefPattern(moduleName, _fun, limitTo);
      }
    }
    String oldName = null;
    moduleName = unquoted;
    do {
      {
        oldName = moduleName;
        IErlangEngine _instance = ErlangEngine.getInstance();
        ModelFindService _modelFindService = _instance.getModelFindService();
        String _resolveMacroValue = _modelFindService.resolveMacroValue(moduleName, module);
        moduleName = _resolveMacroValue;
      }
    } while((!moduleName.equals(oldName)));
    String _fun_1 = res.getFun();
    int _arity = res.getArity();
    return new FunctionPattern(moduleName, _fun_1, _arity, limitTo, matchAnyFunctionDefinition, module, false);
  }
  
  protected static ErlangSearchPattern _getSearchPatternFromOpenResultAndLimitTo(final LocalCallOpenResult res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    String moduleName = null;
    boolean _tripleNotEquals = (module != null);
    if (_tripleNotEquals) {
      String _moduleName = module.getModuleName();
      moduleName = _moduleName;
      if ((offset != (-1))) {
        final IErlElement e = module.getElementAt(offset);
        OpenUtils _openUtils = new OpenUtils();
        boolean _isTypeDefOrRecordDef = _openUtils.isTypeDefOrRecordDef(res, e);
        if (_isTypeDefOrRecordDef) {
          String _fun = res.getFun();
          return new TypeRefPattern(moduleName, _fun, limitTo);
        }
      }
    }
    String _fun_1 = res.getFun();
    int _arity = res.getArity();
    return new FunctionPattern(moduleName, _fun_1, _arity, limitTo, matchAnyFunctionDefinition, module, true);
  }
  
  protected static ErlangSearchPattern _getSearchPatternFromOpenResultAndLimitTo(final MacroOpenResult res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    String _name = res.getName();
    final String unquoted = StringUtils.unquote(_name);
    return new MacroPattern(unquoted, limitTo);
  }
  
  protected static ErlangSearchPattern _getSearchPatternFromOpenResultAndLimitTo(final RecordOpenResult res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    String _name = res.getName();
    final String unquoted = StringUtils.unquote(_name);
    return new RecordPattern(unquoted, limitTo);
  }
  
  protected static ErlangSearchPattern _getSearchPatternFromOpenResultAndLimitTo(final IncludeOpenResult res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    String _name = res.getName();
    return new IncludePattern(_name, limitTo);
  }
  
  protected static ErlangSearchPattern _getSearchPatternFromOpenResultAndLimitTo(final VariableOpenResult res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    boolean _tripleNotEquals = (module != null);
    if (_tripleNotEquals) {
      if ((offset != (-1))) {
        final IErlElement e = module.getElementAt(offset);
        if ((e instanceof IErlFunctionClause)) {
          String _functionName = ((IErlFunctionClause)e).getFunctionName();
          int _arity = ((IErlFunctionClause)e).getArity();
          String _head = ((IErlFunctionClause)e).getHead();
          String _name = res.getName();
          return new VariablePattern(_functionName, _arity, _head, _name, limitTo, module);
        }
      }
    }
    return null;
  }
  
  protected static ErlangSearchPattern _getSearchPatternFromOpenResultAndLimitTo(final FieldOpenResult res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    String _name = res.getName();
    final String unquoted = StringUtils.unquote(_name);
    String _record = res.getRecord();
    return new RecordFieldPattern(_record, unquoted, limitTo);
  }
  
  protected static ErlangSearchPattern _getSearchPatternFromOpenResultAndLimitTo(final Void res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    return null;
  }
  
  public static ErlangSearchPattern getSearchPattern(final IErlModule module, final SearchFor searchFor, final String pattern, final LimitTo limitTo) {
    String moduleName = "";
    String name = pattern;
    int arity = (-1);
    int p = pattern.indexOf(":");
    if ((p != (-1))) {
      String _substring = pattern.substring(0, p);
      moduleName = _substring;
      String _substring_1 = pattern.substring((p + 1));
      name = _substring_1;
    }
    int _indexOf = name.indexOf("/");
    p = _indexOf;
    if ((p != (-1))) {
      String _substring_2 = name.substring((p + 1));
      Integer _valueOf = Integer.valueOf(_substring_2);
      arity = (_valueOf).intValue();
      String _substring_3 = name.substring(0, p);
      name = _substring_3;
    }
    IErlangEngine _instance = ErlangEngine.getInstance();
    ModelUtilService _modelUtilService = _instance.getModelUtilService();
    SearchPatternFactory _searchPatternFactory = new SearchPatternFactory(_modelUtilService);
    return _searchPatternFactory.getSearchPattern(searchFor, moduleName, name, arity, limitTo, module);
  }
  
  public static void runQuery(final ErlangSearchPattern pattern, final ErlSearchScope scope, final String scopeDescription, final Shell shell) {
    final ErlSearchQuery query = new ErlSearchQuery(pattern, scope, scopeDescription);
    boolean _canRunInBackground = query.canRunInBackground();
    if (_canRunInBackground) {
      NewSearchUI.runQueryInBackground(query);
    } else {
      IWorkbench _workbench = PlatformUI.getWorkbench();
      final IProgressService progressService = _workbench.getProgressService();
      final IStatus status = NewSearchUI.runQueryInForeground(progressService, query);
      boolean _matches = status.matches(((IStatus.ERROR + IStatus.INFO) + IStatus.WARNING));
      if (_matches) {
        ErrorDialog.openError(shell, "Search", 
          ("Problems occurred while searching. " + "The affected files will be skipped."), status);
      }
    }
  }
  
  public static String getWorkingSetsScopeDescription(final IWorkingSet[] workingSets) {
    final String wssS = "working sets ";
    final String wsS = "working set ";
    int _length = workingSets.length;
    boolean _equals = (_length == 0);
    if (_equals) {
      return "";
    }
    String _xifexpression = null;
    int _length_1 = workingSets.length;
    boolean _equals_1 = (_length_1 == 1);
    if (_equals_1) {
      _xifexpression = wsS;
    } else {
      _xifexpression = wssS;
    }
    final String s = _xifexpression;
    return SearchUtil.workingSetLabels(workingSets, s, "\'");
  }
  
  public static String workingSetLabels(final IWorkingSet[] workingSets, final String s, final String surround) {
    final StringBuilder sb = new StringBuilder(s);
    final Function1<IWorkingSet, StringBuilder> _function = new Function1<IWorkingSet, StringBuilder>() {
      public StringBuilder apply(final IWorkingSet ws) {
        StringBuilder _append = sb.append(surround);
        String _label = ws.getLabel();
        StringBuilder _append_1 = _append.append(_label);
        StringBuilder _append_2 = _append_1.append(surround);
        return _append_2.append(", ");
      }
    };
    final Function1<IWorkingSet, StringBuilder> cb = _function;
    IWorkingSet _head = IterableExtensions.<IWorkingSet>head(((Iterable<IWorkingSet>)Conversions.doWrapArray(workingSets)));
    boolean _tripleNotEquals = (_head != null);
    if (_tripleNotEquals) {
      IWorkingSet _head_1 = IterableExtensions.<IWorkingSet>head(((Iterable<IWorkingSet>)Conversions.doWrapArray(workingSets)));
      cb.apply(_head_1);
    }
    Iterable<IWorkingSet> _tail = IterableExtensions.<IWorkingSet>tail(((Iterable<IWorkingSet>)Conversions.doWrapArray(workingSets)));
    IWorkingSet _head_2 = IterableExtensions.<IWorkingSet>head(_tail);
    boolean _tripleNotEquals_1 = (_head_2 != null);
    if (_tripleNotEquals_1) {
      Iterable<IWorkingSet> _tail_1 = IterableExtensions.<IWorkingSet>tail(((Iterable<IWorkingSet>)Conversions.doWrapArray(workingSets)));
      IWorkingSet _head_3 = IterableExtensions.<IWorkingSet>head(_tail_1);
      cb.apply(_head_3);
    }
    int _length = workingSets.length;
    boolean _greaterThan = (_length > 2);
    if (_greaterThan) {
      StringBuilder _append = sb.append("... ");
      return _append.toString();
    }
    int _length_1 = sb.length();
    int _minus = (_length_1 - 2);
    return sb.substring(0, _minus);
  }
  
  public static ErlSearchScope getWorkingSetsScope(final IWorkingSet[] workingSets, final boolean addExternals, final boolean addOTP) throws CoreException {
    final ErlSearchScope result = new ErlSearchScope();
    final Set<String> externalModulePaths = new HashSet<String>();
    boolean _tripleEquals = (workingSets == null);
    if (_tripleEquals) {
      return result;
    }
    for (final IWorkingSet ws : workingSets) {
      {
        final IAdaptable[] elements = ws.getElements();
        for (final IAdaptable a : elements) {
          {
            Object _adapter = a.getAdapter(IResource.class);
            final IResource r = ((IResource) _adapter);
            SearchCoreUtil.addResourceToScope(result, r);
            IErlElement parent = null;
            Object o = a.getAdapter(IErlElement.class);
            if ((o instanceof IErlElement)) {
              parent = ((IErlElement)o);
            } else {
              Object _adapter_1 = a.getAdapter(IResource.class);
              o = _adapter_1;
              boolean _tripleNotEquals = (o != null);
              if (_tripleNotEquals) {
                final IResource resource = ((IResource) o);
                IErlangEngine _instance = ErlangEngine.getInstance();
                IErlModel _model = _instance.getModel();
                final IErlElement element = _model.findElement(resource);
                parent = element;
              }
            }
            boolean _tripleNotEquals_1 = (parent != null);
            if (_tripleNotEquals_1) {
              SearchCoreUtil.addExternalModules(parent, result, externalModulePaths, addExternals, addOTP);
            }
          }
        }
      }
    }
    return result;
  }
  
  public static String getProjectScopeDescription(final Collection<IProject> projects) {
    boolean _or = false;
    boolean _tripleEquals = (projects == null);
    if (_tripleEquals) {
      _or = true;
    } else {
      boolean _isEmpty = projects.isEmpty();
      _or = _isEmpty;
    }
    if (_or) {
      return "";
    }
    String _xifexpression = null;
    int _size = projects.size();
    boolean _equals = (_size == 1);
    if (_equals) {
      _xifexpression = "project";
    } else {
      _xifexpression = "projects";
    }
    final StringBuilder sb = new StringBuilder(_xifexpression);
    sb.append(" ");
    final Function1<IProject, StringBuilder> _function = new Function1<IProject, StringBuilder>() {
      public StringBuilder apply(final IProject p) {
        StringBuilder _append = sb.append("\'");
        String _name = p.getName();
        StringBuilder _append_1 = _append.append(_name);
        return _append_1.append("\', ");
      }
    };
    final Function1<IProject, StringBuilder> cb = _function;
    IProject _head = IterableExtensions.<IProject>head(projects);
    boolean _tripleNotEquals = (_head != null);
    if (_tripleNotEquals) {
      IProject _head_1 = IterableExtensions.<IProject>head(projects);
      cb.apply(_head_1);
    }
    Iterable<IProject> _tail = IterableExtensions.<IProject>tail(projects);
    IProject _head_2 = IterableExtensions.<IProject>head(_tail);
    boolean _tripleNotEquals_1 = (_head_2 != null);
    if (_tripleNotEquals_1) {
      Iterable<IProject> _tail_1 = IterableExtensions.<IProject>tail(projects);
      IProject _head_3 = IterableExtensions.<IProject>head(_tail_1);
      cb.apply(_head_3);
    }
    int _size_1 = projects.size();
    boolean _greaterThan = (_size_1 > 2);
    if (_greaterThan) {
      StringBuilder _append = sb.append("... ");
      return _append.toString();
    }
    int _length = sb.length();
    int _minus = (_length - 2);
    return sb.substring(0, _minus);
  }
  
  public static String getSelectionScopeDescription(final ISelection selection) {
    if ((selection instanceof IStructuredSelection)) {
      final StringBuilder sb = new StringBuilder();
      final List<?> list = ((IStructuredSelection)selection).toList();
      boolean _isEmpty = list.isEmpty();
      if (_isEmpty) {
        return "";
      }
      final Function1<Object, StringBuilder> _function = new Function1<Object, StringBuilder>() {
        public StringBuilder apply(final Object obj) {
          StringBuilder _xblockexpression = null;
          {
            String name = null;
            if ((obj instanceof IResource)) {
              String _name = ((IResource)obj).getName();
              name = _name;
            } else {
              if ((obj instanceof IErlElement)) {
                String _name_1 = ((IErlElement)obj).getName();
                name = _name_1;
              } else {
                name = "?";
              }
            }
            StringBuilder _append = sb.append("\'");
            StringBuilder _append_1 = _append.append(name);
            _xblockexpression = _append_1.append("\', ");
          }
          return _xblockexpression;
        }
      };
      final Function1<Object, StringBuilder> cb = _function;
      Object _head = IterableExtensions.head(list);
      boolean _tripleNotEquals = (_head != null);
      if (_tripleNotEquals) {
        Object _head_1 = IterableExtensions.head(list);
        cb.apply(_head_1);
      }
      Iterable<?> _tail = IterableExtensions.tail(list);
      Object _head_2 = IterableExtensions.head(_tail);
      boolean _tripleNotEquals_1 = (_head_2 != null);
      if (_tripleNotEquals_1) {
        Iterable<?> _tail_1 = IterableExtensions.tail(list);
        Object _head_3 = IterableExtensions.head(_tail_1);
        cb.apply(_head_3);
      }
      int _size = ((IStructuredSelection)selection).size();
      boolean _greaterThan = (_size > 2);
      if (_greaterThan) {
        StringBuilder _append = sb.append("...");
        return _append.toString();
      }
      int _length = sb.length();
      int _minus = (_length - 2);
      return sb.substring(0, _minus);
    }
    return "";
  }
  
  public static String getWorkspaceScopeDescription() {
    return "workspace";
  }
  
  public static String toString(final IWorkingSet[] workingSets) {
    SearchUtil.WorkingSetComparator _workingSetComparator = new SearchUtil.WorkingSetComparator();
    Arrays.<IWorkingSet>sort(workingSets, _workingSetComparator);
    return SearchUtil.workingSetLabels(workingSets, "", "");
  }
  
  private final static int LRU_WORKINGSET_LIST_SIZE = 3;
  
  private static LRUWorkingSetsList fgLRUWorkingSets;
  
  private static String DIALOG_SETTINGS_KEY = "ErlangElementSearchActions";
  
  private static String STORE_LRU_WORKING_SET_NAMES = "lastUsedWorkingSetNames";
  
  public static boolean isSearchPlugInActivated() {
    Bundle _bundle = Platform.getBundle("org.eclipse.search");
    int _state = _bundle.getState();
    return (_state == Bundle.ACTIVE);
  }
  
  public static IDialogSettings getDialogStoreSection() {
    ErlideUIPlugin _default = ErlideUIPlugin.getDefault();
    final IDialogSettings dialogSettings = _default.getDialogSettings();
    IDialogSettings settingsStore = dialogSettings.getSection(SearchUtil.DIALOG_SETTINGS_KEY);
    boolean _tripleEquals = (settingsStore == null);
    if (_tripleEquals) {
      IDialogSettings _addNewSection = dialogSettings.addNewSection(SearchUtil.DIALOG_SETTINGS_KEY);
      settingsStore = _addNewSection;
    }
    return settingsStore;
  }
  
  public static LRUWorkingSetsList getLRUWorkingSets() {
    boolean _tripleEquals = (SearchUtil.fgLRUWorkingSets == null);
    if (_tripleEquals) {
      SearchUtil.restoreState();
    }
    return SearchUtil.fgLRUWorkingSets;
  }
  
  public static void restoreState() {
    LRUWorkingSetsList _lRUWorkingSetsList = new LRUWorkingSetsList(SearchUtil.LRU_WORKINGSET_LIST_SIZE);
    SearchUtil.fgLRUWorkingSets = _lRUWorkingSetsList;
    final IDialogSettings settingsStore = SearchUtil.getDialogStoreSection();
    for (int i = (SearchUtil.LRU_WORKINGSET_LIST_SIZE - 1); (i >= 0); i--) {
      {
        final String[] lruWorkingSetNames = settingsStore.getArray((SearchUtil.STORE_LRU_WORKING_SET_NAMES + Integer.valueOf(i)));
        boolean _tripleNotEquals = (lruWorkingSetNames != null);
        if (_tripleNotEquals) {
          final Set<IWorkingSet> workingSets = new HashSet<IWorkingSet>(2);
          for (int j = 0; (j < lruWorkingSetNames.length); j++) {
            {
              IWorkbench _workbench = PlatformUI.getWorkbench();
              IWorkingSetManager _workingSetManager = _workbench.getWorkingSetManager();
              String _get = lruWorkingSetNames[j];
              final IWorkingSet workingSet = _workingSetManager.getWorkingSet(_get);
              boolean _tripleNotEquals_1 = (workingSet != null);
              if (_tripleNotEquals_1) {
                workingSets.add(workingSet);
              }
            }
          }
          boolean _isEmpty = workingSets.isEmpty();
          boolean _not = (!_isEmpty);
          if (_not) {
            int _size = workingSets.size();
            IWorkingSet[] _newArrayOfSize = new IWorkingSet[_size];
            IWorkingSet[] _array = workingSets.<IWorkingSet>toArray(_newArrayOfSize);
            SearchUtil.fgLRUWorkingSets.add(_array);
          }
        }
      }
    }
  }
  
  public static IWorkingSet[] queryWorkingSets() throws InterruptedException {
    IWorkbench _workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow activeWorkbenchWindow = _workbench.getActiveWorkbenchWindow();
    boolean _tripleEquals = (activeWorkbenchWindow == null);
    if (_tripleEquals) {
      return null;
    }
    final Shell shell = activeWorkbenchWindow.getShell();
    boolean _tripleEquals_1 = (shell == null);
    if (_tripleEquals_1) {
      return null;
    }
    IWorkbench _workbench_1 = PlatformUI.getWorkbench();
    IWorkingSetManager _workingSetManager = _workbench_1.getWorkingSetManager();
    final IWorkingSetSelectionDialog dialog = _workingSetManager.createWorkingSetSelectionDialog(shell, true);
    int _open = dialog.open();
    boolean _notEquals = (_open != Window.OK);
    if (_notEquals) {
      throw new InterruptedException();
    }
    final IWorkingSet[] workingSets = dialog.getSelection();
    int _length = workingSets.length;
    boolean _greaterThan = (_length > 0);
    if (_greaterThan) {
      return workingSets;
    }
    return null;
  }
  
  public static void updateLRUWorkingSets(final IWorkingSet[] workingSets) {
    boolean _or = false;
    boolean _tripleEquals = (workingSets == null);
    if (_tripleEquals) {
      _or = true;
    } else {
      int _length = workingSets.length;
      boolean _lessThan = (_length < 1);
      _or = _lessThan;
    }
    if (_or) {
      return;
    }
    LRUWorkingSetsList _lRUWorkingSets = SearchUtil.getLRUWorkingSets();
    _lRUWorkingSets.add(workingSets);
    IDialogSettings _dialogStoreSection = SearchUtil.getDialogStoreSection();
    SearchUtil.saveState(_dialogStoreSection);
  }
  
  public static void saveState(final IDialogSettings settingsStore) {
    int i = 0;
    Iterable<IWorkingSet[]> _get = SearchUtil.fgLRUWorkingSets.get();
    for (final IWorkingSet[] workingSets : _get) {
      {
        int _length = workingSets.length;
        final String[] names = new String[_length];
        for (int j = 0; (j < workingSets.length); j++) {
          IWorkingSet _get_1 = workingSets[j];
          String _name = _get_1.getName();
          names[j] = _name;
        }
        settingsStore.put((SearchUtil.STORE_LRU_WORKING_SET_NAMES + Integer.valueOf(i)), names);
        i++;
      }
    }
  }
  
  public static void addSearchResult(final List<ModuleLineFunctionArityRef> result, final OtpErlangObject r) throws OtpErlangRangeException {
    final OtpErlangTuple t = ((OtpErlangTuple) r);
    OtpErlangObject _elementAt = t.elementAt(1);
    final OtpErlangList l = ((OtpErlangList) _elementAt);
    for (final OtpErlangObject i : l) {
      {
        final OtpErlangTuple modLineT = ((OtpErlangTuple) i);
        OtpErlangObject _elementAt_1 = modLineT.elementAt(0);
        final String modName = Util.stringValue(_elementAt_1);
        final OtpErlangObject nameO = modLineT.elementAt(1);
        OtpErlangObject _elementAt_2 = modLineT.elementAt(2);
        final OtpErlangLong arityL = ((OtpErlangLong) _elementAt_2);
        final int arity = arityL.intValue();
        OtpErlangObject _elementAt_3 = modLineT.elementAt(3);
        final String clauseHead = Util.stringValue(_elementAt_3);
        OtpErlangObject _elementAt_4 = modLineT.elementAt(4);
        final OtpErlangAtom subClause = ((OtpErlangAtom) _elementAt_4);
        OtpErlangObject _elementAt_5 = modLineT.elementAt(5);
        final OtpErlangLong offsetL = ((OtpErlangLong) _elementAt_5);
        OtpErlangObject _elementAt_6 = modLineT.elementAt(6);
        final OtpErlangLong lengthL = ((OtpErlangLong) _elementAt_6);
        OtpErlangObject _elementAt_7 = modLineT.elementAt(7);
        final OtpErlangAtom isDef = ((OtpErlangAtom) _elementAt_7);
        String name = null;
        if ((nameO instanceof OtpErlangAtom)) {
          String _atomValue = ((OtpErlangAtom)nameO).atomValue();
          name = _atomValue;
        } else {
          String _stringValue = Util.stringValue(nameO);
          name = _stringValue;
        }
        int _intValue = offsetL.intValue();
        int _intValue_1 = lengthL.intValue();
        String _atomValue_1 = subClause.atomValue();
        boolean _parseBoolean = Boolean.parseBoolean(_atomValue_1);
        String _atomValue_2 = isDef.atomValue();
        boolean _parseBoolean_1 = Boolean.parseBoolean(_atomValue_2);
        ModuleLineFunctionArityRef _moduleLineFunctionArityRef = new ModuleLineFunctionArityRef(modName, _intValue, _intValue_1, name, arity, clauseHead, _parseBoolean, _parseBoolean_1);
        result.add(_moduleLineFunctionArityRef);
      }
    }
  }
  
  public static ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(final OpenResult res, final IErlModule module, final int offset, final LimitTo limitTo, final boolean matchAnyFunctionDefinition) throws ErlModelException {
    if (res instanceof ExternalCallOpenResult) {
      return _getSearchPatternFromOpenResultAndLimitTo((ExternalCallOpenResult)res, module, offset, limitTo, matchAnyFunctionDefinition);
    } else if (res instanceof FieldOpenResult) {
      return _getSearchPatternFromOpenResultAndLimitTo((FieldOpenResult)res, module, offset, limitTo, matchAnyFunctionDefinition);
    } else if (res instanceof IncludeOpenResult) {
      return _getSearchPatternFromOpenResultAndLimitTo((IncludeOpenResult)res, module, offset, limitTo, matchAnyFunctionDefinition);
    } else if (res instanceof LocalCallOpenResult) {
      return _getSearchPatternFromOpenResultAndLimitTo((LocalCallOpenResult)res, module, offset, limitTo, matchAnyFunctionDefinition);
    } else if (res instanceof MacroOpenResult) {
      return _getSearchPatternFromOpenResultAndLimitTo((MacroOpenResult)res, module, offset, limitTo, matchAnyFunctionDefinition);
    } else if (res instanceof RecordOpenResult) {
      return _getSearchPatternFromOpenResultAndLimitTo((RecordOpenResult)res, module, offset, limitTo, matchAnyFunctionDefinition);
    } else if (res instanceof VariableOpenResult) {
      return _getSearchPatternFromOpenResultAndLimitTo((VariableOpenResult)res, module, offset, limitTo, matchAnyFunctionDefinition);
    } else if (res == null) {
      return _getSearchPatternFromOpenResultAndLimitTo((Void)null, module, offset, limitTo, matchAnyFunctionDefinition);
    } else {
      throw new IllegalArgumentException("Unhandled parameter types: " +
        Arrays.<Object>asList(res, module, offset, limitTo, matchAnyFunctionDefinition).toString());
    }
  }
}
