package org.erlide.ui.internal.search

import com.ericsson.otp.erlang.OtpErlangAtom
import com.ericsson.otp.erlang.OtpErlangList
import com.ericsson.otp.erlang.OtpErlangLong
import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangRangeException
import com.ericsson.otp.erlang.OtpErlangTuple
import java.text.Collator
import java.util.Arrays
import java.util.Collection
import java.util.Comparator
import java.util.HashSet
import java.util.List
import java.util.Map
import java.util.Set
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.Platform
import org.eclipse.jface.dialogs.ErrorDialog
import org.eclipse.jface.dialogs.IDialogSettings
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.window.Window
import org.eclipse.search.ui.NewSearchUI
import org.eclipse.search.ui.text.Match
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.IWorkbenchWindow
import org.eclipse.ui.IWorkingSet
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.dialogs.IWorkingSetSelectionDialog
import org.eclipse.ui.progress.IProgressService
import org.erlide.core.services.search.SearchCoreUtil
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.ErlModelException
import org.erlide.engine.model.erlang.IErlFunctionClause
import org.erlide.engine.model.erlang.IErlModule
import org.erlide.engine.model.root.ErlElementKind
import org.erlide.engine.model.root.IErlElement
import org.erlide.engine.services.search.ErlSearchScope
import org.erlide.engine.services.search.ErlangSearchPattern
import org.erlide.engine.services.search.ExternalCallOpenResult
import org.erlide.engine.services.search.FieldOpenResult
import org.erlide.engine.services.search.FunctionPattern
import org.erlide.engine.services.search.IncludeOpenResult
import org.erlide.engine.services.search.IncludePattern
import org.erlide.engine.services.search.LimitTo
import org.erlide.engine.services.search.LocalCallOpenResult
import org.erlide.engine.services.search.MacroOpenResult
import org.erlide.engine.services.search.MacroPattern
import org.erlide.engine.services.search.ModuleLineFunctionArityRef
import org.erlide.engine.services.search.RecordFieldPattern
import org.erlide.engine.services.search.RecordOpenResult
import org.erlide.engine.services.search.RecordPattern
import org.erlide.engine.services.search.SearchFor
import org.erlide.engine.services.search.SearchPatternFactory
import org.erlide.engine.services.search.TypeRefPattern
import org.erlide.engine.services.search.VariableOpenResult
import org.erlide.engine.services.search.VariablePattern
import org.erlide.ui.actions.OpenUtils
import org.erlide.ui.internal.ErlideUIPlugin
import org.erlide.util.StringUtils
import org.erlide.util.Util
import org.osgi.framework.Bundle

class SearchUtil {

	public static int SEARCH_IN_SOURCES = 1
	public static int SEARCH_IN_EXTERNALS = 2
	public static int SEARCH_IN_OTP_LIBRARIES = 4

	private static int ARI_TYPESPEC = -2
	private static int ARI_ATTRIBUTE = -3
	private static int ARI_RECORD_DEF = -4
	private static int ARI_MACRO_DEF = -5
	private static int ARI_INCLUDE = -6
	private static int ARI_RECORD_FIELD_DEF = -7

	static class WorkingSetComparator implements Comparator<IWorkingSet> {
		private static Collator collator = Collator.getInstance()

		override int compare(IWorkingSet o1, IWorkingSet o2) {
			return collator.compare(o1.getLabel(), o2.getLabel())
		}
	}

	def static ErlSearchScope getSelectionScope(ISelection selection, boolean addExternals, boolean addOtp) throws CoreException {
		val ErlSearchScope result = new ErlSearchScope()
		val Set<String> externalModulePaths = new HashSet<String>()
		if (selection instanceof IStructuredSelection) {
			for (Object obj : selection.toList()) {
				switch obj {
					IResource: {
						SearchCoreUtil.addResourceToScope(result, obj)
					}
					IErlModule: {
						result.addModule(obj)
					}
					IErlElement: {
						SearchCoreUtil.addExternalModules(obj, result, externalModulePaths, addExternals, addOtp)
					}
				}
			}
		}
		return result
	}

	def static Match createMatch(ModuleLineFunctionArityRef ref, Map<String, IErlModule> pathToModuleMap) {
		val ErlangSearchElement ese = createSearchElementFromRef(ref, pathToModuleMap)
		return new Match(ese, ref.getOffset(), ref.getLength())
	}

	def static ErlangSearchElement createSearchElementFromRef(ModuleLineFunctionArityRef ref,
		Map<String, IErlModule> pathToModuleMap) {
		val IErlModule module = pathToModuleMap.get(ref.getModulePath())
		return createSearchElement(ref, module)
	}

	def static ErlangSearchElement createSearchElement(ModuleLineFunctionArityRef ref, IErlModule module) {
		return new ErlangSearchElement(module, ref.getModulePath(), ref.getName(), ref.getArity(), ref.getClauseHead(),
			ref.isSubClause(), refToKind(ref))
	}

	def static ErlElementKind refToKind(ModuleLineFunctionArityRef ref) {
		switch (ref.getArity()) {
			case ARI_TYPESPEC:
				return ErlElementKind.TYPESPEC
			case ARI_ATTRIBUTE:
				return ErlElementKind.ATTRIBUTE // Kind.MODULE ?
			case ARI_RECORD_DEF:
				return ErlElementKind.RECORD_DEF
			case ARI_MACRO_DEF:
				return ErlElementKind.MACRO_DEF
			case ARI_INCLUDE:
				return ErlElementKind.ATTRIBUTE
			// include actually, attributes are not saved (yet)
			case ARI_RECORD_FIELD_DEF:
				return ErlElementKind.RECORD_FIELD
			default:
				if (ref.isSubClause()) {
					return ErlElementKind.CLAUSE
				}
		}
		return ErlElementKind.FUNCTION
	}

	def static dispatch ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(ExternalCallOpenResult res,
		IErlModule module, int offset, LimitTo limitTo, boolean matchAnyFunctionDefinition) throws ErlModelException {
		var String moduleName = res.getMod()
		if (moduleName === null) {
			return null
		}
		val String unquoted = StringUtils.unquote(moduleName)
		if (module !== null && offset != -1) {
			val IErlElement e = module.getElementAt(offset)
			if (e !== null && (e.getKind() == ErlElementKind.TYPESPEC || e.getKind() == ErlElementKind.RECORD_DEF)) {
				return new TypeRefPattern(moduleName, res.getFun(), limitTo)
			}
		}
		var String oldName
		moduleName = unquoted
		do {
			oldName = moduleName
			moduleName = ErlangEngine.getInstance().getModelFindService().resolveMacroValue(moduleName, module)
		} while (!moduleName.equals(oldName))
		return new FunctionPattern(moduleName, res.getFun(), res.getArity(), limitTo, matchAnyFunctionDefinition,
			module, false)
	}

	def static dispatch ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(LocalCallOpenResult res,
		IErlModule module, int offset, LimitTo limitTo, boolean matchAnyFunctionDefinition) throws ErlModelException {
		var String moduleName = null
		if (module !== null) {
			moduleName = module.getModuleName()
			if (offset != -1) {
				val IErlElement e = module.getElementAt(offset)
				if (new OpenUtils().isTypeDefOrRecordDef(res, e)) {
					return new TypeRefPattern(moduleName, res.getFun(), limitTo)
				}
			}
		}
		return new FunctionPattern(moduleName, res.getFun(), res.getArity(), limitTo, matchAnyFunctionDefinition,
			module, true)
	}

	def static dispatch ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(MacroOpenResult res,
		IErlModule module, int offset, LimitTo limitTo, boolean matchAnyFunctionDefinition) throws ErlModelException {
		val String unquoted = StringUtils.unquote(res.getName())
		return new MacroPattern(unquoted, limitTo)
	}

	def static dispatch ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(RecordOpenResult res,
		IErlModule module, int offset, LimitTo limitTo, boolean matchAnyFunctionDefinition) throws ErlModelException {
		val String unquoted = StringUtils.unquote(res.getName())
		return new RecordPattern(unquoted, limitTo)
	}

	def static dispatch ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(IncludeOpenResult res,
		IErlModule module, int offset, LimitTo limitTo, boolean matchAnyFunctionDefinition) throws ErlModelException {
		return new IncludePattern(res.getName(), limitTo)
	}

	def static dispatch ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(VariableOpenResult res,
		IErlModule module, int offset, LimitTo limitTo, boolean matchAnyFunctionDefinition) throws ErlModelException {
		if (module !== null) {
			if (offset != -1) {
				val IErlElement e = module.getElementAt(offset)
				if (e instanceof IErlFunctionClause) {
					return new VariablePattern(e.getFunctionName(), e.getArity(), e.getHead(), res.getName(),
						limitTo, module)
				}
			}
		}
	}

	def static dispatch ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(FieldOpenResult res,
		IErlModule module, int offset, LimitTo limitTo, boolean matchAnyFunctionDefinition) throws ErlModelException {
		val String unquoted = StringUtils.unquote(res.getName())
		return new RecordFieldPattern(res.getRecord(), unquoted, limitTo)
	}

	def static dispatch ErlangSearchPattern getSearchPatternFromOpenResultAndLimitTo(Void res, IErlModule module,
		int offset, LimitTo limitTo, boolean matchAnyFunctionDefinition) throws ErlModelException {
		return null
	}

	def static ErlangSearchPattern getSearchPattern(IErlModule module, SearchFor searchFor, String pattern,
		LimitTo limitTo) {
		var String moduleName = ""
		var String name = pattern
		var int arity = -1
		var int p = pattern.indexOf(':')
		if (p != -1) {
			moduleName = pattern.substring(0, p)
			name = pattern.substring(p + 1)
		}
		p = name.indexOf('/')
		if (p != -1) {
			arity = Integer.valueOf(name.substring(p + 1))
			name = name.substring(0, p)
		}
		return new SearchPatternFactory(ErlangEngine.getInstance().getModelUtilService()).
			getSearchPattern(searchFor, moduleName, name, arity, limitTo, module)
	}

	def static void runQuery(ErlangSearchPattern pattern, ErlSearchScope scope, String scopeDescription, Shell shell) {
		val ErlSearchQuery query = new ErlSearchQuery(pattern, scope, scopeDescription)
		if (query.canRunInBackground()) {

			/*
             * This indirection with Object as parameter is needed to prevent
             * the loading of the Search plug-in: the VM verifies the method
             * call and hence loads the types used in the method signature,
             * eventually triggering the loading of a plug-in (in this case
             * ISearchQuery results in Search plug-in being loaded).
             */
			NewSearchUI.runQueryInBackground(query)
		} else {
			val IProgressService progressService = PlatformUI.getWorkbench().getProgressService()

			/*
             * This indirection with Object as parameter is needed to prevent
             * the loading of the Search plug-in: the VM verifies the method
             * call and hence loads the types used in the method signature,
             * eventually triggering the loading of a plug-in (in this case it
             * would be ISearchQuery).
             */
			val IStatus status = NewSearchUI.runQueryInForeground(progressService, query)
			if (status.matches(IStatus.ERROR + IStatus.INFO + IStatus.WARNING)) {
				ErrorDialog.openError(shell, "Search",
					"Problems occurred while searching. " + "The affected files will be skipped.", status)
			}
		}
	}

	def static String getWorkingSetsScopeDescription(IWorkingSet[] workingSets) {
		val String wssS = "working sets "
		val String wsS = "working set "
		if (workingSets.length == 0) {
			return ""
		}
		val String s = if(workingSets.length == 1) wsS else wssS
		return workingSetLabels(workingSets, s, "'")
	}

	def static String workingSetLabels(IWorkingSet[] workingSets, String s, String surround) {
		val StringBuilder sb = new StringBuilder(s)
		val cb = [IWorkingSet ws|sb.append(surround).append(ws.getLabel()).append(surround).append(", ")]
		if (workingSets.head !== null)
			cb.apply(workingSets.head)
		if (workingSets.tail.head !== null)
			cb.apply(workingSets.tail.head)
		if (workingSets.length > 2) {
			return sb.append("... ").toString()
		}
		return sb.substring(0, sb.length() - 2)
	}

	def static ErlSearchScope getWorkingSetsScope(IWorkingSet[] workingSets, boolean addExternals, boolean addOTP) throws CoreException {
		val ErlSearchScope result = new ErlSearchScope()
		val Set<String> externalModulePaths = new HashSet<String>()
		if (workingSets === null) {
			return result
		}
		for (IWorkingSet ws : workingSets) {
			val IAdaptable[] elements = ws.getElements()

			for (IAdaptable a : elements) {
				val IResource r = a.getAdapter(IResource) as IResource

				SearchCoreUtil.addResourceToScope(result, r)
				var IErlElement parent = null
				var Object o = a.getAdapter(IErlElement)
				if (o instanceof IErlElement) {
					parent = o
				} else {
					o = a.getAdapter(IResource)
					if (o !== null) {
						val IResource resource = o as IResource
						val IErlElement element = ErlangEngine.getInstance().getModel().findElement(resource)
						parent = element
					}
				}
				if (parent !== null) {
					SearchCoreUtil.addExternalModules(parent, result, externalModulePaths, addExternals, addOTP)
				}
			}
		}
		return result
	}

	def static String getProjectScopeDescription(Collection<IProject> projects) {
		if (projects === null || projects.isEmpty()) {
			return ""
		}
		val StringBuilder sb = new StringBuilder(if(projects.size() == 1) "project" else "projects")
		sb.append(' ')
		val cb = [IProject p|sb.append('\'').append(p.getName()).append("', ")]
		if (projects.head !== null)
			cb.apply(projects.head)
		if (projects.tail.head !== null)
			cb.apply(projects.tail.head)
		if (projects.size() > 2) {
			return sb.append("... ").toString()
		}
		return sb.substring(0, sb.length() - 2)
	}

	def static String getSelectionScopeDescription(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			val StringBuilder sb = new StringBuilder()
			val List<?> list = selection.toList()
			if (list.isEmpty()) {
				return ""
			}
			val cb = [ Object obj |
				var String name
				if (obj instanceof IResource) {
					name = obj.getName()
				} else if (obj instanceof IErlElement) {
					name = obj.getName()
				} else {
					name = "?"
				}
				sb.append('\'').append(name).append("', ")
			]
			if (list.head !== null)
				cb.apply(list.head)
			if (list.tail.head !== null)
				cb.apply(list.tail.head)
			if (selection.size() > 2) {
				return sb.append("...").toString()
			}
			return sb.substring(0, sb.length() - 2)
		}
		return ""
	}

	def static String getWorkspaceScopeDescription() {
		return "workspace"
	}

	def static String toString(IWorkingSet[] workingSets) {
		Arrays.sort(workingSets, new WorkingSetComparator())
		return workingSetLabels(workingSets, "", "")
	}

	// LRU working sets
	val static int LRU_WORKINGSET_LIST_SIZE = 3
	var static LRUWorkingSetsList fgLRUWorkingSets

	// Settings store
	private static String DIALOG_SETTINGS_KEY = "ErlangElementSearchActions"

	private static String STORE_LRU_WORKING_SET_NAMES = "lastUsedWorkingSetNames"

	def static boolean isSearchPlugInActivated() {
		return Platform.getBundle("org.eclipse.search").getState() == Bundle.ACTIVE
	}

	def static IDialogSettings getDialogStoreSection() {
		val IDialogSettings dialogSettings = ErlideUIPlugin.getDefault().getDialogSettings()
		var IDialogSettings settingsStore = dialogSettings.getSection(DIALOG_SETTINGS_KEY)
		if (settingsStore === null) {
			settingsStore = dialogSettings.addNewSection(DIALOG_SETTINGS_KEY)
		}
		return settingsStore
	}

	def static LRUWorkingSetsList getLRUWorkingSets() {
		if (fgLRUWorkingSets === null) {
			restoreState()
		}
		return fgLRUWorkingSets
	}

	def static void restoreState() {
		fgLRUWorkingSets = new LRUWorkingSetsList(LRU_WORKINGSET_LIST_SIZE)
		val IDialogSettings settingsStore = getDialogStoreSection()

		for (var i = LRU_WORKINGSET_LIST_SIZE - 1; i >= 0; i--) {

			val String[] lruWorkingSetNames = settingsStore.getArray(STORE_LRU_WORKING_SET_NAMES + i)
			if (lruWorkingSetNames !== null) {
				val Set<IWorkingSet> workingSets = new HashSet<IWorkingSet>(2)
				for (var j = 0; j < lruWorkingSetNames.length; j++) {
					val IWorkingSet workingSet = PlatformUI.getWorkbench().getWorkingSetManager().
						getWorkingSet(lruWorkingSetNames.get(j))
					if (workingSet !== null) {
						workingSets.add(workingSet)
					}
				}
				if (!workingSets.isEmpty()) {
					fgLRUWorkingSets.add(workingSets.toArray(newArrayOfSize(workingSets.size())))
				}
			}
		}
	}

	def static IWorkingSet[] queryWorkingSets() throws InterruptedException {
		val IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
		if (activeWorkbenchWindow === null) {
			return null
		}
		val Shell shell = activeWorkbenchWindow.getShell()
		if (shell === null) {
			return null
		}
		val IWorkingSetSelectionDialog dialog = PlatformUI.getWorkbench().getWorkingSetManager().
			createWorkingSetSelectionDialog(shell, true)
		if (dialog.open() != Window.OK) {
			throw new InterruptedException()
		}

		val IWorkingSet[] workingSets = dialog.getSelection()
		if (workingSets.length > 0) {
			return workingSets
		}
		return null // 'no working set' selected
	}

	def static void updateLRUWorkingSets(IWorkingSet[] workingSets) {
		if (workingSets === null || workingSets.length < 1) {
			return
		}
		getLRUWorkingSets().add(workingSets)
		saveState(getDialogStoreSection())
	}

	def static void saveState(IDialogSettings settingsStore) {
		var int i = 0
		for (workingSets : fgLRUWorkingSets.get()) {

			val String[] names = newArrayOfSize(workingSets.length)
			for (var int j = 0; j < workingSets.length; j++) {
				names.set(j, workingSets.get(j).getName())
			}
			settingsStore.put(STORE_LRU_WORKING_SET_NAMES + i, names)
			i++
		}
	}

	def static void addSearchResult(List<ModuleLineFunctionArityRef> result, OtpErlangObject r) throws OtpErlangRangeException {
		val OtpErlangTuple t = r as OtpErlangTuple
		val OtpErlangList l = t.elementAt(1) as OtpErlangList
		for (OtpErlangObject i : l) {

			/*
             * find_data([#ref{function=F, arity=A, clause=C, data=D, offset=O,
             * length=L, sub_clause=S} | Rest], Data, M, Acc) -> case D of Data
             * -> find_data(Rest, Data, M, [{M, F, A, C, S, O, L} | Acc]) _ ->
             * find_data(Rest, Data, M, Acc) end.
             */
			val OtpErlangTuple modLineT = i as OtpErlangTuple
			val String modName = Util.stringValue(modLineT.elementAt(0))
			val OtpErlangObject nameO = modLineT.elementAt(1)
			val OtpErlangLong arityL = modLineT.elementAt(2) as OtpErlangLong
			val int arity = arityL.intValue()
			val String clauseHead = Util.stringValue(modLineT.elementAt(3))
			val OtpErlangAtom subClause = modLineT.elementAt(4) as OtpErlangAtom
			val OtpErlangLong offsetL = modLineT.elementAt(5) as OtpErlangLong
			val OtpErlangLong lengthL = modLineT.elementAt(6) as OtpErlangLong
			val OtpErlangAtom isDef = modLineT.elementAt(7) as OtpErlangAtom
			var String name
			if (nameO instanceof OtpErlangAtom) {
				name = nameO.atomValue()
			} else {
				name = Util.stringValue(nameO)
			}
			result.add(
				new ModuleLineFunctionArityRef(modName, offsetL.intValue(), lengthL.intValue(), name, arity, clauseHead,
					Boolean.parseBoolean(subClause.atomValue()), Boolean.parseBoolean(isDef.atomValue())))
		}
	}

}
