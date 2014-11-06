package org.erlide.engine.new_model

import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IWorkspace
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.ISourceFile
import org.erlide.engine.model.root.ErlangLibraryProperties
import org.erlide.engine.model.root.ErlangProjectProperties

interface IErlElement extends IHandle {
}

interface IErlModel extends IErlElement {
	def IWorkspace getWorkspace()

	def Iterable<IErlProject> getProjects()

	def IErlProject getProject(String name)

}

interface IErlLibrary extends IErlElement, IErlLibraryContainer {
	def ErlangLibraryProperties getProperties()

	def Iterable<IErlFolder> getFolders()

	def Iterable<IErlFolder> getSourceFolders()

	def Iterable<IErlFolder> getIncludeFolders()

	def IErlFolder getBinaryFolder()

	def Iterable<IResource> getNonErlangResources()
}

interface IErlProject extends IErlLibrary, IErlLibraryContainer {
	val static String NATURE_ID = "org.erlide.core.erlnature"

	def IProject getWorkspaceProject()

	def ErlangProjectProperties getProjectProperties()

	def Iterable<IErlSource> getSourceFiles()

	def IErlSource getSourceFile(String name)

	def Iterable<IErlModule> getModules()

	def Iterable<IErlHeader> getHeaders()

	def IErlOtpLibrary getOtpLibrary()
}

interface IErlLibraryContainer extends IErlElement {
	def Iterable<IErlLibrary> getLibraries()
}

interface IErlOtpLibrary extends IErlLibrary {
	def String getVersion()
}

interface IErlFolder extends IErlElement {
	def IFolder getWorkspaceFolder()

	def Iterable<IErlSource> getSources()
}

interface IErlSource extends IErlElement, ISourceFile {
	def Iterable<IErlForm> getForms()

	def String getExtension()

	def IErlComment getHeaderComment()

	def Iterable<IErlAttribute> getAttributes()

	def Iterable<IErlAttribute> getAttributesWithTag(String tag)

	def Iterable<IErlFunction> getFunctions()

	def IErlFunction getFunction(String name, int arity)

	def Iterable<IErlError> getErrors()
}

public interface IErlModule extends IErlSource {
}

public interface IErlHeader extends IErlSource {
}
