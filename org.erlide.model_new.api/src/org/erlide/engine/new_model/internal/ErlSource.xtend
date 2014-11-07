package org.erlide.engine.new_model.internal

import java.io.IOException
import java.util.Map
import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.SourceElementBody
import org.eclipse.handly.model.impl.SourceFile
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.ErlangEngine
import org.erlide.engine.NewModelActivator
import org.erlide.engine.new_model.IErlAttribute
import org.erlide.engine.new_model.IErlComment
import org.erlide.engine.new_model.IErlError
import org.erlide.engine.new_model.IErlForm
import org.erlide.engine.new_model.IErlFunction
import org.erlide.engine.new_model.IErlHeader
import org.erlide.engine.new_model.IErlModule
import org.erlide.engine.new_model.IErlSource
import org.erlide.util.Util
import com.ericsson.otp.erlang.OtpErlangTuple

@Data
abstract class ErlSource extends SourceFile implements IErlSource {

    new(ErlProject parent, IFile file) {
        super(parent, file)
    }

    override protected buildStructure(SourceElementBody body, Map<IHandle, Body> newElements, Object ast, String source) {
        val ErlFileStructureBuilder builder = new ErlFileStructureBuilder(newElements, ast as ErlangAST);
        builder.buildStructure(this, body);
    }

    override protected createStructuralAst(String source) throws CoreException {
        try {
            return parse(source, getFile().getCharset())
        } catch (IOException e) {
            throw new CoreException(NewModelActivator.createErrorStatus(e.getMessage(), e))
        }
    }

    def private ErlangAST parse(String contents, String encoding) {
        val parser = ErlangEngine.instance.parserService
        val result = parser.parse(createScannerName, contents)
        if (Util.isOk(result)) {
            val tuple = result as OtpErlangTuple
            new ErlangAST(tuple.elementAt(1) as OtpErlangTuple)
        } else {
            null // TODO fix me
        }
    }

    override protected getHandleManager() {
        ErlModelManager.INSTANCE.getHandleManager()
    }

    override getForms() {
        getChildren(IErlForm)
    }

    override getHeaderComment() {
        val first = children.head
        if (first instanceof IErlComment)
            first
        else
            null
    }

    override getAttributes() {
        getChildren(IErlAttribute)
    }

    override getAttributesWithTag(String tag) {
        getAttributes.filter [
            it.name == tag
        ]
    }

    override getFunctions() {
        getChildren(IErlFunction)
    }

    override getFunction(
        String name,
        int arity
    ) {
        new ErlFunction(this, name, arity)
    }

    override getErrors() {
        getChildren(IErlError)
    }

    def private String createScannerName() {
        if (resource !== null) {
            return resource.fullPath.toPortableString().substring(1)
        }
        return "dummy"
    }
}

@Data
class ErlModule extends ErlSource implements IErlModule {

    new(ErlProject parent, IFile file) {
        super(parent, file)
        if (!file.getParent().equals(parent.resource))
            throw new IllegalArgumentException()
        if (!extension.equals(file.getFileExtension()))
            throw new IllegalArgumentException()
    }

    override getExtension() {
        "erl"
    }

}

@Data
class ErlHeader extends ErlSource implements IErlHeader {

    new(ErlProject parent, IFile file) {
        super(parent, file)
        if (!file.getParent().equals(parent.resource))
            throw new IllegalArgumentException()
        if (!extension.equals(file.getFileExtension()))
            throw new IllegalArgumentException()
    }

    override getExtension() {
        "hrl"
    }
}
