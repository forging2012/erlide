package org.erlide.engine.new_model.internal

import org.eclipse.handly.model.impl.SourceConstruct
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.new_model.IErlAttribute
import org.erlide.engine.new_model.IErlComment
import org.erlide.engine.new_model.IErlExpression
import org.erlide.engine.new_model.IErlForm
import org.erlide.engine.new_model.IErlFunction
import org.erlide.engine.new_model.IErlFunctionClause
import org.erlide.engine.new_model.IErlTypeSpec

@Data
abstract class ErlForm extends SourceConstruct implements IErlForm {

    override protected getHandleManager() {
        ErlModelManager.INSTANCE.getHandleManager()
    }

}

@Data
class ErlAttribute extends ErlForm implements IErlAttribute {

    Iterable<IErlExpression> values

}

@Data
class ErlFunction extends ErlForm implements IErlFunction {

    int arity
    Iterable<IErlFunctionClause> clauses
    IErlTypeSpec typeSpecification
    IErlComment comment

}
