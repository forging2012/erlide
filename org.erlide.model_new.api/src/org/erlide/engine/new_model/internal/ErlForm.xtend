package org.erlide.engine.new_model.internal

import org.eclipse.handly.model.impl.SourceConstruct
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.new_model.IErlAttribute
import org.erlide.engine.new_model.IErlError
import org.erlide.engine.new_model.IErlForm
import org.erlide.engine.new_model.IErlFunction
import org.erlide.engine.new_model.IErlModule

@Data
abstract class ErlForm extends SourceConstruct implements IErlForm {

  override protected getHandleManager() {
    ErlModelManager.INSTANCE.getHandleManager()
  }

  override getModule() {
    parent as IErlModule
  }

}

@Data
class ErlAttribute extends ErlForm implements IErlAttribute {

  override getValues() {
    throw new UnsupportedOperationException("auto-generated method stub")
    // TODO: auto-generated method stub
  }

}

@Data
class ErlFunction extends ErlForm implements IErlFunction {
  int arity

  override getClauses() {
    throw new UnsupportedOperationException("auto-generated method stub")
    // TODO: auto-generated method stub
  }

  override getTypeSpecification() {
    throw new UnsupportedOperationException("auto-generated method stub")
    // TODO: auto-generated method stub
  }

  override getComment() {
    throw new UnsupportedOperationException("auto-generated method stub")
    // TODO: auto-generated method stub
  }

}

@Data
class ErlError extends ErlForm implements IErlError {

  override getMessage() {
    name
  }

}
