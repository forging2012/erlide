package org.erlide.engine.new_model.internal;

import org.eclipse.handly.model.impl.Handle;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.new_model.IErlComment;
import org.erlide.engine.new_model.IErlFunction;
import org.erlide.engine.new_model.IErlFunctionClause;
import org.erlide.engine.new_model.IErlTypeSpec;
import org.erlide.engine.new_model.internal.ErlForm;

@Data
@SuppressWarnings("all")
public class ErlFunction extends ErlForm implements IErlFunction {
  private final int arity;
  
  public Iterable<IErlFunctionClause> getClauses() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public IErlTypeSpec getTypeSpecification() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public IErlComment getComment() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public ErlFunction(final Handle parent, final String name, final int arity) {
    super(parent, name);
    this.arity = arity;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + this.arity;
    return result;
  }
  
  @Override
  @Pure
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    if (!super.equals(obj))
      return false;
    ErlFunction other = (ErlFunction) obj;
    if (other.arity != this.arity)
      return false;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    String result = new ToStringBuilder(this)
    	.addAllFields()
    	.toString();
    return result;
  }
  
  @Pure
  public int getArity() {
    return this.arity;
  }
}
