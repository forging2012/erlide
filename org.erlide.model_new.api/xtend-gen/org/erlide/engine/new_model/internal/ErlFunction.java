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
  
  private final Iterable<IErlFunctionClause> clauses;
  
  private final IErlTypeSpec typeSpecification;
  
  private final IErlComment comment;
  
  public ErlFunction(final Handle parent, final String name, final int arity, final Iterable<IErlFunctionClause> clauses, final IErlTypeSpec typeSpecification, final IErlComment comment) {
    super(parent, name);
    this.arity = arity;
    this.clauses = clauses;
    this.typeSpecification = typeSpecification;
    this.comment = comment;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + this.arity;
    result = prime * result + ((this.clauses== null) ? 0 : this.clauses.hashCode());
    result = prime * result + ((this.typeSpecification== null) ? 0 : this.typeSpecification.hashCode());
    result = prime * result + ((this.comment== null) ? 0 : this.comment.hashCode());
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
    if (this.clauses == null) {
      if (other.clauses != null)
        return false;
    } else if (!this.clauses.equals(other.clauses))
      return false;
    if (this.typeSpecification == null) {
      if (other.typeSpecification != null)
        return false;
    } else if (!this.typeSpecification.equals(other.typeSpecification))
      return false;
    if (this.comment == null) {
      if (other.comment != null)
        return false;
    } else if (!this.comment.equals(other.comment))
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
  
  @Pure
  public Iterable<IErlFunctionClause> getClauses() {
    return this.clauses;
  }
  
  @Pure
  public IErlTypeSpec getTypeSpecification() {
    return this.typeSpecification;
  }
  
  @Pure
  public IErlComment getComment() {
    return this.comment;
  }
}
