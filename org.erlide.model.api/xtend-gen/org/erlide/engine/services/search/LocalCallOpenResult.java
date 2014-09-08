package org.erlide.engine.services.search;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.services.search.OpenResult;

@Data
@SuppressWarnings("all")
public class LocalCallOpenResult extends OpenResult {
  private final String fun;
  
  private final int arity;
  
  public LocalCallOpenResult(final String fun, final int arity) {
    super();
    this.fun = fun;
    this.arity = arity;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.fun== null) ? 0 : this.fun.hashCode());
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
    LocalCallOpenResult other = (LocalCallOpenResult) obj;
    if (this.fun == null) {
      if (other.fun != null)
        return false;
    } else if (!this.fun.equals(other.fun))
      return false;
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
  public String getFun() {
    return this.fun;
  }
  
  @Pure
  public int getArity() {
    return this.arity;
  }
}
