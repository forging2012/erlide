package org.erlide.engine.services.search;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.services.search.OpenResult;

@Data
@SuppressWarnings("all")
public class ExternalCallOpenResult extends OpenResult {
  private final String mod;
  
  private final String fun;
  
  private final int arity;
  
  private final String path;
  
  public ExternalCallOpenResult(final String mod, final String fun, final int arity, final String path) {
    super();
    this.mod = mod;
    this.fun = fun;
    this.arity = arity;
    this.path = path;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.mod== null) ? 0 : this.mod.hashCode());
    result = prime * result + ((this.fun== null) ? 0 : this.fun.hashCode());
    result = prime * result + this.arity;
    result = prime * result + ((this.path== null) ? 0 : this.path.hashCode());
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
    ExternalCallOpenResult other = (ExternalCallOpenResult) obj;
    if (this.mod == null) {
      if (other.mod != null)
        return false;
    } else if (!this.mod.equals(other.mod))
      return false;
    if (this.fun == null) {
      if (other.fun != null)
        return false;
    } else if (!this.fun.equals(other.fun))
      return false;
    if (other.arity != this.arity)
      return false;
    if (this.path == null) {
      if (other.path != null)
        return false;
    } else if (!this.path.equals(other.path))
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
  public String getMod() {
    return this.mod;
  }
  
  @Pure
  public String getFun() {
    return this.fun;
  }
  
  @Pure
  public int getArity() {
    return this.arity;
  }
  
  @Pure
  public String getPath() {
    return this.path;
  }
}
