package org.erlide.engine.new_model.internal;

import org.eclipse.handly.model.impl.Handle;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.new_model.IErlError;
import org.erlide.engine.new_model.internal.ErlForm;

@Data
@SuppressWarnings("all")
public class ErlError extends ErlForm implements IErlError {
  private final String message;
  
  public ErlError(final Handle parent, final String name, final String message) {
    super(parent, name);
    this.message = message;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.message== null) ? 0 : this.message.hashCode());
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
    ErlError other = (ErlError) obj;
    if (this.message == null) {
      if (other.message != null)
        return false;
    } else if (!this.message.equals(other.message))
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
  public String getMessage() {
    return this.message;
  }
}
