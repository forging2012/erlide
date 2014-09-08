package org.erlide.engine.services.search;

import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.services.search.OpenResult;

@Data
@SuppressWarnings("all")
public class FieldOpenResult extends OpenResult {
  private final String record;
  
  private final String name;
  
  public FieldOpenResult(final String record, final String name) {
    super();
    this.record = record;
    this.name = name;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.record== null) ? 0 : this.record.hashCode());
    result = prime * result + ((this.name== null) ? 0 : this.name.hashCode());
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
    FieldOpenResult other = (FieldOpenResult) obj;
    if (this.record == null) {
      if (other.record != null)
        return false;
    } else if (!this.record.equals(other.record))
      return false;
    if (this.name == null) {
      if (other.name != null)
        return false;
    } else if (!this.name.equals(other.name))
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
  public String getRecord() {
    return this.record;
  }
  
  @Pure
  public String getName() {
    return this.name;
  }
}
