/**
 *
 */
package org.erlide.engine.model.root;


/**
 * @author jakob
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface IErlExternal extends IErlElement {

    boolean isOTP();

    boolean hasIncludes();
}
