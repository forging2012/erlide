/**
 *
 */
package org.erlide.engine.model.erlang;

/**
 * @author jakob
 *
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface IErlPreprocessorDef extends IErlMember {

    /**
     * @return the defined name of the macro or record
     */
    public String getDefinedName();

    /**
     * @return the macro or record body as string
     */
    public String getExtra();
}
