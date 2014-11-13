package org.erlide.engine.internal.model.root

import com.ericsson.otp.erlang.OtpErlangString
import java.util.List
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.ProjectConfigurationSerializer
import org.erlide.util.erlang.Bindings
import org.erlide.util.erlang.OtpErlang

class RebarConfigurationSerializer implements ProjectConfigurationSerializer {

    override encodeConfig(ErlangProjectProperties info) {

        // TODO do nothing at the moment, will be implemented in step 2
        null

    // we need to keep the original parsed config and only replace changed parts!
    //        '''
    //            %% coding: utf-8
    //            {require_min_otp_vsn, "«info.runtimeVersion»"}.
    //            {erl_opts,
    //                [
    //                 debug_info,
    //                 «FOR inc: info.includeDirs SEPARATOR ','»{i, "«inc»"},«ENDFOR»
    //                 {src_dirs, [«FOR src: info.sourceDirs SEPARATOR ','»«src.toPortableString»«ENDFOR»]}
    //                ]}.
    //
    //        '''
    }

    override decodeConfig(String config) {
        val result = new ErlangProjectProperties()
        result.setOutputDir(new Path("ebin"))

        val content = ErlangEngine.instance.simpleParserService.parse(config)
        if (content.empty) {
            result.copyFrom(ErlangProjectProperties.DEFAULT)
            return result
        }

        content.forEach [ erl_opts |
            val bindings = OtpErlang.match("{erl_opts,Opts}", erl_opts)
            if (bindings !== null) {
                val opts = bindings.getList("Opts")
                if (opts !== null)
                    opts.forEach [ opt |
                        val b = OtpErlang.match("{Tag,Arg}", opt)
                        if (b !== null)
                            result.parseOption(b)
                    ]
            }
        ]

        if (result.sourceDirs.empty && result.includeDirs.empty) {
            result.copyFrom(ErlangProjectProperties.DEFAULT)
        }
        result
    }

    def void parseOption(ErlangProjectProperties result, Bindings b) {
        switch b.getAtom("Tag") {
            case "i": {
                val List<IPath> incs = newArrayList(result.getIncludeDirs)
                val inc = new Path(b.getString("Arg"))
                if (!incs.contains(inc))
                    incs.add(inc)
                result.setIncludeDirs(incs)
            }
            case "src_dirs": {
                result.setSourceDirs(
                    b.getList("Arg").map [
                        val s = (it as OtpErlangString).stringValue
                        new Path(s)
                    ])
            }
        }
    }
}
