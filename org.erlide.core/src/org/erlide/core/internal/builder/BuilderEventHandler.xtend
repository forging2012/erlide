package org.erlide.core.internal.builder

import com.ericsson.otp.erlang.OtpErlangObject
import com.google.common.eventbus.Subscribe
import java.util.Collection
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.Path
import org.erlide.core.builder.BuilderHelper
import org.erlide.engine.model.builder.MarkerUtils
import org.erlide.runtime.events.ErlEvent
import org.erlide.runtime.events.ErlangEventHandler
import org.erlide.util.erlang.Bindings
import org.erlide.util.erlang.ErlUtils
import org.erlide.util.ErlLogger
import com.google.common.base.Objects
import com.ericsson.otp.erlang.OtpErlangString

public class BuilderEventHandler extends ErlangEventHandler {

    val BuildNotifier notifier
    val BuilderState state
    val IProject project

    new(String backendName, BuildNotifier notifier, IProject project, BuilderState state) {
        super("builder", backendName)
        this.project = project
        this.notifier = notifier
        this.state = state
    }

    @Subscribe
    def void handleEvent(ErlEvent event) {
        if (!event.getTopic().equals(getTopic())) {
            return
        }
        val data = event.getEvent()

        //println('''>> «data»''')
        val processed = process(data)
        if (!processed) {
            ErlLogger.warn('''??? unhandled build«data»''')
        }
    }

    def boolean process(OtpErlangObject data) {
        val r = handlers.dropWhile [ pair |
            !match(pair.key, data, pair.value)
        ]
        !r.empty
    }

    def static boolean match(String template, OtpErlangObject data, (Bindings)=>boolean callback) {
        try {
            val b = ErlUtils.match(template, data)
            if (b === null) {
                return false
            }
            callback.apply(b)
        } catch (Exception e) {
            ErlLogger.error(e)
            return false
        }
    }

    val handlers = #[
        "{phase,Tag}" -> [ Bindings b |
            val tag = b.getAtom("Tag")
            println('''-- phase «tag»''')
            processPhase(tag)
        ],
        "{step,Tag, Num}" -> [ Bindings b |
            val tag = b.getString("Tag")
            val num = b.getInt("Num")
            println('''-- step «tag» «num»''')
            processStep(tag, num)
        ],
        "{compiled,File,Msgs,Deps}" -> [ Bindings b |
            val file = b.getString("File")
            val msgs = b.getList("Msgs")
            val deps = b.getList("Deps")
            processCompiled(file, msgs, deps)
        ],
        "{skipped,File}" -> [ Bindings b |
            val file = b.getString("File")
            processSkipped(file)
        ]
    ]

    def boolean processStep(String step, int num) {
        notifier.newStep(step, num)
        return true
    }

    def boolean processPhase(String phase) {
        notifier.newPhase(phase)
        return true
    }

    def boolean processCompiled(String file, Collection<OtpErlangObject> messages,
        Collection<OtpErlangObject> dependencies) {
        createMarkers(project, file, messages, dependencies)
        reloadBeam(project, file)
        notifier.compiled(file)
        return true
    }

    def boolean processSkipped(String file) {
        notifier.compiled(file)
        return true
    }

    def private static void createMarkers(IProject project, String filePath,
        Collection<OtpErlangObject> messages, Collection<OtpErlangObject> dependencies) {
        if (project === null) {
            return
        }
        val srcFile = project.findMember(filePath)
        MarkerUtils.deleteMarkers(srcFile)
        dependencies.forEach [ dep |
            val depstr = (dep as OtpErlangString).stringValue
            val file = project.findMember(depstr)
            MarkerUtils.deleteMarkers(file)
        ]
        MarkerUtils.addErrorMarkers(srcFile, messages)
    }

    def private static void reloadBeam(IProject project, String filePath) {
        if (project === null) {
            return
        }
        val path = new Path(filePath)
        val ext = path.getFileExtension()
        if (Objects.equal(ext, "erl")) {
            val module = path.removeFileExtension().lastSegment()
            BuilderHelper.loadModule(project, module)
        }
    }

}
