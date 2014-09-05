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
import org.erlide.engine.util.ResourceUtil

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
        ],
        "{messages,Msgs}" -> [ Bindings b |
            val msgs = b.getList("Msgs")
            processMessages(msgs)
        ]
    ]

    def boolean processStep(String step, int num) {
        notifier.newStep(step, num)
        return true
    }

    def boolean processPhase(String phase) {
        notifier.newPhase(phase)
        if (phase == "clean") {
            MarkerUtils.deleteMarkers(project)
        }
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

    def boolean processMessages(Collection<OtpErlangObject> messages) {
        createMarkers(project, null, messages.cleanup, newArrayList())
        return true
    }

    def Iterable<OtpErlangObject> cleanup(Collection<OtpErlangObject> objects) {
        println("cleanup")
        objects.map [
            val b = ErlUtils.match("{project,N,Msg,error}", it)
            if (b === null) {
                return it
            }
            val n = b.getLong("N")
            val msg = b.getString("Msg")
            val HDR = "Failed to extract name from "
            if (!msg.startsWith(HDR)) {
                return it
            }
            val last = msg.substring(HDR.length)
            val ix = last.indexOf(": ")
            if (ix <= 0) {
                return it
            }
            val file = last.substring(0, ix)
            val res = ResourceUtil.findResourceByLocation(project, file.trim)
            val newFile = res?.projectRelativePath
            if (newFile === null) {
                return it
            }
            val newMessage = HDR + newFile + ": "+last.substring(ix + 1)
            ErlUtils.format("{project,~i,~s,error}", n, newMessage)
        ]
    }

    def private static void createMarkers(IProject project, String filePath, Iterable<OtpErlangObject> messages,
        Collection<OtpErlangObject> dependencies) {
        if (project === null) {
            return
        }
        val srcFile = if (filePath === null) project else project.findMember(filePath)
        if (srcFile != project) MarkerUtils.deleteMarkers(srcFile)
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
