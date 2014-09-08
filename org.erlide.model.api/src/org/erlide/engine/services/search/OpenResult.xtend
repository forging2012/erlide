package org.erlide.engine.services.search

import com.ericsson.otp.erlang.OtpErlangAtom
import com.ericsson.otp.erlang.OtpErlangLong
import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangString
import com.ericsson.otp.erlang.OtpErlangTuple
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.util.ErlLogger
import org.erlide.util.Util

abstract class OpenResult {

	def static OpenResult build(OtpErlangObject res) {
		if (!(res instanceof OtpErlangTuple)) {
			return null // not a call, ignore
		}
		val OtpErlangTuple openTuple = res as OtpErlangTuple
		val OtpErlangObject tag = openTuple.elementAt(0)
		if (!(tag instanceof OtpErlangAtom)) {
			return null
		}
		val String kind = (tag as OtpErlangAtom).atomValue()
		try {
			switch kind {
				case "external": {
					return new ExternalCallOpenResult(((openTuple.elementAt(1)) as OtpErlangAtom).atomValue(),
						(openTuple.elementAt(2) as OtpErlangAtom).atomValue(),
						(openTuple.elementAt(3) as OtpErlangLong).intValue(),
						if (openTuple.arity() > 4 && openTuple.elementAt(4) instanceof OtpErlangString) {
							(openTuple.elementAt(4) as OtpErlangString).stringValue()
						} else
							null)
				}
				case "local": {
					return new LocalCallOpenResult((openTuple.elementAt(1) as OtpErlangAtom).atomValue(),
						(openTuple.elementAt(2) as OtpErlangLong).intValue())
				}
				case "include": {
					return new IncludeOpenResult(Util.stringValue(openTuple.elementAt(1)), null)
				}
				case "include_lib": {
					return new IncludeOpenResult(Util.stringValue(openTuple.elementAt(1)),
						Util.stringValue(openTuple.elementAt(2)))
				}
				case "record": {
					return new RecordOpenResult((openTuple.elementAt(1) as OtpErlangAtom).toString())
				}
				case "macro": {
					val OtpErlangAtom element = openTuple.elementAt(1) as OtpErlangAtom
					return new MacroOpenResult(removeQuestionMark(element.atomValue))
				}
				case "variable": {
					val OtpErlangObject o = openTuple.elementAt(1)
					return new VariableOpenResult(
						if (o instanceof OtpErlangTuple) {

							// TODO when can this happen?
							val OtpErlangAtom a = o.elementAt(0) as OtpErlangAtom
							a.atomValue()
						} else if (o instanceof OtpErlangAtom) {
							o.atomValue()
						})
				}
				case "field": {
					return new FieldOpenResult((openTuple.elementAt(1) as OtpErlangAtom).atomValue(),
						(openTuple.elementAt(2) as OtpErlangAtom).atomValue())
				}
				default:
					return null
			}
		} catch (Exception e) {
			ErlLogger.warn(e)
			return null
		}
	}

	def private static String removeQuestionMark(String name) {
		val int i = name.indexOf('?')
		if (i == 0 || i == 1) {
			return name.substring(0, i) + name.substring(i + 1)
		}
		return name
	}

}

@Data
class FieldOpenResult extends OpenResult {
	String record
	String name
}

@Data
class VariableOpenResult extends OpenResult {
	String name
}

@Data
class MacroOpenResult extends OpenResult {
	String name
}

@Data
class RecordOpenResult extends OpenResult {
	String name
}

@Data
class IncludeOpenResult extends OpenResult {
	String name
	String path
}

@Data
class LocalCallOpenResult extends OpenResult {
	String fun
	int arity
}

@Data
class ExternalCallOpenResult extends OpenResult {
	String mod
	String fun
	int arity
	String path
}
