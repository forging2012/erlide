package org.erlide.engine.new_model.internal

import com.ericsson.otp.erlang.OtpErlangAtom
import com.ericsson.otp.erlang.OtpErlangList
import com.ericsson.otp.erlang.OtpErlangLong
import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangTuple
import java.util.List
import org.eclipse.handly.util.TextRange
import org.eclipse.xtend.lib.annotations.Accessors
import org.erlide.util.erlang.ErlUtils

@Accessors
class ErlangAST {

	List<ASTNode> forms
	List<ASTNode> comments

	new(OtpErlangTuple term) {
		val forms = (term.elements.tail.head as OtpErlangList).elements
		this.forms = forms.map[parse(it as OtpErlangTuple)]
		val comments = (term.elements.tail.tail.head as OtpErlangList).elements
		this.comments = comments.map[parse(it as OtpErlangTuple)]
	}

	def ASTNode parse(OtpErlangTuple object) {
		println('''!! «object»''')
		new ASTNode(object)
	}

}

@Accessors
class ASTNode {
	List<OtpErlangObject> items

	new(OtpErlangTuple source) {
		items = source.elements
	}

	def String getKind() {
		(items.head as OtpErlangAtom).atomValue
	}

	def TextRange getPos() {
		val pos = ErlUtils.match("{{Line,Offset,FileOffset},Len}", items.tail.head)
		new TextRange(pos.getInt("FileOffset"), pos.getInt("Len"))
	}

	def String getName() {
		(items.tail.tail.head as OtpErlangAtom).atomValue
	}

	def int getArg() {
		(items.tail.tail.tail.head as OtpErlangLong).intValue
	}
}
