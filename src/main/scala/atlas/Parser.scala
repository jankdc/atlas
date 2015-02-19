package atlas

import nodes.Node
import tokens.Token
import patterns.Pattern
import collection.mutable.Buffer

object Parser {

  type Result = (Seq[Node], Seq[Token])
  type Parsec = (Seq[Token]) => Result

  def parse(ts: Seq[Token]): Node = {
    val (Seq(top), remains) = parseTop(ts)
    assert(remains.isEmpty)
    return top
  }

  private
  def parseTop(ts: Seq[Token]): Result = {
    val desc = "a top level expression"
    val expr = any(desc, parseLet, parseFun, one(patterns.NewLine))
    val parser = seq(rep(expr, patterns.EOF), one(patterns.EOF))
    val (ns, rm) = parser(ts)
    (Seq(nodes.Top(ns)(ts.head.pos)), rm)
  }

  private
  def parseFun(ts: Seq[Token]): Result = {
    val fn = key("fn")
    val name = one(patterns.NameId)
    val newl = one(patterns.NewLine)
    val body = dlist(parseStmt)
    val params = plist(parseParam)
    val parser = seq(fn, name, params, key(":"), parseType, newl, body)
    val (Seq(nodes.NameId(nm), nodes.List(ps), tp, nodes.List(bd)), rs) = parser(ts)
    (Seq(nodes.Fun(nm, ps :+ tp, bd)(ts.head.pos)), rs)
  }

  private
  def parseApp(ts: Seq[Token]): Result = {
    val name = one(patterns.NameId)
    val args = plist(parseAtom)
    val parser = seq(name, args)
    val (Seq(nodes.NameId(nm), nodes.List(as)), rs) = parser(ts)
    (Seq(nodes.App(nm, as)(ts.head.pos)), rs)
  }

  private
  def parseStmt(ts: Seq[Token]): Result = {
    val atomStmt = seq(parseAtom, one(patterns.NewLine))
    val callStmt = seq(parseApp, one(patterns.NewLine))
    val passStmt = seq(key("pass"), one(patterns.NewLine))
    val parser = any("a statement",
      atomStmt,
      callStmt,
      passStmt,
      parseLet,
      parseMut,
      parseFun)

    parser(ts)
  }

  private
  def parseParam(ts: Seq[Token]): Result = {
    val parser = seq(one(patterns.NameId), key(":"), parseType)
    val (Seq(nodes.NameId(nm), tp), rs) = parser(ts)
    (Seq(nodes.Param(nm, tp)(ts.head.pos)), rs)
  }

  // TODO: Add more types!
  // e.g. tuples, list, maps, polymorphic
  private
  def parseType(ts: Seq[Token]): Result = {
    val simple = one(patterns.NameId)
    val others = rep(seq(key("->"), simple))
    val parser = seq(simple, others)
    val (types, rs) = parser(ts)
    types match {
      case Seq(one) => (types, rs)
      case morethan => (Seq(nodes.Lam(types)(ts.head.pos)), rs)
    }
  }

  private
  def parseMut(ts: Seq[Token]): Result = {
    val let     = key("let")
    val mut     = key("mut")
    val name    = one(patterns.NameId)
    val assign  = key("=")
    val newline = one(patterns.NewLine)
    val parser  = seq(let, mut, name, assign, parseAtom, newline)
    val (Seq(nodes.NameId(nm), rv), rm) = parser(ts)
    (Seq(nodes.Mut(nm, rv)(ts.head.pos)), rm)
  }

  private
  def parseLet(ts: Seq[Token]): Result = {
    val let     = key("let")
    val name    = one(patterns.NameId)
    val assign  = key("=")
    val newline = one(patterns.NewLine)
    val parser  = seq(let, name, assign, parseAtom, newline)
    val (Seq(nodes.NameId(nm), rv), rm) = parser(ts)
    (Seq(nodes.Let(nm, rv)(ts.head.pos)), rm)
  }

  private
  def parseAtom(ts: Seq[Token]): Result = {
    val parens = seq(key("("), parseAtom, key(")"))
    val anymsg = "an atomic expression"
    val integr = one(patterns.Integer)
    val nameid = one(patterns.NameId)
    val parser = any(anymsg, integr, parens, nameid, parseApp)
    parser(ts)
  }

  private
  def plist(item: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val parenL = key("(")
      val parenR = key(")")
      val others = rep(any("a parameter", seq(key(","), item), item), ")")
      val parser = seq(parenL, eat(item, others), parenR)
      val (ns, rm) = parser(ts)
      (Seq(nodes.List(ns)(ts.head.pos)), rm)
    }

  private
  def dlist(item: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val indent = one(patterns.Indent)
      val dedent = one(patterns.Dedent)
      val others = rep(item, patterns.Dedent)
      val parser = seq(indent, others, dedent)
      val (ns, rm) = parser(ts)
      (Seq(nodes.List(ns)(ts.head.pos)), rm)
    }

  private
  def rep(p: Parsec, until: Pattern): Parsec =
    (ts: Seq[Token]) => {
      var buffer = Buffer[Node]()
      var remain = ts

      def reached = remain.headOption match {
        case Some(t) => (t.productPrefix == until.productPrefix)
        case None => false
      }

      while (! reached) {
        val (nodes, rm) = p(remain)
        buffer ++= nodes
        remain = rm
      }

      (buffer.toSeq, remain)
    }

  private
  def rep(p: Parsec, until: String): Parsec =
    (ts: Seq[Token]) => {
      var buffer = Buffer[Node]()
      var remain = ts

      def reached = remain.headOption match {
        case Some(t) => (t.raw == until)
        case None => false
      }

      while (! reached) {
        val (nodes, rm) = p(remain)
        buffer ++= nodes
        remain = rm
      }

      (buffer.toSeq, remain)
    }

  private
  def rep(p: Parsec): Parsec =
    (ts: Seq[Token]) => {
      var buffer = Buffer[Node]()
      var remain = ts

      try {
        while (true) {
          val (nodes, rm) = p(remain)
          buffer ++= nodes
          remain = rm
        }
      }
      catch {
        case err: ParserError if remain.length == err.count =>
          // Do nothing
      }

      (buffer.toSeq, remain)
    }

  private
  def any(msg: String, ps: Parsec*): Parsec =
    (ts: Seq[Token]) => {
      var currentParsed = Option[Result](null)
      var furthest = ParserError(ts.length, "")

      ps.takeWhile(_ => currentParsed == None).foreach {
        parsec =>

        try {
          val parsed@(nodes, next) = parsec(ts)
          val (_, prev) = currentParsed getOrElse (Seq(), ts)

          if (next.length < prev.length)
            { currentParsed = Some(parsed) }
        }
        catch {
          case err: ParserError if err.count < furthest.count =>
            furthest = err
          case err: ParserError =>
            // Do nothing...
        }
      }

      // Check if any of the parsers has succeeded.
      // If it didn't, throw the appropriate message
      // using the longest parsed error as the highest
      // priority.
      (currentParsed, furthest.count == ts.length) match {
        case (Some(nodes), _) =>
          nodes
        case (None, false) =>
          throw furthest
        case (None, true) =>
          throw ts.report(msg)
      }
    }

  private
  def seq(ps: Parsec*): Parsec =
    (ts: Seq[Token]) => {
      ps.foldLeft((Seq[Node](), ts)) {
        case ((nodes, remains), parsec) =>
          val (n, newRemains) = parsec(remains)
          (nodes ++ n, newRemains)
      }
    }

  private
  def eat(ps: Parsec*): Parsec =
    (ts: Seq[Token]) => {
      var buffer = Buffer[Node]()
      var remain = ts

      for (p <- ps)
        try {
          val (nodes, rm) = p(remain)
          buffer ++= nodes
          remain = rm
        }
        catch {
          case err: ParserError
            if err.count == remain.length =>
              // Do nothing.
          case err: ParserError =>
            println(s"WENT HERE ${err.count} ${remain.length}")
            throw err
        }

      (buffer.toSeq, remain)
    }

  private
  def one(p: Pattern): Parsec =
    (ts: Seq[Token]) => ts match {
      case h +: rest if h.productPrefix == p.productPrefix =>
        (pin(h), rest)
      case others =>
        throw others.report(p.toString)
    }

  private
  def key(s: String): Parsec =
    (ts: Seq[Token]) => ts match {
      case tokens.Reserved("pass") +: rest if "pass" == s =>
        (Seq(nodes.Nop()(ts.head.pos)), rest)
      case tokens.Reserved(n) +: rest if n == s =>
        (Seq(), rest)
      case others =>
        throw others.report(s)
    }

  private
  def pin(token: Token): Seq[Node] = {
    implicit val pos = token.pos
    token match {
      case tokens.NameId(n) => Seq(nodes.NameId(n))
      case tokens.Integer(n) => Seq(nodes.Integer(n.toInt))
      case others => Seq()
    }
  }

  private implicit
  class NodeOps(val ts: Seq[Token]) extends AnyVal {
    def report(s: String): ParserError = {
      ts.headOption match {
        case Some(t) =>
          val row = t.pos.row
          val column = t.pos.column
          val prefix = t.productPrefix
          val errMsg = s"[$row,$column]: Expected $s but got $prefix."
          ParserError(ts.length, errMsg)
        case None =>
          val errMsg = s"Expected $s, but reached end of file."
          ParserError(ts.length, errMsg)
      }
    }
  }
}
