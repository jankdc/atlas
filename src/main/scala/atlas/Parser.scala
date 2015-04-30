package atlas

import atlas.ast.Node
import atlas.tokens.Token
import scala.collection.mutable

object Parser {
  type Result = (Seq[Node], Seq[Token])
  type Parsec = (Seq[Token]) => Result

  def mkASTree(ts: Seq[Token]): Node =
    parseTop(ts) match { case (Seq(node), Seq()) => node }

  private def parseTop(ts: Seq[Token]): Result = {
    val desc = "a top level expression"
    val expr = any(desc, parseStatic, parseFun, one("NewLine"))
    val parser = seq(repPat(expr, "EOF"), one("EOF"))
    val (ns, rm) = parser(ts)
    (Seq(ast.Top(ns)(ts.head.pos)), rm)
  }

  private def parseFun(ts: Seq[Token]): Result = {
    val fn = key("fn")
    val name = one("NamedId")
    val newl = one("NewLine")
    val body = dlist(parseStmt)
    val params = plist(parseParam)
    val parser = seq(fn, name, params, key(":"), parseType, newl, body)
    val (Seq(ast.NamedId(nm)
        , ast.List(ps)
        , tp: ast.Type
        , ast.List(bd))
        , rs) = parser(ts)
    val narrow = ps collect { case p: ast.Param => p }
    (Seq(ast.Fun(nm, narrow, tp, bd)(ts.head.pos)), rs)
  }

  private def parseApp(ts: Seq[Token]): Result = {
    val name = one("NamedId")
    val args = plist(parseExpr)
    val parser = seq(name, args)
    val (Seq(ast.NamedId(nm), ast.List(as)), rs) = parser(ts)
    (Seq(ast.App(nm, as)(ts.head.pos)), rs)
  }

  private def parseSubs(ts: Seq[Token]): Result = {
    val name = one("NamedId")
    val arg = seq(key("["), parseExpr, key("]"))
    val parser = seq(name, arg)
    val (Seq(ast.NamedId(nm), as), rs) = parser(ts)
    (Seq(ast.Subscript(nm, as)(ts.head.pos)), rs)
  }

  private def parseCons(ts: Seq[Token]): Result = {
    val typedef = seq(key("["), parseType, key("]"))
    val parser = seq(typedef, plist(parseExpr))
    val (Seq(tp, ast.List(args)), rs) = parser(ts)
    (Seq(ast.Cons(tp, args)(ts.head.pos)), rs)
  }

  private def parseStmt(ts: Seq[Token]): Result = {
    val exprStmt = seq(parseExpr, one("NewLine"))
    val callStmt = seq(parseApp, one("NewLine"))
    val newlStmt = one("NewLine")
    val passStmt = seq(key("pass"), one("NewLine"))
    val parser = any("a statement",
      newlStmt,
      exprStmt,
      callStmt,
      passStmt,
      parseLet,
      parseMut,
      parseStatic,
      parseFun,
      parseCond)

    parser(ts)
  }

  private def parseParam(ts: Seq[Token]): Result = {
    val parser = seq(one("NamedId"), key(":"), parseType)
    val (Seq(ast.NamedId(nm), tp), rs) = parser(ts)
    (Seq(ast.Param(nm, tp)(ts.head.pos)), rs)
  }

  private def parseListType(ts: Seq[Token]): Result = {
    val parenL = key("[")
    val parenR = key("]")
    val parser = seq(parenL, parseType, parenR)
    val (Seq(tp), rm) = parser(ts)
    (Seq(ast.ListType(tp)(ts.head.pos)), rm)
  }

  // TODO: Add more types!
  // e.g. tuples, list, maps, polymorphic
  private def parseType(ts: Seq[Token]): Result = {
    val simple = one("NamedId")
    val others = rep(seq(key("->"), simple))
    val simpTp = seq(simple, others)
    val parser = any("a type", simpTp, parseListType)

    val (types, rs) = parser(ts)
    (Seq(ast.Type(types)(ts.head.pos)), rs)
  }

  private def parseCond(ts: Seq[Token]): Result = {
    val block = dlist(parseStmt)
    val break = one("NewLine")
    val ifStmt = seq(key("if"), parseExpr, break, block)
    val elifStmt = rep(parseElif)
    val elseStmt = eat(parseElse)

    val (Seq(cond, ast.List(body)), ts0) = ifStmt(ts)
    val (elifNodes, ts1) = elifStmt(ts0)
    val (elseNodes, ts2) = elseStmt(ts1)

    (Seq(ast.Cond(cond, body, elifNodes ++ elseNodes)(ts.head.pos)), ts2)
  }

  private def parseElif(ts: Seq[Token]): Result = {
    val block = dlist(parseStmt)
    val break = one("NewLine")
    val parser = seq(key("elif"), parseExpr, break, block)
    val (Seq(cond, ast.List(body)), rs) = parser(ts)
    (Seq(ast.Elif(cond, body)(ts.head.pos)), rs)
  }

  private def parseElse(ts: Seq[Token]): Result = {
    val block = dlist(parseStmt)
    val break = one("NewLine")
    val parser = seq(key("else"), break, block)
    val (Seq(ast.List(body)), rs) = parser(ts)
    (Seq(ast.Else(body)(ts.head.pos)), rs)
  }

  private def parseStatic(ts: Seq[Token]): Result = {
    val static  = key("static")
    val name    = one("NamedId")
    val colon   = key(":")
    val assign  = key("=")
    val newline = one("NewLine")
    val parser  = seq(static, name, colon, parseType, assign, parseExpr, newline)
    val (Seq(ast.NamedId(nm), tp, rv), rm) = parser(ts)
    (Seq(ast.Static(nm, tp, rv)(ts.head.pos)), rm)
  }

  private def parseMut(ts: Seq[Token]): Result = {
    val let     = key("let")
    val mut     = key("mut")
    val name    = one("NamedId")
    val assign  = key("=")
    val newline = one("NewLine")
    val parser  = seq(let, mut, name, assign, parseExpr, newline)
    val (Seq(ast.NamedId(nm), rv), rm) = parser(ts)
    (Seq(ast.Mut(nm, rv)(ts.head.pos)), rm)
  }

  private def parseLet(ts: Seq[Token]): Result = {
    val let     = key("let")
    val name    = one("NamedId")
    val assign  = key("=")
    val newline = one("NewLine")
    val parser  = seq(let, name, assign, parseExpr, newline)
    val (Seq(ast.NamedId(nm), rv), rm) = parser(ts)
    (Seq(ast.Let(nm, rv)(ts.head.pos)), rm)
  }

  private def parseExpr(ts: Seq[Token]): Result = {
    val parser = seq(parseAtom, rep(seq(parseBinOp, parseAtom)))
    val (nodes, rm) = parser(ts)
    val (combined, Seq()) = sortExpr(nodes.tail, nodes.head, 0)
    (Seq(combined), rm)
  }

  private def parseBinOp(ts: Seq[Token]): Result = {
    val parser = any("an operator", precedenceMap.keys.toSeq.map(key):_*)
    parser(ts)
  }

  private def parseUnaOp(ts: Seq[Token]): Result = {
    val parser = seq(key("-"), parseAtom)
    val (Seq(ast.Operator(op), value), rm) = parser(ts)
    (Seq(ast.UnaOp(op, value)(ts.head.pos)), rm)
  }

  private def parseAtom(ts: Seq[Token]): Result = {
    val parens = seq(key("("), parseExpr, key(")"))
    val anymsg = "an atomic expression"
    val integr = one("Integer")
    val nameid = one("NamedId")
    val boolvl = any("boolean expression", key("false"), key("true"))
    val parser = any(anymsg,
      parens,
      integr,
      parseApp,
      parseSubs,
      parseCons,
      nameid,
      parseUnaOp,
      boolvl)

    val result = parser(ts)
    result
  }

  private def plist(item: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val parenL = key("(")
      val parenR = key(")")
      val others = repRaw(seq(key(","), item), ")")
      val parser = seq(parenL, eat(item, others), parenR)
      val (ns, rm) = parser(ts)
      (Seq(ast.List(ns)(ts.head.pos)), rm)
    }

  private def dlist(item: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val indent = one("Indent")
      val dedent = one("Dedent")
      val others = repPat(item, "Dedent")
      val parser = seq(indent, others, dedent)
      val (ns, rm) = parser(ts)
      (Seq(ast.List(ns)(ts.head.pos)), rm)
    }

  private def repPat(p: Parsec, until: String): Parsec =
    (ts: Seq[Token]) => {
      val buffer = mutable.Buffer[Node]()
      var remain = ts

      def reached = remain.headOption match {
        case Some(t) => (t.productPrefix == until)
        case None => false
      }

      while (! reached) {
        val (nodes, rm) = p(remain)
        buffer ++= nodes
        remain = rm
      }

      (buffer.toSeq, remain)
    }

  private def repRaw(p: Parsec, until: String): Parsec =
    (ts: Seq[Token]) => {
      val buffer = mutable.Buffer[Node]()
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

  private def rep(p: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val buffer = mutable.Buffer[Node]()
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

  private def any(msg: String, ps: Parsec*): Parsec =
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

  private def seq(ps: Parsec*): Parsec =
    (ts: Seq[Token]) => {
      ps.foldLeft((Seq[Node](), ts)) {
        case ((nodes, remains), parsec) =>
          val (n, newRemains) = parsec(remains)
          (nodes ++ n, newRemains)
      }
    }

  private def eat(ps: Parsec*): Parsec =
    (ts: Seq[Token]) => {
      val buffer = mutable.Buffer[Node]()
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
            throw err
        }

      (buffer.toSeq, remain)
    }

  private def one(pattern: String): Parsec =
    (ts: Seq[Token]) => ts match {
      case h +: rest if h.productPrefix == pattern =>
        (pin(h), rest)
      case others =>
        throw others.report(pattern)
    }

  private def key(s: String): Parsec =
    (ts: Seq[Token]) => ts match {
      case tokens.Reserve(`s`) +: rest  =>
        if (s == "pass")
          (Seq(ast.Nop()(ts.head.pos)), rest)
        else if (s == "true")
          (Seq(ast.Boolean(true)(ts.head.pos)), rest)
        else if (s == "false")
          (Seq(ast.Boolean(false)(ts.head.pos)), rest)
        else if (precedenceMap contains s)
          (Seq(ast.Operator(s)(ts.head.pos)), rest)
        else
          (Seq(), rest)
      case others =>
        throw others.report(s)
    }

  private def pin(t: Token): Seq[Node] = t match {
    case tokens.NamedId(n) => Seq(ast.NamedId(n)(t.pos))
    case tokens.Integer(n) => Seq(ast.Integer(n.toInt)(t.pos))
    case others => Seq()
  }

  private val precedenceMap = Map(
    ("==" -> 10),
    ("!=" -> 10),
    ("+"  -> 20),
    ("-"  -> 20),
    (">"  -> 30),
    ("<"  -> 30),
    ("<=" -> 30),
    (">=" -> 30),
    ("*"  -> 40),
    ("/"  -> 40))

  private def sortExpr(s: Seq[Node], n: Node, m: Int): (Node, Seq[Node]) =
    s match {
      case ast.Operator(op) +: rest1 =>
        val p = precedenceMap.get(op) getOrElse -1

        if (p < m) return (n, s)

        val (rhs1 +: rest2) = rest1
        val (rhs2,   rest3) = rest2 match {
          case ast.Operator(rhsOp) +: _ =>
            val next = precedenceMap.get(rhsOp) getOrElse -1

            if (p < next)
              sortExpr(rest2, rhs1, p + 1)
            else
              (rhs1, rest2)

          case _ =>
            (rhs1, rest2)
        }

        sortExpr(rest3, ast.BinOp(n, op, rhs2)(n.pos), m)
      case _ =>
        (n, s)
    }

  private implicit class NodeOps(val ts: Seq[Token]) {
    def report(s: String): ParserError =
      ts.headOption match {
        case Some(t) =>
          val e = if (t.raw == "\n") "\\n" else t.raw
          val m = s"${t.pos}: Expected $s but got '$e'."
          ParserError(ts.length, m)
        case None =>
          val m = s"Expected $s, but reached end of file."
          ParserError(ts.length, m)
      }
  }
}
