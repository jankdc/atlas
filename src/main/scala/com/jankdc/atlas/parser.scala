package com.jankdc.atlas

import scala.util.{Try, Failure, Success}
import scala.collection.mutable.Buffer
import scala.io.Source

case class UnexpectedToken(val parsed: Int, msg: String)
  extends RuntimeException(msg)

object parser {
  type Result = (Seq[Node], Seq[Token])
  type Parsec = (Seq[Token]) => Result

  def main(args: Array[String]): Unit = {
    try {
      val stream = Source.fromURL(getClass.getResource("/atom.atlas"))
      val source = stream.mkString
      val tokens = lexer.lex(source)
      tokens.foreach(println)
        val astree = parser.parse(tokens)
        astree.foreach{n => show(n); println()}
      if (parser.checkTokens(tokens)) {
      }
    }
    catch {
      case err: UnexpectedToken => println("[error] " ++ err.getMessage)
    }
  }

  private
  def checkTokens(ts: Seq[Token]): Boolean = {
    for (t <- ts) t match {
      case Token(tokens.Unknown, r, l) =>
        println(s"[error][${l.row},${l.column}]: Unknown token: $r")
        return false
      case Token(tokens.Badent, r, l) =>
        println(s"[error][${l.row},${l.column}]: Bad indentation.")
        return false
      case others => // Do nothing
    }
    true
  }

  private
  def show(node: Node): Unit = node match {
    case Node(ast.Abs(n, ps, bd), _) =>
      show(n); print(": ")
      ps.foreach{p => show(p); print(" ")}
      println()
      bd.foreach{b => print("  "); show(b); println()}
    case Node(ast.Param(l, r), _) =>
      print("("); show(l); print(":"); show(r); print(")")
    case Node(ast.App(n, args), _) =>
      show(n)
      print("("); args.foreach{a => show(a); print(" ")}; print(")")
    case Node(ast.Type(n), _) =>
      show(n)
    case Node(ast.Let(n, rv), _) =>
      show(n); print(" = "); show(rv)
    case Node(ast.Mut(n, rv), _) =>
      show(n); print(" = "); show(rv)
    case Node(ast.Name(n), _) =>
      print(n)
    case Node(ast.Number(n), _) =>
      print(n)
    case Node(ast.Nop(), _) =>
      print("nop")
  }

  private
  def parse(ts: Seq[Token]): Seq[Node] = {
    val newl   = one(tokens.NewLine)
    val abs    = parseAbs _
    val let    = seq(parseLet, one(tokens.NewLine))
    val expr   = any("a top level expression", abs, let, newl)
    val parser = seq(rep(expr, tokens.EOF), one(tokens.EOF))
    parser(ts) match {
      case (nodes, rest) =>
        nodes.filterNot(n => n.group.isInstanceOf[ast.Nop])
    }
  }

  private
  def parseAbs(ts: Seq[Token]): Result = {
    val fn      = one(tokens.Fn)
    val name    = one(tokens.Name)
    val comma   = one(tokens.Comma)
    val params  = parenList(parseParam, comma)
    val colon   = one(tokens.Colon)
    val newline = one(tokens.NewLine)
    val tp      = parseType _
    val body    = dentList(parseStmt, newline)

    val defLhsParser = seq(fn, name)
    val defRhsParser = seq(colon, tp, newline)

    val (ns1, rm1) = defLhsParser(ts)
    val (ns2, rm2) = params(rm1)
    val (ns3, rm3) = defRhsParser(rm2)
    val (ns4, rm4) = body(rm3)

    ns1 ++ ns3 match {
      case Seq(fn, name, _, tp, _) =>
        (Seq(Node(ast.Abs(name, ns2 :+ tp, ns4), fn.line)), rm4)
    }
  }

  private
  def parseApp(ts: Seq[Token]): Result = {
    val name   = one(tokens.Name)
    val comma  = one(tokens.Comma)
    val args   = parenList(parseAtom, comma)
    val parser = seq(name, args)
    parser(ts) match {
      case (h +: rest, rem) =>
        (Seq(Node(ast.App(h, rest), h.line)), rem)
    }
  }

  private
  def parseStmt(ts: Seq[Token]): Result = {
    val parser = any("a statement", parseLet, parseMut, parseExpr)
    parser(ts)
  }

  private
  def parseExpr(ts: Seq[Token]): Result = {
    val parser = any("a simple expression", parseAtom)
    parser(ts)
  }

  private
  def parseParam(ts: Seq[Token]): Result = {
    val name   = one(tokens.Name)
    val colon  = one(tokens.Colon)
    val tp     = parseType _
    val parser = seq(name, colon, tp)
    parser(ts) match {
      case (Seq(name, _, tp), rest) =>
        (Seq(Node(ast.Param(name, tp), name.line)), rest)
    }
  }

  private
  def parseType(ts: Seq[Token]): Result = {
    val parser = one(tokens.Name)
    parser(ts) match {
      case (Seq(name), rest) =>
        (Seq(Node(ast.Type(name), name.line)), rest)
    }
  }

  private
  def parseMut(ts: Seq[Token]): Result = {
    val let    = one(tokens.Let)
    val mut    = one(tokens.Mut)
    val name   = one(tokens.Name)
    val assign = one(tokens.Assign)
    val parser = seq(let, mut, name, assign, parseExpr)
    parser(ts) match {
      case (Seq(let, _, name, _, value), rest) =>
        (Seq(Node(ast.Mut(name, value), let.line)), rest)
    }
  }

  private
  def parseLet(ts: Seq[Token]): Result = {
    val let    = one(tokens.Let)
    val name   = one(tokens.Name)
    val assign = one(tokens.Assign)
    val parser = seq(let, name, assign, parseExpr)
    parser(ts) match {
      case (Seq(let, name, _, value), rest) =>
        (Seq(Node(ast.Let(name, value), let.line)), rest)
    }
  }

  private
  def parseAtom(ts: Seq[Token]): Result = {
    val parenL = one(tokens.ParenL)
    val parenR = one(tokens.ParenR)
    val number = one(tokens.Number)
    val paren  = bin(parenL, parseAtom, parenR)
    val name   = one(tokens.Name)
    val parser = any("an atomic expression", number, paren, name)
    parser(ts)
  }

  private
  def parenList(item: Parsec, delim: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val parenL = one(tokens.ParenL)
      val parenR = one(tokens.ParenR)

      val (_, r1) = parenL(ts)

      val single = try Some(item(r1)) catch {
        case err: UnexpectedToken => None
      }

      single match {
        case None =>
          val (_, r2) = parenR(r1)
          (Seq(), r2)
        case Some((n2, r2)) =>
          val items  = rep(una(delim, item), tokens.ParenR)
          val parser = seq(items, parenR)
          val (n3, r3) = parser(r2)
          (n2 ++ n3, r3)
      }
    }

  private
  def dentList(item: Parsec, delim: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val indent = one(tokens.Indent)
      val dedent = one(tokens.Dedent)
      val items  = rep(unaRhs(item, delim), tokens.Dedent)
      val parser = seq(indent, items)
      parser(ts) match {
        case (h +: rest, rem) =>
          val (_, finalRem) = dedent(rem)
          (rest, finalRem)
      }
    }

  private
  def lst(item: Parsec, delim: Parsec): Parsec =
    seqOpt(item, repOpt(una(delim, item)))

  private
  def rep(parsec: Parsec, end: tokens.Group): Parsec = {
    (ts: Seq[Token]) => {
      var buffer = Buffer[Node]()
      var remain = ts
      var reached = remain.headOption match {
        case Some(Token(g, _, _)) => (g == end)
        case None => false
      }

      while (! remain.isEmpty && ! reached) {
        val (nodes, rm) = parsec(remain)
        buffer ++= nodes
        remain = rm
        reached = remain.headOption match {
          case Some(Token(g, _, _)) => (g == end)
          case None => false
        }
      }
      (buffer.toSeq, remain)
    }
  }

  private
  def rep(parsec: Parsec): Parsec = {
    (ts: Seq[Token]) => {
      var buffer = Buffer[Node]()
      var remain = ts
      while (! remain.isEmpty) {
        val (nodes, rm) = parsec(remain)
        buffer ++= nodes
        remain = rm
      }
      (buffer.toSeq, remain)
    }
  }

  private
  def repOpt(parsec: Parsec): Parsec = {
    (ts: Seq[Token]) => {
      var buffer = Buffer[Node]()
      var remain = ts
      try {
        while (! remain.isEmpty) {
          val (nodes, rm) = parsec(remain)
          buffer ++= nodes
          remain = rm
        }
      }
      catch {
        case err: UnexpectedToken
          if err.parsed == remain.length =>
            // Do nothing
      }
      (buffer.toSeq, remain)
    }
  }

  private
  def any(msg: String, parsecs: Parsec*): Parsec = {
    (ts: Seq[Token]) => {
      var result = Option[Result](null)
      var furthest = UnexpectedToken(ts.length, "")
      for (parsec <- parsecs) try {
        result = Some(parsec(ts))
      }
      catch {
        case err: UnexpectedToken =>
          if (err.parsed < furthest.parsed) {
            furthest = err
          }
      }
      (result, furthest.parsed == ts.length) match {
        case (Some(nodes), _) =>
          nodes
        case (None, false) =>
          throw furthest
        case (None, true) =>
          throw report(ts, msg)
      }
    }
  }

  // NOTE: Drops lhs result.
  private
  def una(lhs: Parsec, rhs: Parsec): Parsec = {
    (ts: Seq[Token]) => {
      val (_, r1) = lhs(ts)
      val (n, r2) = rhs(r1)
      (n, r2)
    }
  }

  // NOTE: Drops lhs result.
  private
  def unaRhs(lhs: Parsec, rhs: Parsec): Parsec = {
    (ts: Seq[Token]) => {
      val (n, r1) = lhs(ts)
      val (_, r2) = rhs(r1)
      (n, r2)
    }
  }

  // NOTE: Drops lhs and rhs results.
  private
  def bin(lhs: Parsec, mid: Parsec, rhs: Parsec): Parsec = {
    (ts: Seq[Token]) => {
      val (_, r1) = lhs(ts)
      val (n, r2) = mid(r1)
      val (_, r3) = rhs(r2)
      (n, r3)
    }
  }

  private
  def seq(parsecs: Parsec*): Parsec = {
    (ts: Seq[Token]) => {
      parsecs.foldLeft((Seq[Node](), ts)) {
        case ((res, rem), parsec) =>
          val (r, newRem) = parsec(rem)
          (res ++ r, newRem)
      }
    }
  }

  private
  def seqOpt(parsecs: Parsec*): Parsec = {
    (ts: Seq[Token]) => {
      var buffer = Buffer[Node]()
      var remain = ts

      try {
        for (parsec <- parsecs) {
          val (nodes, rm) = parsec(remain)
          buffer ++= nodes
          remain = rm
        }
      }
      catch {
        case err: UnexpectedToken
          if err.parsed == remain.length =>
            // Do nothing
      }

      (buffer.toSeq, remain)
    }
  }

  private
  def one(group: tokens.Group): Parsec = {
    (ts: Seq[Token]) => ts match {
      case token +: rest if token.group == group =>
        (Seq(wrap(token)), rest)
      case ts =>
        throw report(ts, s"$group")
    }
  }

  private
  def wrap(token: Token): Node = token match {
    case Token(tokens.Name, raw, line) =>
      Node(ast.Name(raw), line)
    case Token(tokens.Number, raw, line) =>
      Node(ast.Number(raw.toInt), line)
    case Token(others, raw, line) =>
      Node(ast.Nop(), line)
  }

  private
  def report(ts: Seq[Token], expected: String): UnexpectedToken = {
    ts.headOption match {
      case Some(Token(g,_,Line(r,c))) =>
        val errMsg = s"[$r,$c]: Expected $expected, got $g."
        UnexpectedToken(ts.length, errMsg)
      case None =>
        val errMsg = s"Expected $expected, but reached end of file."
        UnexpectedToken(ts.length, errMsg)
    }
  }
}
