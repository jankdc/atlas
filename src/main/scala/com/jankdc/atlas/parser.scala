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
      val astree = parser.parse(tokens)
      astree.foreach{n => show(n); println()}
    }
    catch {
      case err: UnexpectedToken => println("[error] " ++ err.getMessage)
    }
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

  def parse(ts: Seq[Token]): Seq[Node] = {
    val desc = "a top level expression"
    val expr = any(desc, parseLet, parseAbs, one(tokens.NewLine))
    val comb = rep(expr, tokens.EOF)
    val (ns, _) = comb(ts)
    ns.filterNot(n => n.group.isInstanceOf[ast.Nop])
  }

  private
  def parseAbs(ts: Seq[Token]): Result = {
    val fn     = one(tokens.Fn)
    val name   = one(tokens.Name)
    val comma  = one(tokens.Comma)
    val params = plist(parseParam, comma)
    val colon  = one(tokens.Colon)
    val newl   = one(tokens.NewLine)
    val fnLhs  = seq(fn, name)
    val fnRhs  = seq(colon, parseType, newl)
    val body   = dlist(parseStmt)

    val rs0 = ts
    val (ns1, rs1) = fnLhs(rs0)
    val (ns2, rs2) = params(rs1)
    val (ns3, rs3) = fnRhs(rs2)
    val (ns4, rs4) = body(rs3)

    val Seq(f, n, _, t, _) = ns1 ++ ns3
    (Seq(Node(ast.Abs(n, ns2 :+ t, ns4), f.line)), rs4)
  }

  private
  def parseApp(ts: Seq[Token]): Result = {
    val name  = one(tokens.Name)
    val comma = one(tokens.Comma)
    val args  = plist(parseAtom, comma)
    val comb  = seq(name, args)
    val (h +: rest, rs) = comb(ts)
    (Seq(Node(ast.App(h, rest), h.line)), rs)
  }

  private
  def parseStmt(ts: Seq[Token]): Result = {
    val newl = one(tokens.NewLine)
    val expr = rhs(parseExpr, newl)
    val comb = any("a statement", parseLet, parseMut, expr)
    comb(ts)
  }

  // TODO: Need to add built-in operators.
  // Plese refer to occam's version.
  // https://github.com/jankdc/occam
  private
  def parseExpr(ts: Seq[Token]): Result = {
    val parser = any("a simple expression", parseAtom)
    parser(ts)
  }

  private
  def parseParam(ts: Seq[Token]): Result = {
    val name  = one(tokens.Name)
    val colon = one(tokens.Colon)
    val comb  = seq(name, colon, parseType)
    val (Seq(n, _, tp), rs) = comb(ts)
    (Seq(Node(ast.Param(n, tp), n.line)), rs)
  }

  // TODO: Add more types!
  // e.g. tuples, list, maps, polymorphic
  private
  def parseType(ts: Seq[Token]): Result = {
    val name = one(tokens.Name)
    val comb = any("a type", name)
    comb(ts)
  }

  private
  def parseMut(ts: Seq[Token]): Result = {
    val let    = one(tokens.Let)
    val mut    = one(tokens.Mut)
    val name   = one(tokens.Name)
    val assign = one(tokens.Assign)
    val newl   = one(tokens.NewLine)
    val comb   = seq(let, mut, name, assign, parseExpr, newl)
    val (Seq(l, _, n, _, v, _), rs) = comb(ts)
    (Seq(Node(ast.Mut(n, v), l.line)), rs)
  }

  private
  def parseLet(ts: Seq[Token]): Result = {
    val let    = one(tokens.Let)
    val name   = one(tokens.Name)
    val assign = one(tokens.Assign)
    val newl   = one(tokens.NewLine)
    val comb   = seq(let, name, assign, parseExpr, newl)
    val (Seq(l, n, _, v, _), rs) = comb(ts)
    (Seq(Node(ast.Let(n, v), l.line)), rs)
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
  def plist(item: Parsec, delim: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val parenL = one(tokens.ParenL)
      val parenR = one(tokens.ParenR)

      val (_, r1) = parenL(ts)
      val single = try Some(item(r1)) catch { case _:UnexpectedToken => None }

      single match {
        case None =>
          val (_, r2) = parenR(r1)
          (Seq(), r2)
        case Some((n2, r2)) =>
          val parser   = rep(lhs(delim, item), tokens.ParenR)
          val (n3, r3) = parser(r2)
          (n2 ++ n3, r3)
      }
    }

  private
  def dlist(item: Parsec): Parsec =
    (ts: Seq[Token]) => {
      val indent = one(tokens.Indent)
      val dedent = one(tokens.Dedent)
      val items  = rep(item, tokens.Dedent)
      val comb   = lhs(indent, items)
      comb(ts)
    }

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

      if (reached) {
        (buffer.toSeq, remain.tail)
      }
      else {
        (buffer.toSeq, remain)
      }
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

  private
  def lhs(lhs: Parsec, rhs: Parsec): Parsec = {
    (ts: Seq[Token]) => {
      val (_, r1) = lhs(ts)
      val (n, r2) = rhs(r1)
      (n, r2)
    }
  }

  private
  def rhs(lhs: Parsec, rhs: Parsec): Parsec = {
    (ts: Seq[Token]) => {
      val (n, r1) = lhs(ts)
      val (_, r2) = rhs(r1)
      (n, r2)
    }
  }

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
