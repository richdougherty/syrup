package com.richdougherty.jsai

import org.mozilla.javascript.{ Parser => RhinoParser }
import org.mozilla.javascript.Node
import org.mozilla.javascript.Token
import org.mozilla.javascript.ast
import scala.collection.JavaConversions
import org.mozilla.javascript.ast.AstRoot

object Parser {
  
  case class Program(ses: List[SourceElement])
  sealed trait SourceElement
  case class StatementSourceElement(statement: Statement) extends SourceElement
  case class FunctionDeclarationSourceElement(functionDeclaration: FunctionDeclaration) extends SourceElement

  sealed trait Statement
  case class ExpressionStatement(e: Expression) extends Statement
  case class ReturnStatement(expr: Option[Expression]) extends Statement

  case class FunctionDeclaration(ident: String, params: List[String], body: List[SourceElement])

  sealed trait Expression
  case class InfixExpression(l: Expression, op: Operator, r: Expression) extends Expression
  sealed trait LiteralExpression extends Expression
  case class StringLiteral(d: String) extends LiteralExpression
  sealed trait MemberExpression extends Expression
  sealed trait PrimaryExpression extends MemberExpression
  case class Identifier(s: String) extends PrimaryExpression
  case class CallExpression(target: MemberExpression, args: List[Expression]) extends Expression

  sealed trait Operator
  case object AdditionOperator extends Operator
  
  import JavaConversions.iterableAsScalaIterable
  
  def nullableToOption[A <: AnyRef](a: A): Option[A] = if (a == null) None else Some(a)

  def transformScriptNode(node: Node): Program = {
    assert(node.getType() == Token.SCRIPT)
    val sourceElements = for (child <- node) yield transformSourceElement(child)
    Program(sourceElements.toList)
  }

  def transformSourceElement(node: Node): SourceElement = {
    node match {
      case expressionStatement: ast.ExpressionStatement =>
        StatementSourceElement(ExpressionStatement(transformExpression(expressionStatement.getExpression())))
      case rs: ast.ReturnStatement => {
        val expr = for (child <- nullableToOption(rs.getReturnValue())) yield transformExpression(child)
        StatementSourceElement(ReturnStatement(expr))
      }
      case fn: ast.FunctionNode => {
        FunctionDeclarationSourceElement(FunctionDeclaration(
            fn.getFunctionName().getIdentifier(),
            (for (child <- fn.getParams()) yield unimplementedTransform[String](child)).toList,
            (for (child <- fn.getBody()) yield transformSourceElement(child)).toList
        ))
      }
      case _ => unimplementedTransform[SourceElement](node)
    }
  }

  def transformMemberExpression(node: Node): MemberExpression = node match {
    case n: ast.Name => Identifier(n.getIdentifier())
  }

  def transformExpression(node: Node): Expression = {
    node match {
      case infixExpression: ast.InfixExpression =>
        InfixExpression(transformExpression(infixExpression.getLeft()), transformOperator(infixExpression.getOperator()), transformExpression(infixExpression.getRight()))
      case sl: ast.StringLiteral => StringLiteral(sl.getValue())
      case fc: ast.FunctionCall => CallExpression(
        transformMemberExpression(fc.getTarget()),
        (for (arg <- fc.getArguments()) yield transformExpression(arg)).toList
      )
      case _ => unimplementedTransform[Expression](node)
    }
  }
  
  def transformOperator(op: Int): Operator = {
    op match {
      case Token.ADD => AdditionOperator
      case _ => error("Cannot transform operator: "+op)
    }
  }

  def unimplementedTransform[A](node: Node): A = error("Not implemented: " + node.getClass);

  def parse(programSource: String): Program = {
    val rhinoParser = new RhinoParser()
    val rhinoAst = rhinoParser.parse(programSource, "<program>", 1)
    transformScriptNode(rhinoAst.asInstanceOf[AstRoot])
  }

}