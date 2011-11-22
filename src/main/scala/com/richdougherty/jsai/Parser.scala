package com.richdougherty.jsai

import org.mozilla.javascript.{ Parser => RhinoParser }
import org.mozilla.javascript.Node
import org.mozilla.javascript.Token
import org.mozilla.javascript.ast
import scala.collection.JavaConversions
import org.mozilla.javascript.ast.AstRoot

class Parser {

  case class Program(ses: List[SourceElement])
  sealed trait SourceElement
  case class StatementSourceElement(statement: Statement) extends SourceElement
  //case class FunctionDeclarationSourceElement(functionDeclaration: FunctionDeclaration) extends SourceElement

  sealed trait Statement
  case class ExpressionStatement(e: Expression) extends Statement
  
  sealed trait Expression
  case class InfixExpression(l: Expression, op: Operator, r: Expression) extends Expression
  case class LiteralExpression(l: Literal) extends Expression
  
  sealed trait Literal
  case class StringLiteral(d: String) extends Literal
  
  sealed trait Operator
  case object AdditionOperator extends Operator
  
  import JavaConversions.iterableAsScalaIterable

  def transformScriptNode(node: Node): Program = {
    assert(node.getType() == Token.SCRIPT)
    val sourceElements = for (child <- node) yield transformSourceElement(child)
    Program(sourceElements.toList)
  }

  def transformSourceElement(node: Node): SourceElement = {
    node match {
      case expressionStatement: ast.ExpressionStatement =>
        StatementSourceElement(ExpressionStatement(transformExpression(expressionStatement.getExpression())))
      case _ => unimplementedTransform[SourceElement](node)
    }
  }

  def transformExpression(node: Node): Expression = {
    node match {
      case infixExpression: ast.InfixExpression =>
        InfixExpression(transformExpression(infixExpression.getLeft()), transformOperator(infixExpression.getOperator()), transformExpression(infixExpression.getRight()))
      case sl: ast.StringLiteral => LiteralExpression(StringLiteral(sl.getValue()))
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