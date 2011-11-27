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
  case class VariableStatement(decls: List[VariableDeclaration]) extends Statement
  case class ExpressionStatement(e: Expression) extends Statement
  case class ReturnStatement(expr: Option[Expression]) extends Statement

  case class FunctionDeclaration(ident: String, params: List[String], body: List[SourceElement])

  case class VariableDeclaration(ident: String, initialiser: Option[Expression])

  sealed trait Expression
  case class PrefixExpression(op: UnaryOperator, r: Expression) extends Expression
  case class InfixExpression(l: Expression, op: BinaryOperator, r: Expression) extends Expression
  case class PostfixExpression(r: Expression, op: UnaryOperator) extends Expression
  sealed trait LiteralExpression extends Expression
  case class NumericLiteral(d: Double) extends LiteralExpression
  case class StringLiteral(d: String) extends LiteralExpression
  sealed trait MemberExpression extends Expression
  sealed trait PrimaryExpression extends MemberExpression
  case class Identifier(s: String) extends PrimaryExpression
  case class CallExpression(target: MemberExpression, args: List[Expression]) extends Expression

  sealed trait BinaryOperator
  sealed trait ReusableOperator extends BinaryOperator
  // Primary
  // Left-hand-side
  // Postfix
  // Unary
  // Multiplicative
  // Additive
  case object AdditionOperator extends ReusableOperator
  // Bitwise shift
  // Relational
  case object LessThanOperator extends BinaryOperator
  // Equality
  // Binary bitwise
  // Binary logical
  // Conditional
  // Assignment
  case object SimpleAssignmentOperator extends BinaryOperator
  case class CompoundAssignmentOperator(op: ReusableOperator) extends BinaryOperator
  // Comma

  sealed trait UnaryOperator
  case object IncrementOperator extends UnaryOperator
  case object DecrementOperator extends UnaryOperator

  import JavaConversions.iterableAsScalaIterable
  
  def nullableToOption[A <: AnyRef](a: A): Option[A] = if (a == null) None else Some(a)

  def transformScriptNode(node: Node): Program = {
    assert(node.getType() == Token.SCRIPT)
    val sourceElements = for (child <- node) yield transformSourceElement(child)
    Program(sourceElements.toList)
  }

  def transformSourceElement(node: Node): SourceElement = {
    node match {
      case decl: ast.VariableDeclaration => StatementSourceElement(VariableStatement(
          (for (child <- decl.getVariables()) yield VariableDeclaration(
              child.getTarget().asInstanceOf[ast.Name].getIdentifier(),
              nullableToOption(child.getInitializer()).map(transformExpression(_))
          )).toList
      ))
      case expressionStatement: ast.ExpressionStatement =>
        StatementSourceElement(ExpressionStatement(transformExpression(expressionStatement.getExpression())))
      case rs: ast.ReturnStatement => {
        val expr = for (child <- nullableToOption(rs.getReturnValue())) yield transformExpression(child)
        StatementSourceElement(ReturnStatement(expr))
      }
      case fn: ast.FunctionNode => {
        FunctionDeclarationSourceElement(FunctionDeclaration(
            fn.getFunctionName().getIdentifier(),
            (for (child <- fn.getParams()) yield child.asInstanceOf[ast.Name].getIdentifier()).toList,
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
      case in: ast.InfixExpression =>
        InfixExpression(transformExpression(in.getLeft()), transformBinaryOperator(in.getOperator()), transformExpression(in.getRight()))
      case un: ast.UnaryExpression if un.isPostfix() =>
        PostfixExpression(transformExpression(un.getOperand()), transformUnaryOperator(un.getOperator()))
      case un: ast.UnaryExpression =>
        PrefixExpression(transformUnaryOperator(un.getOperator()), transformExpression(un.getOperand()))
      case nl: ast.NumberLiteral => NumericLiteral(nl.getNumber())
      case sl: ast.StringLiteral => StringLiteral(sl.getValue())
      case fc: ast.FunctionCall => CallExpression(
        transformMemberExpression(fc.getTarget()),
        (for (arg <- fc.getArguments()) yield transformExpression(arg)).toList
      )
      case n: ast.Name => Identifier(n.getIdentifier())
      case _ => unimplementedTransform[Expression](node)
    }
  }
  
  def transformBinaryOperator(op: Int): BinaryOperator = {
    op match {
      case Token.ADD => AdditionOperator
      case Token.LT => LessThanOperator
      case Token.ASSIGN => SimpleAssignmentOperator
      case Token.ASSIGN_ADD => CompoundAssignmentOperator(AdditionOperator)
      case _ => error("Cannot transform BinaryOperator: "+op)
    }
  }

  def transformUnaryOperator(op: Int): UnaryOperator = {
    op match {
      case Token.INC => IncrementOperator
      case Token.DEC => DecrementOperator
      case _ => error("Cannot transform UnaryOperator: "+op)
    }
  }

  def unimplementedTransform[A](node: Node): A = error("Not implemented: " + node.getClass);

  def parse(programSource: String): Program = {
    val rhinoParser = new RhinoParser()
    val rhinoAst = rhinoParser.parse(programSource, "<program>", 1)
    transformScriptNode(rhinoAst.asInstanceOf[AstRoot])
  }

}