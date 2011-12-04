package com.richdougherty.jsai

import org.mozilla.javascript.CompilerEnvirons
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
  case class BlockStatement(stmts: List[Statement]) extends Statement
  case class VariableStatement(decls: List[VariableDeclaration]) extends Statement
  case class ExpressionStatement(e: Expression) extends Statement
  case class IfStatement(
      testExpr: Expression,
      trueStmt: Statement,
      falseStmt: Option[Statement]) extends Statement
  sealed trait IterationStatement
  case class ForStatement(
      init: Either[Option[Expression],List[VariableDeclaration]],
      testExpr: Option[Expression],
      incrExpr: Option[Expression],
      stmt: Statement) extends Statement with IterationStatement
  case class ReturnStatement(expr: Option[Expression]) extends Statement
  case class SwitchStatement() extends Statement // FIXME: Stub
  case class LabelledStatement(label: String, child: Statement) extends Statement

  case class FunctionDeclaration(ident: String, params: List[String], body: List[SourceElement])

  case class VariableDeclaration(ident: String, initialiser: Option[Expression])

  sealed trait Expression
  case class PrefixExpression(op: UnaryOperator, r: Expression) extends Expression
  case class InfixExpression(l: Expression, op: BinaryOperator, r: Expression) extends Expression
  case class PostfixExpression(r: Expression, op: UnaryOperator) extends Expression
  sealed trait LiteralExpression extends Expression
  case class BooleanLiteral(b: Boolean) extends LiteralExpression
  case class NumericLiteral(d: Double) extends LiteralExpression
  case class StringLiteral(d: String) extends LiteralExpression
  case class ArrayInitialiser(elems: List[Option[Expression]]) extends Expression
  case class ObjectInitialiser(pas: List[PropAssign]) extends Expression
  case class PropertyAccessor(base: Expression, propertyName: Expression) extends Expression
  sealed trait MemberExpression extends Expression
  sealed trait PrimaryExpression extends MemberExpression
  case class Identifier(s: String) extends PrimaryExpression
  case class CallExpression(target: MemberExpression, args: List[Expression]) extends Expression
  case class ConditionalExpression(testExpr: Expression, trueExpr: Expression, falseExpr: Expression) extends Expression

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
  case object NegativeOperator extends UnaryOperator

  sealed trait PropAssign
  case class ValuePropAssign(n: PropAssignName, v: Expression) extends PropAssign
  //case class GetPropAssign(n: PropAssignName, body: List[SourceElement]) extends PropAssign
  //case class SetPropAssign(n: PropAssignName, params: List[String], body: List[SourceElement]) extends PropAssign
  sealed trait PropAssignName
  case class IdentifierPropAssignName(n: String) extends PropAssignName
  //case class StringLiteralPropAssignName(n: StringLiteral) extends PropAssignName
  //case class NumericLiteralPropAssignName(n: NumericLiteral) extends PropAssignName

  import JavaConversions.iterableAsScalaIterable
  
  def nullableToOption[A <: AnyRef](a: A): Option[A] = if (a == null) None else Some(a)

  def transformScriptNode(node: Node): Program = {
    assert(node.getType() == Token.SCRIPT)
    val sourceElements = for (child <- node) yield transformSourceElement(child)
    Program(sourceElements.toList)
  }

  def transformSourceElement(node: Node): SourceElement = {
    node match {
      case fn: ast.FunctionNode => {
        FunctionDeclarationSourceElement(FunctionDeclaration(
            fn.getFunctionName().getIdentifier(),
            (for (child <- fn.getParams()) yield child.asInstanceOf[ast.Name].getIdentifier()).toList,
            (for (child <- fn.getBody()) yield transformSourceElement(child)).toList
        ))
      }
      case _ => StatementSourceElement(transformStatement(node))
    }
  }

  def transformStatement(node: Node): Statement = {
    node match {
      case sc: ast.Scope if sc.getType == Token.BLOCK => BlockStatement(
        (for (child <- sc.getStatements()) yield transformStatement(child)).toList
      )
      case decl: ast.VariableDeclaration => VariableStatement(
          transformVariableDeclarationList(decl)
      )
      case expressionStatement: ast.ExpressionStatement =>
        ExpressionStatement(transformExpression(expressionStatement.getExpression()))
      case is: ast.IfStatement => IfStatement(
        transformExpression(is.getCondition()),
        transformStatement(is.getThenPart()),
        nullableToOption(is.getElsePart()).map(transformStatement(_))
      )
      case fl: ast.ForLoop => ForStatement(
          fl.getInitializer() match {
            case decl: ast.VariableDeclaration => Right(transformVariableDeclarationList(decl))
            case null => Left(None)
            case expr => Left(Some(transformExpression(expr)))
          },
          nullableToOption(fl.getCondition()).map(transformExpression(_)),
          nullableToOption(fl.getIncrement()).map(transformExpression(_)),
          transformStatement(fl.getBody())
      )
      case rs: ast.ReturnStatement => {
        val expr = for (child <- nullableToOption(rs.getReturnValue())) yield transformExpression(child)
        ReturnStatement(expr)
      }
      case ls: ast.LabeledStatement => {
        val stmt = transformStatement(ls.getStatement());
        ls.getLabels().foldRight(stmt)((lab: ast.Label, childStmt: Statement) => LabelledStatement(lab.getName(), childStmt))
      }
      case _ => unimplementedTransform[Statement](node)
    }
  }

  def transformVariableDeclarationList(decl: ast.VariableDeclaration): List[VariableDeclaration] = {
    (for (child <- decl.getVariables()) yield VariableDeclaration(
      child.getTarget().asInstanceOf[ast.Name].getIdentifier(),
      nullableToOption(child.getInitializer()).map(transformExpression(_))
    )).toList
  }

  def transformMemberExpression(node: Node): MemberExpression = node match {
    case n: ast.Name => Identifier(n.getIdentifier())
  }

  def transformExpression(node: Node): Expression = {
    node match {
      case nl: ast.NumberLiteral => NumericLiteral(nl.getNumber())
      case sl: ast.StringLiteral => StringLiteral(sl.getValue())
      case al: ast.ArrayLiteral => ArrayInitialiser(
        al.getElements().toList.map {
          case n: Node => Some(transformExpression(n))
        }
      )
      case ol: ast.ObjectLiteral => ObjectInitialiser(
        ol.getElements().toList.map {
          case op: ast.ObjectProperty => {
            val n = op.getLeft() match {
              case n: ast.Name => IdentifierPropAssignName(n.getIdentifier())
            }
            if (op.getType() == Token.COLON) {
              val v = transformExpression(op.getRight())
              ValuePropAssign(n, v)
            } else error("Unsupported ObjectProperty type: " + op.getType())
          }
        }
      )
      case eg: ast.ElementGet => PropertyAccessor(
        transformExpression(eg.getTarget()),
        transformExpression(eg.getElement())
      )
      // "The dot notation is explained by the following syntactic conversion..."
      case in: ast.InfixExpression if in.getOperator() == Token.GETPROP => PropertyAccessor(
        transformExpression(in.getLeft()),
        StringLiteral(in.getRight().asInstanceOf[ast.Name].getIdentifier())
      )
      case in: ast.InfixExpression =>
        InfixExpression(transformExpression(in.getLeft()), transformBinaryOperator(in.getOperator()), transformExpression(in.getRight()))
      case un: ast.UnaryExpression if un.isPostfix() =>
        PostfixExpression(transformExpression(un.getOperand()), transformUnaryOperator(un.getOperator()))
      case un: ast.UnaryExpression =>
        PrefixExpression(transformUnaryOperator(un.getOperator()), transformExpression(un.getOperand()))
      case kl: ast.KeywordLiteral => kl.getType() match {
        case Token.TRUE => BooleanLiteral(true)
        case Token.FALSE => BooleanLiteral(false)
      }
      case fc: ast.FunctionCall => CallExpression(
        transformMemberExpression(fc.getTarget()),
        (for (arg <- fc.getArguments()) yield transformExpression(arg)).toList
      )
      case n: ast.Name => Identifier(n.getIdentifier())
      case c: ast.ConditionalExpression => ConditionalExpression(
        transformExpression(c.getTestExpression()),
        transformExpression(c.getTrueExpression()),
        transformExpression(c.getFalseExpression())
      )
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
      case Token.NEG => NegativeOperator
      case _ => error("Cannot transform UnaryOperator: "+op)
    }
  }

  def unimplementedTransform[A](node: Node): A = error("Not implemented: " + node.getClass);

  def parse(programSource: String): Program = {
    val ces = new CompilerEnvirons()
    ces.setIdeMode(true)
    val rhinoParser = new RhinoParser(ces)
    val rhinoAst = rhinoParser.parse(programSource, "<program>", 1)
    transformScriptNode(rhinoAst.asInstanceOf[AstRoot])
  }

}