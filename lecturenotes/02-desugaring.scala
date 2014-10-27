/*
These are lecture notes for the "Programming Languages and Types" at the University of Tübingen.

loosely based on Sec. 4 of the 2nd edition of the book 
"Programming Languages: Application and Interpretation" by Shriram Krishnamurthi

Please send comments or errors in these notes via email to Klaus Ostermann.
My email address can be found on my webpage. Alternatively, you can also 
propose corrections as a github pull request.

*/

/* In the exercises you have already seen the basic structure of an interpreter
 * by means of an interpreter for a language of arithmetic expressions: */
object AE {
  // Abstract Syntax Tree
  sealed trait Expression // we use "sealed" to get completeness checks for pattern matching
  case class Literal(n: Int) extends Expression
  case class Addition(lhs: Expression, rhs: Expression) extends Expression

  // Example
  val ex = Addition(Literal(1), Addition(Literal(5), Literal(3)))

  // Interpreter
  def eval(e: Expression): Int =
    e match {
      case Literal(n) => n
      case Addition(lhs, rhs) =>
        eval(lhs) + eval(rhs)
    }
}

/* 
 * In this lecture we want to study the technique of desugaring as a means
 * to structure programming languages and decompose a language into a
 * core language and syntactic sugar.
 * 
 * For illustration, consider the following proposed extensions to the language:
 *  1. Multiplication
 *  2. Subtraction
 *  3. Unary Negation 
 *
 * Extension number 1 is a good example for a core language extension.
 * We have no way of expressing multiplication in terms of the existing 
 * constructs (if we had some looping construct we could express multiplication
 * as repeated addition but we do not have loops).
 * 
 * Hence we add this language construct to the (core) language:
 */

object MAE {
  // Abstract Syntax Tree
  sealed trait Expression
  case class Literal(n: Int) extends Expression
  case class Addition(lhs: Expression, rhs: Expression) extends Expression
  case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
  // Example
  val ex = Addition(Literal(1), Multiplication(Literal(5), Literal(3)))

  // Interpreter
  def eval(e: Expression): Int =
    e match {
      case Literal(n) => n
      case Addition(lhs, rhs) => eval(lhs) + eval(rhs)
      case Multiplication(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}

/* Let us now consider extension #2, subtraction. One way to support subtraction is
 * to add it to the core language, just like multiplication: 
 */

object SMAE {
  // Abstract Syntax Tree
  sealed trait Expression
  case class Literal(n: Int) extends Expression
  case class Addition(lhs: Expression, rhs: Expression) extends Expression
  case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
  case class Subtraction(lhs: Expression, rhs: Expression) extends Expression
  // Example
  val ex = Subtraction(Literal(1), Multiplication(Literal(5), Literal(3)))

  // Interpreter
  def eval(e: Expression): Int =
    e match {
      case Literal(n) => n
      case Addition(lhs, rhs) => eval(lhs) + eval(rhs)
      case Multiplication(lhs, rhs) =>  eval(lhs) * eval(rhs)
      case Subtraction(lhs, rhs) =>  eval(lhs) - eval(rhs)
    }
}

/* However, another way of adding subtraction is to treat it as syntactic sugar
 * using the fact that a - b = a + (-1 * b)
 *
 * One way of expressing the desugaring is as a syntax transformation: */

def desugarSMAE2MAE(e: SMAE.Expression) : MAE.Expression = e match {
  case SMAE.Literal(n) => MAE.Literal(n)
  case SMAE.Addition(lhs, rhs) => MAE.Addition(desugarSMAE2MAE(lhs), desugarSMAE2MAE(rhs))
  case SMAE.Multiplication(lhs, rhs) => MAE.Multiplication(desugarSMAE2MAE(lhs), desugarSMAE2MAE(rhs)) 
  case SMAE.Subtraction(lhs, rhs) => 
    MAE.Addition(desugarSMAE2MAE(lhs), 
                 MAE.Multiplication(MAE.Literal(-1),desugarSMAE2MAE(rhs)))
}

/* With this desugaring in place, we do not need an interpreter for SMAE anymore; rather
 *, we can reuse the MAE interpreter: */

val res = MAE.eval(desugarSMAE2MAE(SMAE.ex))

/* If we had written other algorithms on MAE, or had proven properties of MAE, they'd be applicable
 * to SMAE, too. Hence desugaring is a way of reusing code, proofs, ... .
 * It is important, though, that the desugared language feature is gone after desugaring.
 * For instance, a pretty printer would print the desugared code. A debugger would use
 * the desugared code. This can be an important downside to desugaring. There are ways
 * to avoid or mitigate these shortcomings, but they require additional work.
 *
 * There is a second way of realizing desugaring which does not require the definition of
 * a copy of the AST classes. We can desugar earlier, namely during the construction of the AST:
 */

object SMAE2 {
  // Abstract Syntax Tree
  sealed trait Expression
  case class Literal(n: Int) extends Expression
  case class Addition(lhs: Expression, rhs: Expression) extends Expression
  case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
  def subtraction(e1: Expression, e2: Expression) : Expression =
    Addition(e1, Multiplication(Literal(-1), e2))
  
  // Compared to SMAE, we only have to change upper case Subtraction by lower case subtraction
  // when constructing examples.
  val ex = subtraction(Literal(1), Multiplication(Literal(5), Literal(3)))

  // Interpreter - no case for subtraction needed
  def eval(e: Expression): Int =
    e match {
      case Literal(n) => n
      case Addition(lhs, rhs) => eval(lhs) + eval(rhs)
      case Multiplication(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}

/* Let us now consider the third extension, unary minus.
 * Here we have three choices:
 * i) Add unary minus to the core language
 * ii) Treat unary minus as syntactic sugar for the core language using  -x = (-1)*x
 * iii) Treat unary minus as syntactic sugar on top of the syntactic sugar using -x = 0 - x.
 *
 * We will use the third option to illustrate that one can build layers of syntactic sugar:
 */

object USMAE {
  // Abstract Syntax Tree
  sealed trait Expression
  case class Literal(n: Int) extends Expression
  case class Addition(lhs: Expression, rhs: Expression) extends Expression
  case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
  def subtraction(e1: Expression, e2: Expression) : Expression =
    Addition(e1, Multiplication(Literal(-1), e2))
  def unaryminus(e: Expression) = subtraction(Literal(0), e)
  
  // Compared to SMAE, we only have to change upper case Subtraction by lower case subtraction
  // when constructing examples.
  val ex = subtraction(unaryminus(Literal(1)), Multiplication(Literal(5), Literal(3)))

  // Interpreter - no case for subtraction needed
  def eval(e: Expression): Int =
    e match {
      case Literal(n) => n
      case Addition(lhs, rhs) => eval(lhs) + eval(rhs)
      case Multiplication(lhs, rhs) =>  eval(lhs) * eval(rhs)
    }
}
