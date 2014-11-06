// Notes from the 2nd exercise session of the course
// "Programming Languages I" at Tübingen University.
// Winter term 2014/2015.
//
// Alternatives to Pattern Matching
// ================================
//
// Last week, we have seen this code with pattern matching:

object UsingPatternMatching {
  // Abstract Syntax Tree
  trait Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp

  // Example
  val onePlusEight = Add(Num(1), Add(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(l, r) => eval(l) + eval(r)
    }

  // Test
  def test {
    print(eval(onePlusEight))
  }
}

// Here are some of the ways to write this code without pattern
// matching.
//
// Using isInstanceOf and asInstanceOf
// -----------------------------------
//
// Instead of pattern matching, we could also use isInstanceOf
// and asInstanceOf to distinguish Num and Add nodes in the
// abstract syntax tree. In fact, the Scala compiler will desugar
// the code that uses pattern matching into something similar to
// the code below:

object UsingInstanceOf {
  // Abstract Syntax Tree
  trait Exp
  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp

   // Example
  val onePlusEight = Add(Num(1), Add(Num(5), Num(3)))

  // Interpreter
  def eval(e: Exp): Int =
    if (e.isInstanceOf[Num])
      e.asInstanceOf[Num].n
    else if (e.isInstanceOf[Add])
      eval(e.asInstanceOf[Add].lhs) + eval(e.asInstanceOf[Add].rhs)
    else
      sys.error("unknown expression")

  def test {
    print(eval(onePlusEight))
  }
}

// In Scala, you should never write such code by hand. Instead, use
// pattern matching which gets desugared to something similar
// anyway.
//
// Using Dynamic Dispatch
// ----------------------
//
// Here is a solution that uses dynamic dispatch:

object UsingDynamicDispatch {
  // Abstract Syntax Tree
  trait Exp {
    def eval: Int
  }

  case class Num(n: Int) extends Exp {
    def eval: Int = n
  }

  case class Add(lhs: Exp, rhs: Exp) extends Exp {
    def eval: Int = lhs.eval + rhs.eval
  }

  // Example
  val onePlusEight = Add(Num(1), Add(Num(5), Num(3)))

  // Test
  def test {
    print(onePlusEight.eval)
  }
}

// When we talked about this code, we have seen a diagram for the
// Composite pattern on the blackboard:
//
//        +-------+
//        |  Exp  |  2
//        +-------+ <------+
//        | eval  |        |
//        | ...   |
//        +-------+        |
//            ^            |
//           /_\           |
//            |            |
//       +----+----+       |
//       |         |       |
//   +-------+ +-------+   |
//   |  Num  | |  Add  |   |
//   +-------+ +-------+ --+
//   | eval  | | eval  |
//   | ...   | | ...   |
//   +-------+ +-------+
//
// Note: If we want to add more operations beside eval, we have
// to add them to all of the classes.
//
// In this lecture, we will work a lot with abstract syntax
// trees, so Composite-like structures will be important.

// Using the Visitor Pattern
// -------------------------
//
// And finally a solution that uses visitors:

object UsingVisitors {
  // Visitor interface
  // (one method per case class!)
  trait Visitor[Value] {
    def num(n: Int): Value
    def add(lhs: Value, rhs: Value): Value
  }

  // Abstract Syntax Tree & Folding the Visitor
  trait Exp {
    def foldExp[Value](v: Visitor[Value]): Value
  }

  case class Num(n: Int) extends Exp {
    def foldExp[Value](v: Visitor[Value]): Value =
      v.num(n)
  }

  case class Add(lhs: Exp, rhs: Exp) extends Exp {
    def foldExp[Value](v: Visitor[Value]): Value =
      v.add(lhs.foldExp(v), rhs.foldExp(v))
  }

  // Evaluation
  object Eval extends Visitor[Int] {
    def num(n: Int): Int = n
    def add(lhs: Int, rhs: Int): Int = lhs + rhs
  }

  // Example
  val onePlusEight = Add(Num(1), Add(Num(5), Num(3)))

  // Test
  def test {
    print(onePlusEight.foldExp(Eval))
  }
}

// Compare this with the visitor code at the end of
// 03-ae.scala. There:
//
//  - a visitor is a case class that stores functions
//  - concrete visitors are values of that case class
//  - foldExp is implemented with pattern matching
//
// Here:
//
//  - a visitor is an interface with abstract methods
//  - concrete visitors are instances of that interface
//  - foldExp is implemented with dynamic dispatch
//
// These details differ, but both versions are implementations of
// the visitor pattern.
//
// When we talked about this code, we have seen a diagram for the
// Visitor pattern on the blackboard:
//
//             +---------------+                 +-----------+
//             |      Exp      |  2              |  Visitor  |
//             +---------------+ <---------+     +-----------+
//             | fold(Visitor) |           |     | num       |
//             +---------------+           |     | add       |
//                     ^                   |     +-----------+
//                    /_\                  |           ^
//                     |                   |          /_\
//            +--------+-------+           |           |
//            |                |           |      +----+----+
//   +---------------+ +---------------+   |      |         |
//   |  Num          | |  Add          |   |  +-------+ +-------+
//   +---------------+ +---------------+ --+  |  Eval | |  ...  |
//   | fold(Visitor) | | fold(Visitor) |      +-------+ +-------+
//   +---------------+ +---------------+      | num   | | num   |
//                                            | add   | | add   |
//                                            +-------+ +-------+
//
// Note that if we want to add more operations beside Eval, we
// can just add more classes. We don't have to change the
// existing code. This is one of the benefits of the Visitor
// pattern.
//
// For the examples in this lecture, we'll mostly just use
// pattern matching. When you implement a language in the context
// of a bigger sotware project, you might want to look into the
// Visitor pattern and other approaches again.
//
// HOMEWORK ASSIGNMENT
// ===================

// HOMEWORK
//
// Email homework as Scala source file to:
//
//   rendel@informatik.uni-tuebingen.de
//
// Work in groups of 1 or 2 students. Send the email CC to the
// other student in your team.
// 
// Put "pl1-hw02" in subject, please
//
// 0. write in the email:
//      - your names
//      - your student ids ("Matrikelnummer")
// 1. implement a pretty printer for the language with Add and Num.
//
// The pretty printer should turn an Exp into a String. It should
// only add parentheses where necessary.
//
// Examples:
//
//   Num(1)                            ==>  "1"
//   Add(Num(1), Num(2))               ==>  "1 + 2"
//   Add(Num(1), Add(Num(2), Num(3)))  ==>  "1 + (2 + 3)"
//   Add(Add(Num(1), Num(2)), Num(3))  ==>  "1 + 2 + 3"
//
// No need for parentheses to the left of Add, because by
// default, Add is executed left-to-right.
//
// If you want, you can experiment with implementing the pretty
// printer with pattern matching, with dynamic dispatch, and as a
// visitor.
//
// Send question by email to rendel@informatik.uni-tuebingen.de
