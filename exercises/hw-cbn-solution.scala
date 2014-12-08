// Here is some code from 06-eval.scala:

sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)

case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp

case class LazyFun(param: Symbol, body: Exp) extends Exp

def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)

val test = App( Fun('x,Add('x,5)), 7)
val test2 = wth('x, 5, App(Fun('f, App('f,3)), Fun('y,Add('x,'y))))

object Recursive {

sealed abstract class Value
trait NotThunk extends Value
type Env = Map[Symbol, Value]

case class NumV(n: Int) extends NotThunk
case class ClosureV(f: Fun, env: Env) extends NotThunk
case class LazyClosureV(f: LazyFun, env: Env) extends NotThunk
case class ThunkV(e: Exp, env: Env) extends Value {
  // var cache : NotThunk = null
  lazy val cache = evalWithEnv(e, env)
}

// fully forces the value
def force(v: Value): NotThunk =
  v match {
    case thunk@ThunkV(e, env) =>
      // if (thunk.cache == null) {
      //   val result = force(evalWithEnv(e, env))
      //   thunk.cache = result
      //   result
      // } else {
      //   thunk.cache
      // }
      thunk.cache
    case v : NotThunk => v
  }

def evalWithEnv(e: Exp, env: Env) : NotThunk = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => force(env(x))
  case Add(l,r) => {
    (evalWithEnv(l,env), evalWithEnv(r,env)) match {
      case (NumV(v1),NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param,body) => ClosureV(f, env)
  case f@LazyFun(param, body) => LazyClosureV(f, env)
  case App(f,a) => evalWithEnv(f,env) match {
    // Use environment stored in closure to realize proper lexical scoping!
    case ClosureV(f,closureEnv) =>
      evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a,env)))
    case LazyClosureV(f,closureEnv) =>
      evalWithEnv(f.body, closureEnv + (f.param -> ThunkV(a, env)))
    case _ => sys.error("can only apply functions, found " + f)
  }
}
}

import Recursive._

val test3 = App(LazyFun('x, 42), App(5, 4))
val test4 = App(LazyFun('x, 'x), 42)
val test5 = wth('y, 42, App(LazyFun('x, wth('y, 27, 'x)), 'y))

assert( evalWithEnv(test, Map.empty) == NumV(12))
assert( evalWithEnv(test2,Map.empty) == NumV(8))
assert( evalWithEnv(test3, Map.empty) == NumV(42))

assert( evalWithEnv(test4, Map.empty) == NumV(42))

assert( evalWithEnv(test5, Map.empty) == NumV(42))

// HOMEWORK ASSIGNMENT
// ===================
//
// Email homework as Scala source file to:
//
// rendel@informatik.uni-tuebingen.de
//
// Work in groups of 1 or 2 students. Send the email CC to the
// other student in your team. Hand in before the morning of
// Monday, December 8.
//
//
// Put "pl1-hw04" in subject, please
//
// 0. write in the email:
// - your names
// - your student ids ("Matrikelnummer")
// 1. Add a case class LazyFun and extend the interpreter so that
//    LazyFun creates function with call-by-need evaluation order.
//
// About task 1: This is similar to how Scala has functions with
// and without the => annotation for call-by-name.
//
// Send question by email to rendel@informatik.uni-tuebingen.de
