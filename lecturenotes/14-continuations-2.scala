object transform {

  // Let's encode the three properties of programs in CPS in the
  // Scala type system. This will help us to get the
  // transformation right.

  // Arbitrary expressions:

  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: Symbol) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  implicit def num2exp(n: Int) = Num(n)
  implicit def id2exp(s: Symbol) = Id(s)
   
  case class Fun(param: Symbol, body: Exp) extends Exp
  case class App (funExpr: Exp, argExpr: Exp) extends Exp

  // Expressions in CPS:

  sealed abstract class ContExp
  sealed abstract class TrivialContExp extends ContExp
  sealed abstract class NontrivialContExp extends ContExp

  case class ContNum(n: Int) extends TrivialContExp

  case class ContId(name: Symbol) extends TrivialContExp
  // ContId would be NontrivialContExp if call-by-name

  case class ContAdd(lhs: TrivialContExp, rhs: TrivialContExp)
      extends TrivialContExp
  // can also decide to ContAdd(...) extends NontrivialContExp
  // TODO explain how to deal with nontrivial adds and 2nd req.

  // lhs and rhs *have to be* TrivialContExp because they are
  // not in tail position!

  implicit def num2contexp(n: Int) = ContNum(n)
  implicit def id2contexp(s: Symbol) = ContId(s)
   
  case class ContApp(funExpr: TrivialContExp,
                     argExpr: TrivialContExp,
                     cont: Continuation)
      extends NontrivialContExp

  // funExpr and argExpr *have to be* trivial because they
  // are not in tail position

  case class ContFun(param: Symbol,
                     contParam: Symbol,
                     body: NontrivialContExp)
      extends TrivialContExp

  // sometimes subexpressions can be nontrivial, for example:
  //
  // case class If(condExpr: trivial,
  //               thenBranch: nontrivial,
  //               elseBranch: nontrivial)
  //   extends Nontrivial

  // We also need continuations!

  sealed abstract class Continuation extends ContExp

  case class FunContinuation(
    param: Symbol,
    body: NontrivialContExp)
      extends Continuation

  case class IdContinuation(
    name: Symbol)
      extends Continuation

  implicit def id2cont(name: Symbol) = IdContinuation(name)

  case class AppContinuation(
    cont: Continuation,
    arg: TrivialContExp)
      extends NontrivialContExp

  // We need to generate fresh names
  var counter: Int = 0
  def freshName(name: String) = {
    counter += 1
    Symbol(name + "_" + counter)
  }

  // Ok, now we can write a transformation of arbitrary
  // expressions into non-trivial expressions in CPS.

  def cps(e: Exp, k: Continuation): NontrivialContExp =
    e match {
      case Num(n) => AppContinuation(k, ContNum(n))
      case Id(name) => AppContinuation(k, ContId(name))
      case Add(lhs, rhs) => {
        val x = freshName("x")
        val y = freshName("y")

        cps(lhs, FunContinuation(x,
          cps(rhs, FunContinuation(y,
            AppContinuation(k, ContAdd(x, y))))))
      }

      case Fun(param, body) =>
        AppContinuation(k,
          ContFun(param, 'dynk, cps(body, 'dynk)))

        // in the function body, we have two continuations:
        //
        // k: continuation from where the lambda is
        //    (static continuation)
        //
        // dynk: continuation from where the application is
        //       (dynamic continuation)

      case App (funExpr, argExpr) =>
        cps(funExpr, FunContinuation('f,
          cps(argExpr, FunContinuation('a,
            ContApp('f, 'a, k)))))
    }
}

import transform._

// Experiments show that this transformation works, but also that
// it creates unnecessary expressions of the form:
//
//   AppContinuation(FunContinuation(name, body), value)
//
// 1. These expressions are called "administrative redexes".
//
// 2. We could write the administrative redexes as
//
//      wthContinuation(name, value, body)
//
//    with wthContinuation defined like wth from an earlier lecture.
//
// 3. The AppContinuation part of the administrative redexes are
//    created in the cases for expressions that are already
//    trivial (Num, Id, and Fun).
//
// 4. The FunContinuation part of the administrative redexes are
//    created in the recursive calls of cps.
//
// => The administrative redexes are created because cps always
//    creates a nontrivial expression in CPS, but sometimes a
//    trivial expression in CPS would be shorter and more
//    appropriate.
