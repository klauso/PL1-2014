/**
Automatic Transformation into CPS
=================================

In the previous lecture, we learned about continuation-passing
style (CPS), how it allows to treat execution order as a
first-class concept, and how to manually transform a program into
it. Today, we want to write an automatic transformation from an
FAE program in direct style into the equivalent program in
continuation-passing style. On the way, we will also learn how to
use the Scala type system to help us encode the specification of
a transformation like this.
*/

object transform {

  /**
  Precise typing
  --------------

  The transformation will be a Scala method that pattern matches
  on FAE expressions, and creates other FAE expressions. The
  signature of that method could be:

      def cps(exp: Exp): Exp

  Of course, the idea would be that `cps` always returns
  expressions in continuation-passing style. To ensure that we
  get the implementation of `cps` right, we would have to write a
  test suite, or make a proof, or at least think hard about
  whether our implement is correct. Instead of going this route,
  we are going to define a set of case classes that exactly
  encode FAE programs in CPS but no other FAE programs. We can
  then use a signature such as the following for the
  transformation:

      def cps(exp: Exp): ContExp

  The benefit will be that when we implement `cps`, the Scala
  typechecker will ensure that we actually return an expression
  in CPS, because all values of the type `ContExp` represent
  expressions in CPS.

  Source, target, and meta language
  ---------------------------------

  When we think about a transformation between languages, it
  helps to use different words for the different languages
  involved:

    - The *source language* is the language the input to the
      transformation is written in. In the lecture today, the
      source language is FAE.

    - The *target language* is the language the output of the
      transformation is written in. In the lecture today, the
      target language is FAE in continuation-passing style.

    - The *meta language* is the language the transformation
      itself is written in. In the lecture today, the meta
      language is Scala.

  The source language
  -------------------

  Our source language is simply FAE, as defined in a previous
  lecture. We copy the relevant definitions here:
  */

  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: Symbol) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  implicit def num2exp(n: Int) = Num(n)
  implicit def id2exp(s: Symbol) = Id(s)
   
  case class Fun(param: Symbol, body: Exp) extends Exp
  case class App (funExpr: Exp, argExpr: Exp) extends Exp

  /**
  Target language
  ---------------

  The target language should be the subset of the source language
  that is in continuation-passing style, that is, the subset
  where programs have the three properties from the previous
  lecture:

    1. All calls are in tail position.
    2. All functions take continuation arguments.
    3. No function returns a value.

  To be able to formalize these properties in the Scala
  typesystem, we reformulate them using the terms "non-trivial
  expression" and "trivial expression" that we already used
  informally in the previous lecture.  Now it is time to define
  them more formally:

    - An expression is *non-trivial* if evaluating it might take
      a long time, multiple steps, or might not
      terminate. Example: Function calls.

    - An expression is *trivial* if evaluating it is sure to be
      instantaneous and always succeeds. Example: Integer
      literals.

  In CPS, trivial subexpressions will be left alone. In
  particular, they will still return values. But every nontrivial
  subexpressions needs to be changed so that it accepts and calls
  a continuation instead of returning a value. So we can
  reformulate the three properties of programs in
  continuation-passing style as follows:

   1. All non-trivial expressions are in tail position.
   2. All functions take continuation arguments.
   3. No trivial expressions are in tail position.

  Note how the third property used to be semantic (what happens
  at run time) and became syntactic (how programs look like). The
  two versions of the third property still mean the same thing:
  Only trivial expressions evaluate to values, but they cannot
  appear in tail position, so the last thing a function does can
  never be to return a value. The good thing about this more
  syntactic reformulation is that we can encode it into the
  syntax of the target language, that is, into the case classes
  that we use to represent target language programs.

  Trivial and nontrivial expression in the target language
  --------------------------------------------------------

  We start by saying that an expression in continuation-passing
  style is either trivial or nontrivial:
  */

  sealed abstract class ContExp
  sealed abstract class TrivialContExp extends ContExp
  sealed abstract class NontrivialContExp extends ContExp

  /**
  No we go through the five case classes of FAE and decide
  whether they are trivial or nontrivial, and what there
  subexpressions are.

  We start with numeric literals. They are the paradigmatic
  example of trivial expressions, so we let `ContNum` extend
  `TrivialContExp`:
  */

  case class ContNum(n: Int) extends TrivialContExp

  /**
  Next we look at identifier occurrences. Since we are in a
  call-by-value setting, evaluating an identifier is
  instantaneous and always terminates. We therefore classify
  identifier occurrences as trivial. In a call-by-name or
  call-by-need setting, we would have to treat them as nontrivial
  instead!
  */

  case class ContId(name: Symbol) extends TrivialContExp

  /**
  Now we consider addition. Evaluating an addition expression
  only takes a single step, so we treat them as trivial and let
  `ContAdd` extend `TrivialContExp`.

  Evaluation of `ContAdd` will work by evaluating the
  subexpressions first, and then doing the addition. So neither
  of the subexpressions is in tail positions, so they have to be
  trivial, by the third property of programs in CPS. We therefore
  use `TrivialContExp` for both `lhs` and `rhs`.
  */

  case class ContAdd(lhs: TrivialContExp, rhs: TrivialContExp)
      extends TrivialContExp

  // can also decide to ContAdd(...) extends NontrivialContExp
  // TODO explain how to deal with nontrivial adds and 2nd req.

  /**
  The usual implicit conversions, for convenience:
  */

  implicit def num2contexp(n: Int) = ContNum(n)
  implicit def id2contexp(s: Symbol) = ContId(s)

  /**
  Next we have to think about application. Evaluating an
  application means evaluating the subexpressions first and then
  calling the function. So the subexpressions are not in tail
  position and therefore have to be trivial by the third property
  of programs in CPS. We therefore use `TrivalContExp` for
  `funExpr` and `argExpr`.

  By the second property of programs in CPS, all functions take
  continuations as additional argument. So we also have to
  provide a continuation in every function call. To ensure we
  don't forget these continuation arguments, we add a field of
  type `Continuation` to the `ContApp` case class. The type
  `Continuation` will be defined below.

  Finally, evaluating an application might take multiple steps or
  a long term or might not even terminate, depending on what
  happens inside the called function. We therefore classify
  applications as nontrivial, and let `ContApp` extend
  `NontrivialContExp`.
  */

  case class ContApp(funExpr: TrivialContExp,
                     argExpr: TrivialContExp,
                     cont: Continuation)
      extends NontrivialContExp


  /**
  Finally, we think about functions. The body of a function is in
  tail position, so it has to be nontrivial by the third property
  of programs in CPS. Accordingly, we choose `body:
  NontrivialContExp`.

  By the second property of programs in CPS, all functions take
  continuations as additional argument. We add a field of type
  `Symbol` to hold the name of the continuation parameter.

  Function expressions itself are instantaneously evaluated to
  closures, so they are trivial, and we let `ContFun` extend
  `TrivialContExp`.
  */

  case class ContFun(param: Symbol,
                     contParam: Symbol,
                     body: NontrivialContExp)
      extends TrivialContExp


  /**
  In the examples above, subexpressions were usually required to
  be trivial. But sometimes, subexpressions can be required to be
  nontrivial, for example:

      case class If(condExpr: TrivialContExp,
                    thenBranch: NontrivialContExp,
                    elseBranch: NontrivialContExp)
        extends NontrivialContExp

  By the first and third property of programs in CPS, whether a
  subexpression needs to trivial or nontrivial depends on whether
  it is in tail position or not.

  Continuations in the target language
  ------------------------------------

  Now in addition to trivial and nontrivial expressions, we also
  need continuations in our target language. We already used the
  type `Continuation` once above, in the type of a field of
  `ContApp`.
  */

  sealed abstract class Continuation extends ContExp

  /**
  There are two kinds of continuations in the target language:
  Names of continuation arguments, and anonymous continuations
  constructed by function expressions. The body of an anonymous
  continuation has to be a nontrivial expression, since it is in
  tail position.
  */

  case class FunContinuation(
    param: Symbol,
    body: NontrivialContExp)
      extends Continuation

  case class IdContinuation(
    name: Symbol)
      extends Continuation

  implicit def id2cont(name: Symbol) = IdContinuation(name)

  /**
  Finally, we also need to be able to *call* a
  continuation. Since we don't know what the continuation will
  do, calling a continuation is a nontrivial expression. And
  since we first evaluate the continuation's argument and then
  call the continuation, the argument is not in tail position and
  therefore needs to be a trivial expression.
  */

  case class AppContinuation(
    cont: Continuation,
    arg: TrivialContExp)
      extends NontrivialContExp

  /**
  Fresh names
  -----------

  To avoid name clashes, we need to generate fresh names all the
  time.
  */

  var counter: Int = 0
  def freshName(name: String) = {
    counter += 1
    Symbol(name + "_" + counter)
  }

  /**
  The Transformation
  ------------------

  Ok, now we can write a transformation of arbitrary
  expressions into non-trivial expressions in CPS.
  */

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

  /**
  Note how the Scala type checker helps us get this right by
  ensuring that we don't forget any continuation arguments or mix
  up trivial and nontrivial expressions.
  */
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
