/**
This file contains the source code from the lectures on January
20 and 22.

Monads
======

A common interface for all monads:
*/

trait Monad {
  type M[_]
  def unit[A](a: A): M[A]
  def bind[A, B](p: M[A], f: A => M[B]): M[B]

  implicit class monadicSyntax[A](p: M[A]) {
    def flatMap[B](f: A => M[B]) = bind(p, f)
    def map[B](f: A => B) = flatMap(x => unit(f(x)))
  }
}

/**
The identity monad implements the Monad interface but doesn't
provide any additional features:
*/

trait IdentityMonad extends Monad {
  type M[X] = X
  def unit[A](a: A): M[A] = a
  def bind[A, B](p: M[A], f: A => M[B]): M[B] = f(p)
}

/**
Failure and recovery from failure
=================================

Interface for monads that support failure and recovering from
failure:
*/

trait Failure extends Monad {
  def fail[A]: M[A]
  def recover[A](p: M[A], q: M[A]): M[A]
}

/**
The Option monad captures the essence of the following
programming style:

    action1(...) match {
      case None => None
      case Some(result1) => action2(...) match {
        case None => None
        case Some(result2) => ...

This supports failure (by returning None) and recovery from
failure (by matching on None and trying something else).
*/

trait OptionMonad extends Monad with Failure {
  type M[X] = Option[X]
  def unit[A](a: A): M[A] =
    Some(a)
  def bind[A, B](p: M[A], f: A => M[B]): M[B] =
    p match {
      case None => None
      case Some(a) => f(a)
    }
  def fail[A]: M[A] =
    None
  def recover[A](p: M[A], q: M[A]): M[A] =
    p match {
      case None => q
      case Some(a) => Some(a)
    }
}

/**
The list monad captures the essence of programming with loops
over lists. This supports failure (by returning an empty list)
and recovery from failure (by trying with the next element from a
list).

*/

trait ListMonad extends Monad with Failure {
  type M[X] = List[X]
  def unit[A](a: A): M[A] =
    List(a)
  def bind[A, B](p: M[A], f: A => M[B]): M[B] =
    p match {
      case List() => Nil
      case a +: as => f(a) ++ bind(as, f)
    }
  def fail[A]: M[A] =
    Nil
  def recover[A](p: M[A], q: M[A]): M[A] =
    p ++ q
}

/**
Accessing information and changing it for subcomputations
=========================================================

Interface for monads that support accessing some information and
adapting the information for subcomputations:
*/

trait Reader extends Monad {
  type R
  def ask: M[R]
  def local[A](f: R => R, p: M[A]): M[A]
}

/**
The Reader monad captures the essence of the following
programming style:

  r => action1(r) + action2(r)

This supports accessing the information in r and adapting the
information for subcomputations.
*/

trait ReaderMonad extends Monad with Reader {
  type M[X] = R => X
  def unit[A](a: A): M[A] =
    r => a
  def bind[A, B](p: M[A], f: A => M[B]): M[B] =
    r => f(p(r))(r)
  def ask: M[R] =
    r => r
  def local[A](f: R => R, p: M[A]): M[A] =
    r => p(f(r))
}

/**
Accessing information and adapting it for the remainder of the computation
==========================================================================
*/

trait State extends Monad {
  type S
  def get: M[S]
  def put(s: S): M[Unit]
}

/**
The State monad captures the essence of the following
programming style:

  s0 => action1(s0, ...) match {
    case (result1, s1) => action2(s1, ...) match {
      case (result2, s2) => ...

This supports accessing the information in s and adapting the
information for the remainder of the computation.
*/

trait StateMonad extends Monad with State {
  type M[X] = S => (X, S)
  def unit[A](a: A): M[A] =
    s => (a, s)
  def bind[A, B](p: M[A], f: A => M[B]): M[B] =
    s0 => p(s0) match {
      case (a, s1) => f(a)(s1)
    }
  def get: M[S] =
    s0 => (s0, s0)
  def put(s1: S): M[Unit] =
    s0 => (Unit, s1)
}

/**
State vs. Reader
================

We can implement the reader monad in terms of the state monad:
*/

trait StateAsReader extends Reader { this : Monad with State =>
  type R = S

  def ask: M[R] =
    get

  def local[A](f: R => R, p: M[A]): M[A] =
    for {
      s <- get
      _ <- put(f(s))
      result <- p
      _ <- put(s)
    } yield result
}

/**
Continuations
=============

Interface for monads that support capturing the current
continuation:
*/

trait Continuation extends Monad {
  type Result
  def callcc[A, B](f: (A => M[B]) => M[A]): M[A]
}

/**
The Continuation monad captures programming in
continuation-passing style.
*/
  
trait ContinuationMonad extends Monad with Continuation {
  type M[X] = (X => Result) => Result
  def unit[A](a: A): M[A] =
    k => k(a)
  def bind[A, B](p: M[A], f: A => M[B]): M[B] =
    k => p(a => f(a)(k))
  def callcc[A, B](f: (A => M[B]) => M[A]): M[A] =
    k => f(a => _ => k(a))(k)
}

/**
Monadic Interpreter for First-Class Continuations
=================================================

For today, we assume that we have a monad that supports Reader
(to manage an environment) and Continuation (to access the
current continuation). This allows us to rewrite the interpreter
from 17-firstclasscontinuations.scala as follows:

*/

trait Example extends Monad with Reader with Continuation {
  sealed abstract class Exp
  case class Num(n: Int) extends Exp
  case class Id(name: Symbol) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Fun(param: Symbol, body: Exp) extends Exp
  case class App (funExpr: Exp, argExpr: Exp) extends Exp
  case class Letcc(param: Symbol, body: Exp) extends Exp

  implicit def num2exp(n: Int) = Num(n)
  implicit def id2exp(s: Symbol) = Id(s)

  type R = Map[Symbol, Value]

  sealed abstract class Value
  case class NumV(n: Int) extends Value
  case class ClosureV(f: Fun, env: R) extends Value
  case class ContV(k: Value => M[Value]) extends Value

  def eval(e: Exp) : M[Value] = e match {
    case Num(n: Int) =>
      unit(NumV(n))

    case Id(x) => for {
      env <- ask
    } yield env(x)

    case Add(l, r) => for {
      lv <- eval(l)
      rv <- eval(r)
    } yield (lv, rv) match {
      case (NumV(v1), NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }

    case f@Fun(param, body) => for {
      env <- ask
    } yield ClosureV(f, env)

    case App(f, a) => for {
      fv <- eval(f)
      av <- eval(a)
      result <- fv match {
        case ClosureV(Fun(param, body), env) =>
          local(_ => env + (param -> av), eval(body))
        case ContV(k) =>
          k(av)
        case _ => sys.error("can only apply functions or continuations")
      }
    } yield result

    case Letcc(param, body) =>
      callcc((k: Value => M[Value]) =>
        local(env => env + (param -> ContV(k)),
          eval(body)))
  }
}

/**
The next step will be to figure out how to compose monads. For
this example, we would like to compose the implementation of the
Reader monad with the implementation of the Continuation
monad. For interpreters for bigger languages, we need to compose
even more monads.
*/
