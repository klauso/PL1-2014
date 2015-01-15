// Notes from the lab session on Monday, January 12

// First Part: Simple Examples
// ===========================

// Direct Style          Continuation-Passing Style
//
//  42                   k => k(42)
//
//  f(5)                 k => f_k(5, k)
//
//  f(g(5))              k => g_k(a => f_k(a, k))
//
//  if (f(5))            k => f_k(5, a => if (a)
//    g(1)                                  g_k(1, k)
//  else                                  else
//    27                                    k(27))

// Second Part: Less Simple Examples
// =================================

// Good old faculty function
// -------------------------

def fac(x: Int): Int =
  if (x < 1)
    1
  else
    x * fac(x - 1)

// if we treat <, - and * as trivial:
def fac_k[A](x: Int, k: Int => A): A =
  if (x < 1)
    k(1)
  else
    fac_k(x - 1, a =>
      k(x * a))

// Note about the type variable A: We can choose any type for A,
// as long as we choose the same type for both occurrences. In
// the lecture, Tillmann Rendel used Unit and Klaus Ostermann
// used Nothing. Here, we tell Scala that every type works by
// using a type variable.

// or, if we want to treat <, - and * as nontrivial:
def fac_k[A](x: Int, k: Int => A): A =
  <_k(x, 1, a =>
    if a
      k(1)
    else
      -_k(x, 1, b =>
        fac_k(b, c =>
          *_k(x, c, k)))

// The interpreter from 03_ae.scala
// --------------------------------

def eval(e: Exp, env: Env) : Int =
  e match {
    case Num(n) => n
    case Id(x) => env(x)
    case Add(l,r) => eval(l,env) + eval(r,env)
    case Mul(l,r) => eval(l,env) * eval(r,env)
  }

// if we leave environments as they are:
def eval_k(e: Exp, env: Env, k: Int => Nothing) : Nothing =
  e match {
    case Num(n) => k(n)
    case Id(x) => k(env(x))
    case Add(l,r) => eval_k(l,env, a => eval(r,env, b => k(a + b)))
    case Mul(l,r) => eval_k(l,env, a => eval(r,env, b => k(a * b)))
  }

// if we CPS-transform the environment lookup:
type Env_K = Map[Symbol, (Value => Nothing) => Nothing]

def eval_k(e: Exp, env_k: Env_K, k: Int => Nothing) : Nothing =
  e match {
    case Num(n) => k(n)
    case Id(x) => env_k(x, k)
    case Add(l,r) => eval_k(l, env, a => eval(r, env, b => k(a + b)))
    case Mul(l,r) => eval_k(l, env, a => eval(r, env, b => k(a * b)))
  }

// Homework Assignments
// ====================
//
// Email homework as Scala source file to:
//
//   rendel@informatik.uni-tuebingen.de
//
// Work in groups of 1 or 2 students. Send the email CC to the
// other student in your team.
//
// Put "pl1-hw06" in subject, please
//
//  1. Transform the expression f(f(3) + f(4)) into CPS
//  2. Transform the following program into CPS:

def all(f: Int => Boolean, list: List[Int]): Boolean =
  if (list.isEmpty) {
    true
  } else {
    f(list.head) && all(f, list.tail)
  }

val even = (number: Int) => number % 2 == 0

assert(all(even, List(2, 4, 6, 8, 10)))
assert(!(all(even, List(2, 4, 7, 8, 10))))

// 3. Change eval_k to short-circuit multiplications
//    if the first factor is 0.
//
// About the last task: For example, in the program
//
//   Mul(Num(0), Mul(Num(1), Num(2)))
//
// we don't have to compute * (1 + 2) because we already
// know that 0 times whatever will be 0. So we can "jump
// over" this extra computation, returning 0 directly.
//
// For another example, consider this program:
//
//   Add(Mul(Add(Num(-2), Num(2)), Add(Num(1), Num(2))), Num(5))
//
// We have to compute -2 + 2 = 0, then we can jump over * (1 + 2)
// to directly compute 0 + 5 = 5.
//
// Hint for the last task: Use a second continuation that is
// called whenever the result is 0:

def eval_clever(e, env, k_nonzero, k_zero) = ...
