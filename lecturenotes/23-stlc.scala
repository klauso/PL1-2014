/**
The Simply-Typed Lambda Calculus
================================

We start with the untyped substitution-based lambda calculus augmented by the possibility to add type annotations to function definitions. The type annotation is ignored by the interpreter.
*/

sealed abstract class Type

sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)
case class Fun(param: Symbol, t: Type, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp
case class Bool(b: Boolean) extends Exp

def freshName(names: Set[Symbol], default: Symbol) : Symbol = {
  var last : Int = 0
  var freshName = default  
  while (names contains freshName) { freshName = Symbol(default.name+last.toString); last += 1; }
  freshName
}

def freeVars(e: Exp) : Set[Symbol] =  e match {
   case Id(x) => Set(x)
   case Add(l,r) => freeVars(l) ++ freeVars(r)
   case Fun(x,_,body) => freeVars(body) - x
   case App(f,a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
   case Bool(b) => Set.empty
}

def subst(e1 : Exp, x: Symbol, e2: Exp) : Exp = e1 match {
  case Num(n) => e1
  case Bool(x) => e1
  case Add(l,r) => Add(subst(l,x,e2), subst(r,x,e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case App(f,a) => App(subst(f,x,e2),subst(a,x,e2))
  case Fun(param,t,body) => 
    if (param == x) e1 else {
      val fvs = freeVars(body) ++ freeVars(e2)
      val newvar = freshName(fvs, param)
      Fun(newvar, t, subst(subst(body, param, Id(newvar)), x, e2))
    }                            
}

def eval(e: Exp) : Exp = e match {
  case Id(v) => sys.error("unbound identifier: " + v.name)
  case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case App(f,a) => eval(f) match {
     case Fun(x,_,body) => eval( subst(body,x, eval(a)))  // call-by-value
     case _ => sys.error("can only apply functions")
  }
  case _ => e // numbers and functions evaluate to themselves
}

/**
We classify values into three types: Booleans, integers, and function types. For function types, we need some abstraction for its input and output; otherwise the type checker cannot be compositional. Luckily we do already have such an abstraction, namely types. Hence ``Funtype`` becomes a recursive data type.
*/
case class BoolType() extends Type
case class IntType() extends Type
case class FunType(from: Type, to: Type) extends Type

/** 
The type checker for the so-called _Simply-Typed Lambda Calculus_  (STLC). To deal with identifiers, we need an abstraction of environments. Environments have a type like ``Map[Symbol,Value]``. A _type environment_ (traditionally called gamma) classifies environments point-wise.  A type environment has the form ``Map[Symbol,Type]``. We say that an environment ``env`` has type ``gamma``, if for all ``x`` on which ``gamma`` is defined, ``typeCheck(env(x),Map.empty) == gamma(x)``.

The type checker for the STLC is as follows:
*/

def typeCheck(e: Exp, gamma: Map[Symbol,Type]) : Type = e match {
  case Num(n) => IntType()
  case Bool(x) => BoolType()
  case Add(a,b) => (typeCheck(a,gamma),typeCheck(b,gamma)) match {
    case (IntType(),IntType()) => IntType()
    case _ => sys.error("Type Error in addition")
  }
  case Id(x) => gamma.get(x) match {
    case Some(t) => t
    case _ => sys.error("free variable: " ++ x.toString)
   } 
  case App(f,a) => typeCheck(f,gamma) match {
    case FunType(from,to) =>  
      if (from == typeCheck(a,gamma)) to 
        else sys.error(from.toString ++ " expected, found: " ++ typeCheck(a,gamma).toString)
    case t => sys.error("Type error: function expected, found: " ++ t.toString)
  }
  case Fun(x,t,body) => FunType(t,typeCheck(body,gamma + (x -> t)))
}

/**
Soundness 
=========
If ``e: Exp`` und ``gamma : Map[Symbol,Type]`` and ``typeCheck(e, gamma) == T``, then:
  If``env: Map[Symbol,Exp]`` such that ``gamma`` is the type of ``env``,  then ``eval(e,env) == v`` and ``typeCheck(v,Map.empty) == T``.

In particular, the STLC has the special and non-trivial property that all well-typed programs terminate.
*/












