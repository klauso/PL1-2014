// an implementation of the macro expansion algorithm as described in the 1986 paper
// on "Hygienic macro expansion" by Kohlbecker et al.

abstract class Var
case class SimpleVar(name: Symbol) extends Var {
  override def toString = name.name }
case class TSVar(name: Symbol, ts: Int) extends Var {
  override def toString = name.name + ":" + ts.toString }

abstract class Exp 
case class Num(n: Int) extends Exp
case class Id(name: Var) extends Exp {
  override def toString = name.toString }
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Fun(param: Var, body: Exp) extends Exp {
  override def toString = "(lambda " + param.toString + " "+body.toString + ")"}
case class App (funExpr: Exp, argExpr: Exp) extends Exp {
  override def toString = "(" + funExpr.toString + " " + argExpr.toString + ")"}
def wth(x: Var, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)
abstract class MacroExp extends Exp
case class MacroTok(name: Symbol) extends MacroExp
case class MacroApp(name: Symbol, args: List[Exp]) extends MacroExp {
  override def toString = "(" + name.name + " " + args.mkString(" ") + ")"}
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(SimpleVar(s))
implicit def id2var(s: Symbol) = SimpleVar(s)

def addTimeStamp(ts: Int, e: Exp) : Exp = e match {
    case Id(SimpleVar(n)) => Id(TSVar(n,ts))
    case Add(l,r) => Add(addTimeStamp(ts,l), addTimeStamp(ts,r))
    case Fun(SimpleVar(p),b) => Fun(TSVar(p,ts),addTimeStamp(ts,b)) 
    case Fun(p,b) => Fun(p,addTimeStamp(ts,b)) 
    case App(f,a) => App(addTimeStamp(ts,f),addTimeStamp(ts,a))
    case MacroApp(name, args) => MacroApp(name, args.map(addTimeStamp(ts,_)))
    case _ => e 
}

def evMacro(e : Exp, theta : MacroApp => Exp, j: Int) : Exp = e match {
  case Add(l,r) => Add(evMacro(l,theta,j),evMacro(r,theta,j))
  case Fun(param,body) => Fun(param, evMacro(body,theta,j))
  case App(f,a) => App(evMacro(f,theta,j),evMacro(a,theta,j))
  case MacroApp(name, args) => evMacro(addTimeStamp(j,theta(MacroApp(name,args))), theta, j+1)
  case _ => e
}

def freshName(names: Set[Symbol], default: Symbol) : Symbol = {
  var last : Int = 0
  var freshName = default  
  while (names contains freshName) { freshName = Symbol(default.name+last.toString); last += 1; }
  freshName
}

assert( freshName(Set('y,'z),'x) == 'x)
assert( freshName(Set('x2,'x0,'x4,'x,'x1),'x) == 'x3)

def vars(e: Exp) : Set[Symbol] =  e match {
   case Id(SimpleVar(x)) => Set(x)
   case Add(l,r) => vars(l) ++ vars(r)
   case Fun(_,body) => vars(body)
   case App(f,a) => vars(f) ++ vars(a)
   case MacroApp(_,_) => sys.error("vars works only after macro expansion")
   case _ => Set.empty
}

def subst(e1 : Exp, w: Var, e2: Id) : Exp = e1 match {
  case Add(l,r) => Add(subst(l,w,e2),subst(r,w,e2))
  case App(f,a) => App(subst(f,w,e2),subst(a,w,e2))
  case Id(y) => if (y == w) e2 else e1
  case Fun(param,body) => 
    if (param == w) e1 else Fun(param,subst(body, w, e2))                            
}

def removeBoundTimeStamps(e: Exp) : Exp = e match {
  case Add(l,r) => Add(removeBoundTimeStamps(l),removeBoundTimeStamps(r))
  case Fun(TSVar(name,ts),body) => {
    val p = Symbol(freshName(vars(body),name).name + ":new")
    Fun(SimpleVar(p),removeBoundTimeStamps(subst(body,TSVar(name,ts),Id(SimpleVar(p)))))
  }
  case App(f,a) => App(removeBoundTimeStamps(f),removeBoundTimeStamps(a))
  case MacroApp(name, args) => sys.error("non-expanded macro: " + e.toString)
  case _ => e
}

def removeUnboundTimeStamps(e: Exp) : Exp = e match {
  case Add(l,r) => Add(removeUnboundTimeStamps(l),removeUnboundTimeStamps(r))
  case Fun(TSVar(name,ts),body) => Fun(SimpleVar(name),removeUnboundTimeStamps(body))
  case Fun(SimpleVar(name),body) => Fun(SimpleVar(name),removeUnboundTimeStamps(body))
  case App(f,a) => App(removeUnboundTimeStamps(f),removeUnboundTimeStamps(a))
  case Id(TSVar(name,ts)) => Id(SimpleVar(name))
  case MacroApp(name, args) => sys.error("non-expanded macro: " + e.toString)
  case _ => e
}
 

val macros : MacroApp => Exp =  {
  case MacroApp('LET, Id(i) :: e :: b :: Nil) => App(Fun(i,b),e)
  case MacroApp('IF, a :: b :: c :: Nil) => App(App(App('ef, a),b),c)
  case MacroApp('OR, e1 :: e2 :: Nil) => MacroApp('LET, List('v, e1, MacroApp('IF, List('v,'v,e2))))
  case MacroApp('NAIVEOR, a :: b :: Nil) => { val v = Id(TSVar('v,0)); MacroApp('LET, List(v, a, MacroApp('IF, List(v, v, b)))) }
  case e => sys.error("Cannot expand: "+e.toString)
}

def ehyg(e : Exp) : Exp = removeUnboundTimeStamps(removeBoundTimeStamps(evMacro(addTimeStamp(0,e),macros,1)))

val ex1 = MacroApp('LET, List('x, MacroApp('OR, List('a,'v)), MacroApp('NAIVEOR, List('x,'v))))

// try, e.g. ehyg(ex1)
// ehyg(ex1) = ((lambda x:new ((lambda v:new (((ef v:new) v:new) v:new)) x:new)) ((lambda v:new (((ef v:new) v:new) v)) a))