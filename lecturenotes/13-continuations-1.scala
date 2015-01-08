/**
Programming in Continuation-Passing Style (CPS)
===============================================

With the programming language features we have seen so far, the
order of execution was always implicit and fixed for a given
language. Today, we learn how to write a program so that it can
specify its own order of execution explicitly, and even change
the order of execution. The key concept will be the notion of the
*continuation* of a point during execution of a program. The
continuation is the part of the computation that has to be
executed after that point, that is, the continuation describes
how to *continue* execution.

Remember how we made functions first-class, and that gaves us
additional expressivity because we could write functions over
functions and store functions in datastructures? Today is the
first of a couple of lectures about making continuations
first-class, and again, this will give us additional expressivity
because we will be able to write functions over continuations and
store continuations in datastructures. Making things first-class
in order to gain expressivity is a common theme in the world of
programming languages.

But before we learn more about continuations, here are some
examples of programs that require unusual orders of execution,
and can therefore benefit from continuations:

  * In a debugger, you want to stop execution of the program to
    inspect the state, and then resume execution at the precise
    point where it stopped.

  * With lightweight concurrency, you want to execute multiple
    threads of execution at the same time, jumping back and forth
    between them.

  * In a distributed system, you want to halt execution on one
    computer while you wait for the execution on some other
    computer to finish.

Such unusual orders of execution are also called control
effects. Programming with continuations supports these and many
other examples of control effects. Today, we will focus on
distributed systems, or specifically, the basic architecture of a
Web application.

Running example: A very simple calculator
-----------------------------------------

We use a very simple program as a running example. The program
asks the users for two numbers and then displays the sum of the
numbers. Here is the desktop version of this calculator:
*/

object Desktop {
  def inputNumber(prompt: String): Int = {
    print(prompt + "> ")
    Integer.parseInt(readLine())
  }

  def program() {
    println("The sum is " +
      (inputNumber("First number") +
        inputNumber("Second number")) +
      ".")
  }
}

/**
Here is an example interaction with this simple program:

    scala> Desktop.program()
    First number> 2
    Second number> 40
    The sum is 42.

Note that `Desktop.program` performs the following steps, one
after another:

 1. Ask the user for the first number.
 2. Ask the user for the second number.
 3. Print the sum of the two numbers.

We don't have to specify that these steps are done in this order,
and we cannot change the order. This fixed order of execution
becomes a problem when we consider how a Web version of the
calculator would have to work. The user interaction would be
distributed over multiple pages:

 1. the first number is entered on the first page

 2. when the first number is submitted,
    a form to enter the second number is shown

 3. when the second number is submitted,
    a form with the result is generated and shown.

Even this "addition server" is difficult to implement. Because
HTTP is a stateless protocol (for good reasons), every Web
program is basically forced to terminate after every request, and
we cannot just use the `Desktop.program` operation that we wrote
before. Instead, the Web developer must turn this application
into three programs for the three pages:

 1. The first program displays the first form.

 2. The second program consumes the form values from the first
    form, and generates the second form.

 3. The third program consumes the form values from the second
    form, computes the output, and generates the result.

For the purpose of this lecture, we ignore HTTP, HTML etc. and
just output strings. But the programs we write have the same
structure that a real Web program could have.

First try: Failing to remember firstNumber
------------------------------------------

Let's try to translate the Desktop version to a Web version of
the calculator.
*/

object FirstTry {
  /**
  We change the `inputNumber` method to only print the prompt
  but not actually read any input from the user. In a Web
  program, all interaction with the user is done by the
  browser, but we try to model the server here. So instead of
  reading input from the user, we print a line with
  information where to send the input when it becomes
  available. In a Web program, this would be an URL the
  browser would send the input to after the user clicks the
  "submit" button.
  */
  def inputNumber(prompt: String, name: String) = {
    print(prompt + "> ")
    print("Send answer to " + name)
  }

  /**
  We split the `program` method into three methods that
  correspond to the three steps above. The first program asks the
  user to call the second program with the first number
  */

  def program1() {
    inputNumber("First number", "FirstTry.program2")
  }

  /**
  The second program gets the first number as an argument but
  doesn't do anything with it. Instead, it just asks the user to
  call the third program with the second number.
  */

  def program2(firstNumber: Int) {
    inputNumber("Second number", "FirstTry.program3")
  }

  /**
  The third program gets the second number as an argument and
  would like to add the first number and the second number. But
  this doesn't work because the first number is no longer known.
  */

  def program3(secondNumber: Int) {
    // commented out because firstNumber is not known here
    //
    // println("The sum is " +
    //   (firstNumber + secondNumber) +
    //   ".")
  }
}

/**
Our first try failed because when we learn the second number, we
have already forgotten the first number.

Second try: Storing firstNumber in a database
---------------------------------------------

The problem with the code in `FirstTry` was that the third
program knew only the second number, but not the first
number. But the first number is known to second program. To
communicate information from one Web program to another, a Web
developer might use a Java Servlet session object, or a database
field, to store that information between the two program
runs. (Application developers are often pushed to do this because
that is the feature most conveniently supported by the language
and API they are employing). We can model this solution idea in
Scala as follows:
*/

object Database {
  /**
  Both the inputNumber method and the first program are the same
  as in the first try.
  */

  def inputNumber(prompt: String, name: String) = {
    println(prompt + "> ")
    println("Send answer to " + name)
  }

  def program1() {
    inputNumber("First number", "Database.program2")
  }

  /**
  We use a single mutable variable to simulate the server-side
  database.
  */

  var database: Int = 0

  /**
  We modify the second program to store the first number in the
  database.
  */

  def program2(firstNumber: Int) {
    database = firstNumber
    inputNumber("Second number", "Database.program3")
  }

  /**
  The third program can now recover the first number from the
  database and compute the sum.
  */

  def program3(secondNumber: Int) {
    val firstNumber = database

    println("The sum is " +
      (firstNumber + secondNumber) +
      ".")
  }
}

/**
This works fine if there is only one user of the Web application,
and if that user only access the pages generated by the three
programs once, in the correct order. Here is an example
interaction that shows what works:

    // Access first Web page
    scala> Database.program1()
    First number>
    Send answer to Database.program2

    // Fill out first form, press "submit"
    scala> Database.program2(2)
    Second number>
    Send answer to Database.program3

    // Fill out second form, press "submit"
    scala> Database.program3(40)
    The sum is 42.

But if the user keeps the windows with the page generated by
program2 open, then program3 seems to compute the wrong
answer. Here is an example interaction that shows some of the
problems:

    // Access first Web page
    scala> Database.program1()
    First number>
    Send answer to Database.program2

    // Fill out first form, press "submit"
    scala> Database.program2(2)
    Second number>
    Send answer to Database.program3

    // Fill out second form, press "submit"
    scala> Database.program3(40)
    The sum is 42.

    // Go back to first form, change value, press "submit"
    scala> Database.program2(10)
    Second number>
    Send answer to Database.program3

    // Go back to result form, press "refresh"
    scala> Database.program3(40)
    The sum is 50.

At the very end, it is not clear whether we want `42` (like
last time we accessed the very same Web page) or whether
we want `50` (because we changed the first number in
between). Imagine trying to use this Web application in two
browser tabs, hoping to perform two independent
calculations. Instead, the value of one brower tab would override
the value from the other. This is a typical problem with Web
applications that use server site state to communicate
information between Web pages.

Third try: Stateless server
---------------------------

The problem with the `Database` solution above is related to the
use of server-side state. Alternatively, Web developers can use
the hidden field mechanism of HTML to send the first number back
to the client, to be resubmitted to the next server-side
program. We can model this in Scala as follows:
*/

object Stateless {
  /**
  Both the inputNumber method and the first program are the same
  as in the first try.
  */

  def inputNumber(prompt: String, name: String) = {
    println(prompt + "> ")
    println("Send answer to " + name)
  }

  def program1() {
    inputNumber("First number", "Stateless.program2")
  }

  /**
  We modify the second program to encode the first number in the
  program the user should call with the second number. In Web
  programming, this corresponds to encoding all information from
  previous forms in the URL or in hidden fields.
  */

  def program2(firstNumber: Int) {
    inputNumber("Second number", "Stateless.program3(" + firstNumber + ")")
  }

  /**
  The third program now gets called with both numbers.
  */

  def program3(firstNumber: Int)(secondNumber: Int) {
    println("The sum is " +
      (firstNumber + secondNumber) +
      ".")
  }
}

/**
With this version of the program, we can go back in the browser
history and enter other numbers, always computing the correct
information overall. For example:

    // Access first Web page
    scala> Stateless.program1()
    First number>
    Send answer to Stateless.program2

    // Fill out first form, press "submit"
    scala> Stateless.program2(2)
    Second number>
    Send answer to Stateless.program3(2)

    // Fill out second form, press "submit"
    scala> Stateless.program3(2)(40)
    The sum is 42.

    // Go back to first form, change value, press "submit"
    scala> Stateless.program2(10)
    Second number>
    Send answer to Stateless.program3(10)

    // Go back to result form, press "refresh"
    scala> Stateless.program3(2)(40)
    The sum is 42.

With this implementation, we could open the application in
multiple browser tabs, and the calculations in teh various tabs
would not interact. While this programming style works fine for
Web applications, it is annoying that we have to change how we
program just because we move an application from a Desktop
environment to the Web.

Continuations
-------------

In a way, the difference between the Desktop and Web versions of
the simple calculator in our examples is just how `inputNumber`
communicates with the user. In a perfect world, we would only
have to change `inputNumber` in order to switch between Desktop
and Web support. But for technical reasons, we had the change the
whole structure of the program instead.

We could do better if we could somehow get hold of the pending
computation (or: continuation) at the time when the method
`inputNumber` was invoked.

To understand what these continuations are, we look at the
original, Desktop version of the calculator again.
*/

object Continuations {
  /**
  The same `inputNumber` as in `Desktop` above.
  */
  def inputNumber(prompt: String): Int = {
    print(prompt + "> ")
    Integer.parseInt(readLine())
  }

  /**
  The same `program` as in `Desctop` above.
  */
  def variant1() {
    println("The sum is " +
      (inputNumber("First number") +
        inputNumber("Second number")) +
      ".")
  }

/**
The pending computation (or, continuation in the following) of
the first call to inputNumber is to to take the result of the
first inputNumber invocation, invoke inputNumber again, then add
the result of the second invocation to the result of the first
invocation, and finally display the sum. The overall behavior of
`program` is to first invoke `inputNumber` and then execute the
continuation. We can make this split between what happens first
and what the continuation is explicit by representing the
continuation as a function:
*/
  def variant2() {
    val cont1 = (firstNumber: Int) => {
      println("The sum is " +
        (firstNumber +
          inputNumber("Second number")) +
        ".")
    }

    cont1(inputNumber("First number"))
  }

/**
Note that `variant1` and `variant2` do exactly the same thing,
but in `variant2`, it is clearer what happens first and what the
continuation is. This continuation contains another invocation of
inputNumber. We can make the continuation of this second
invocation of inputNumber explicit as follows:
*/

  def variant3() {
    val cont1 = (firstNumber: Int) => {
      val cont2 = (secondNumber: Int) => {
        println("The sum is " +
          (firstNumber +
            secondNumber) +
          ".")
      }

      cont2(inputNumber("SecondNumber"))
    }

    cont1(inputNumber("First number"))
  }
}

/**
Note that inside `cont2`, we are using the variable `firstNumber`
that is in scope as the argument of `cont1`. In other words, the
value of the variable `firstNumber` is stored in the closure of
`cont2`.

Continuation-passing style
--------------------------

So far, we have made the continuations of the invocations of
`inputNumber` explicit without being able to use them for
anything good. The key for using continuations to achieve control
effects is to change functions to accept their continuation. In
this case, instead of calling something like

    cont1(inputNumber("First number"))

we want to call something like:

    inputNumber("First number", cont1)

That is, instead of waiting for `inputNumber` to return a number
and then invoking the continuation ourselves, we want to pass the
continuation to `inputNumber` and have it invoke the continuation
for us. This style of passing continuations to functions is
called *continuation-passing style* (or short: CPS).

We implement a version of `program` in continuation-passing style
in a class with an abstract method for `inputNumber`. Later, we
can specialize `inputNumber` to support Desktop-style or
Web-style user interaction without further changes to `program`,
as promised.
*/

abstract class CPS {
  /**
  An abstract version of `inputNumber` that accepts a
  continuation as an additional argument. When finished with the
  user interaction, instead of returning a number, it invokes
  this continuation with the number as argument. Traditionally,
  the continuation to call after a function finishes is called
  `k`.
  */
  def inputNumber(prompt: String, k: Int => Unit)

  /**
  We can now write the program in continuation-passing style:
  */

  def variant1() {
    val cont1 = (firstNumber: Int) => {
      val cont2 = (secondNumber: Int) => {
        println("The sum is " +
          (firstNumber +
            secondNumber) +
          ".")
      }

      inputNumber("SecondNumber", cont2)
    }

    inputNumber("First number", cont1)
  }

  /**
  Equivalently, we can use anonymous functions for the
  continuations:
  */

  def variant2() {
    inputNumber("First number", firstNumber =>
      inputNumber("SecondNumber", secondNumber =>
        println("The sum is " +
          (firstNumber +
            secondNumber) +
          ".")
      )
    )
  }

  /**
  Note that in `variant2`, the nested calls to inputNumber appear
  in their eventual order of execution. That's why programs in CPS
  are usually written with anonymous functions for the
  continuations, as in `variant2`.
  */
}

/**
CPS, Desktop-version
--------------------

We can now easily provide an implementation for `inputNumber`
that works for Desktop programs:
*/

object CPSDesktop extends CPS {
  def inputNumber(prompt: String, k: Int => Unit) {
    print(prompt + "> ")
    k(Integer.parseInt(readLine()))
  }
}

/**

This implementation of `inputNumber` simply calls the
continuation after the user enters the number. This is the CPS
version of `Desktop.inputNumber`. Overall, the interaction with
`CPSDesktop.variant1` (or `variant2`) is just like with
`Dektop.program`:

    scala> CPSDesktop.variant1()
    First number> 2
    SecondNumber> 30
    The sum is 42.

CPS, Web-version
----------------

The implementation of `inputNumber` for Web programs needs to
generate a Web page and arrange for the server to invoke the
continuation once the user entered the number on the client. This
can be implemented, say, by associating to a contination a
unique ID, store the continuation in a database on the server
using this ID, and storing the ID in the form such that it gets
submitted back to the server once the client presses the submit
button.

Here is code illustrating this idea. For simplicity we assume
that all web forms return a single integer value.
*/

object CPSWeb extends CPS {
  /**
  A database of continuations.
  */

  var database: Map[String, Int => Unit] = Map()

  /**
  Generating unique identifiers.
  */

  var counter: Int = 0
  def freshName: String = {
    counter += 1
    "program" + counter
  }

  /**
  This version of `inputNumber` stores the continuation in the
  database and then stops executing (by not calling any
  continuation).
  */
  def inputNumber(prompt: String, k: Int => Unit) {
    println(prompt + "> ")
    val name = freshName
    database += (name -> k)
    println("Send answer to " + name)
  }

  /**
  The client should call the following function with the name of
  the program to execute as well as the number entered by the
  user.
  */
  def call(name: String, answer: Int) {
    database(name)(answer)
  }
}

/**

The interaction with `CPSWeb` allows multiple tabs etc, just like
for the `Stateless` version:

    // Access first Web page
    scala> CPSWeb.variant1()
    First number>
    Send answer to program1

    // Fill out first form, press "submit"
    scala> CPSWeb.call("program1", 2)
    SecondNumber>
    Send answer to program2

    // Fill out second form, press "submit"
    scala> CPSWeb.call("program2", 40)
    The sum is 42.

    // Go back to first form, change value, press "submit"
    scala> CPSWeb.call("program1", 10)
    SecondNumber>
    Send answer to program3

    // Go back to result form, press "refresh"
    scala> CPSWeb.call("program2", 40)
    The sum is 42.

In this version of the Web application, all special handling of
client-server communication is in the `inputNumber` method, and
the `program` method is the same as in the (CPS) Desktop
version. This is possible because programming in
continuation-passing style allows us to abstract over the order
of execution.

Three properties of programs in CPS
-----------------------------------

Let's take a step back and look again what it means if a program
is in continuation-passing style. Programs in CPS have the
following properties:

  1. All calls are in tail position.
  2. All functions take continuation arguments.
  3. No function returns a value.

About the first property: Something is in tail position
if in some executions of a function, it is the last thing that
happens before the function is finished. For example, consider
this function:

    def a() {
      if b() {
        c()
      else
        d(e())
      }
    }

 - The call to b() is not in tail position, because after b() is
   called, we always have to call either c() or e() and d()
   before the function a() is finished.

 - The call to c() is in tail position, because sometimes (when
   b() returns true), the call to c() is the last thing that
   happens in the execution of the function a().

 - The call to e() is not in tail position, because after e() is
   called, we always have to call d() before the function a() is
   finished.

 - the call to d() is in tail positions, because sometimes (when
   b() returns false), the call to c() is the last thing that
   happens in the execution of the function a().

The first property also implies that the order of execution is
very clear and explicit: Since every call is in tail position,
the first thing a function does has to be the last thing the
function does. In other words, every function only does one thing
and then calls the continuation to continue execution with the
next thing. This leads to a convenient sequentialization of the
program into lots of one-thing-per-step functions.

About the second property: Since *all* functions in a program
take continuation arguments, this implies that the transformation
to continuation-passing style has to be global. If we want to use
continuations somewhere in our program, we have to transform the
whole program to continuation-passing style, including all libraries.

About the third property: It wouldn't make sense to return the
value, because the call to that function would be in tail
position (per the first property), so no other function that
could use that value for something would be executed after the
return of the value-returning function.

Our understanding of these properties will help us to formalize
the transformation of arbitrary FAE programs into
continuation-passing style in the next lecture.

Three steps for transforming into CPS by hand
---------------------------------------------

At some point in the lecture today, we transformed
`Desktop.program` to CPS. We ended up with `CPS.variant2`. Let's
look again at how we can transform an arbitrary piece of code
into continuation-passing style, by hand. We do this in the
following three steps:

 1. give a name to the result of every nontrivial subexpression.
 2. choose an evaluation order for the nontrivial subexpressions.
 3. use continuations instead of helper variables

Here is the transformation we have seen before, using these three
steps. We start with:

    def program() {
      println("The sum is " +
        (inputNumber("First number") +
          inputNumber("Second number")) +
        ".")
    }

In the first step, we assign the following names to the subexpressions:

 - firstNumber = inputNumber("First number")
 - secondNumber = inputNumber("Second number")

Here, we treat the + operator as trivial. We could also treat it
as non-trivial and give names to subexpressions involving the +
operator. Then we would have to CPS-transform also the
implementation of +. We will talk about the difference between
trivial and nontrivial expressions more in the next lecture.

In the second step, we choose an evaluation order. Let's choose
to compute firstNumber first, and secondNumber second. After this
step, we rewrite the program as follows:

    def program() {
      val firstNumber = inputNumber("First number")
      val secondNumber = inputNumber("Second number")
      println("The sum is " +
        (firstNumber +
          secondNumber) +
        ".")
    }

In the third step, we add an continuation argument to program,
call that continuation instead of returning a result, and we
rewrite

    val name = function(args)
    rest

to

    function(args, name =>
      rest).

In this case, we get the following program, which is in
continuation-passing style:

    def program(k: Int => Unit) {
      inputNumber("First number", firstNumber =>
        inputNumber("Second number", secondNumber =>
          println("The sum is " +
            (firstNumber +
              secondNumber) +
            ".")))
    }

Note that in this step, we assume that inputNumber has also been
transformed to continuation-passing style. So we have to apply
the three steps to every function in our program.

Another example: fibonacci numbers
----------------------------------

Here is another simple example for how to transform into CPS
manually: The good old fibonacci function. We start with:

    def fibonacci(n: Int): Int =
      if (n < 2)
        1
      else
        fibonacci(n - 1) + fibonacci(n - 2)

In the first step, we give names to every nontrivial
subexpression:

  - a = fibonacci(n - 1)
  - b = fibonacci(n - 2)

In the second step, we choose an evaluation order for the
nontrivial subexpressions. The usual choice would be to evaluate
a first, and then b. But in continuation-passing style, we can
also choose to evaluate b first, and then a. After all, with
continuations, we are more flexible in what we want to evaluate
when. Let's choose that unusual evaluation order:

    def fibonacci(n: Int): Int =
      if (n < 2)
        1
      else {
        val b = fibonacci(n - 2)
        val a = fibonacci(n - 1)
        a + b
      }

In the third step, we add a continuation argument and use
continuations everywhere:

    def fibonacci(n: Int, k: Int => Unit) {
      if (n < 2)
        k(1)
      else
        fibonacci(n - 2, b =>
          fibonacci(n - 1, a =>
            k(a + b)))
    }
*/
