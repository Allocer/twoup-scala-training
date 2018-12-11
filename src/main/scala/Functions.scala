import scala.annotation.tailrec

object Functions extends App {

  //Simple function and method
  val function = () => println("I am function")
  function()

  val function2 = (i: Int, j: Int) => i * j
  println(function2(3, 2))

  //a function can be the final value
  println(function2)

  def sum(x: Int, y: Int) = x + y

  println(sum(5, 5))

  //a method can't be the final value
  //sum //compile error

  //DIFFERENCES: A method can appear in an expression as an internal value (to be called with arguments) but it can’t be the final value, while a function can

  //a method can have no parameter list
  def method = 20

  println(method)

  //a method can have an empty parameter list
  def method2() = 10

  println(method2)
  println(method2()) //can invoke methods like this also

  //a function's parameter list could be empty but functions MUST HAVE parameter list
  val f = () => println("I have an empty parameter list")
  val f1 = (s: String) => println(s"I have 1 argument: ${s}")
  f()
  f1("Scalowe czary-mary")

  //annonymous functions
  val increment = (x: Int) => x + 1
  val incrementationAsAnonymousFunction = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }
  println(incrementationAsAnonymousFunction(2))

  val addAnonymous = new Function2[Int, Int, Int] { //eq to: val addNormal = (a: Int, b: Int) => a + b
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }
  println(addAnonymous(2, 3))

  //Functions as parameters
  val list = List(1, 2, 3, 4)
  val mapped = list.map(x => x * 2) //expression: x => x * 2 is a function, so we pass function as an argument
  println(mapped)
  println(mapped.sum)

  val filterMeLikeOneOfYourFrenchGirls = (x: Int) => x % 2 == 0
  val filtered = list.filter(filterMeLikeOneOfYourFrenchGirls)
  println(filtered)
  val filtered2 = list.filter(_ % 2 == 0)
  println(filtered2)

  //ETA expansion - because when a function is expected but a method is provided, it will be automatically converted into a function
  def changeElement(x: Int) = x * x

  val changedList = list.map(changeElement) //why it does work?
  println(changedList)

  //HOF - higher order functions
  //- One or more of its parameters is a function, and it returns some value.
  //- It returns a function, but none of its parameters is a function.
  //- Both of the above: One or more of its parameters is a function, and it returns a function.
  def giveMeAFunction(f: Int => Int, x: Int) = f(x)

  val func = (x: Int) => x + 1
  println(giveMeAFunction(func, 10))

  def sayMeSomething(answer: () => Unit) = answer()

  def sayHello(): Unit = println("Hello")

  sayMeSomething(sayHello)

  //Return functions
  val anotherExample = (f: String => String, s: String) => f(s)
  val func2 = (s: String) => s
  println(anotherExample(func2, "Your Argument"))

  val anotherExample1 = (f: String => String) => f("Your Argument")
  val func3 = (s: String) => s
  println(anotherExample1(func3))

  //Partial functions
  val sum = (a: Int, b: Int, c: Int) => a + b + c
  println(sum(1, 2, 3))

  val partialSum = sum(1, 2, _: Int) //partial function with no third parameter (compile error if no type explicite)
  println(partialSum(3))

  //The first two numbers (1 and 2) were passed into the original sum function; that process created the new function named f, which is a
  // partially applied function; then, some time later in the code, the third number (3) was passed into f.

  val partialSum2 = sum(_: Int, 4, _: Int) //partial function with no 2 args
  println(partialSum2(1, 2))


  //Currying
  def currying(x: Int)(y: Int) = x + y

  val res1 = currying(3)(5)
  println(res1)

  val res2 = currying(2) _ //underscore, partial function
  val res3 = res2(3)
  println(res3)


  def curryBinaryOperator[A](operator: (A, A) => A): A => (A => A) = {
    def curry(a: A): A => A = {
      (b: A) => operator(a, b)
    }

    curry
  }

  def add(a: Int, b: Int) = a + b // (Int, Int) => Int
  def multiply(a: Int, b: Int) = a * b // (Int, Int) => Int

  val addCurried = curryBinaryOperator(add) // Int => (Int => Int)
  val multiplyCurried = curryBinaryOperator(multiply) // Int => (Int => Int)

  //Recursion = IN SCALA NEVER USE TRADITIONAL LOOPS like while
  def traditionalFibonacci(n: Int): Int = {
    if (n <= 2) 1
    else traditionalFibonacci(n - 1) + traditionalFibonacci(n - 2)
  }

  println(traditionalFibonacci(20))

  def getFibonacci(index: Int): Int = {
    @tailrec
    def getTailRec(index: Int, prev: Int, current: Int): Int = {
      if (index <= 0) {
        current
      } else {
        getTailRec(index - 1, prev = prev + current, current = prev)
      }
    }

    getTailRec(index, prev = 1, current = 0)
  }

  println(getFibonacci(20))

  //Sum all elements between two numbers
  def sumRecursive(a: Int, b: Int): Int = {
    if (a > b) 0
    else sumRecursive(a + 1, b) //need to set return type explicite
  }

  println(sumRecursive(1, 100))

  // 1 - basic recursive factorial method
  def factorial(n: Int): Int = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  println(factorial(10))

  // 2 - tail-recursive factorial method
  def factorial2(n: Long): Long = {
    @tailrec
    def factorialAccumulator(acc: Long, n: Long): Long = {
      if (n == 0) acc
      else factorialAccumulator(n * acc, n - 1)
    }

    factorialAccumulator(1, n)
  }

  println(factorial2(10))

  def square(x: Int): Int = x * x

  def sumSquares(a: Int, b: Int): Int =
    if (a > b) 0 else square(a) + sumSquares(a + 1, b)

  println(sumSquares(2, 5))
  //Tail recursion is a special case of recursion where the calling function does no more computation after making a recursive call


}
