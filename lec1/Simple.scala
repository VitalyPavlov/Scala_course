// Home work for Lec1: Functions in Scala 
// Fix errors, rewrite functions or write your own implementations here 
// bku Apr 27 2019


object Simple extends App {
  
//Fix error
//--------------------------------------------------------------------------------------------
  def Func0 (a:Int):Some[Int] = {
    Some(-1*a)
  }

  val res0:Some[Int] = Func0(7)
  println(s"res0 = $res0")


// Rewrite to accept the lambda
//--------------------------------------------------------------------------------------------  
  def Func1 (f:(Int, Int) => Int) = {
    f(0, 1)
  }

  val res1  = Func1 ((x:Int, y:Int) => 2*x + y)
  println(s"res1 = $res1")


// Curry Func2 and write the full curried definition, as in Func0
//--------------------------------------------------------------------------------------------
  def Func2 (a:Int, b:Int, c:Int) = a + b + c

  def Func2_curried (a:Int) = ((b:Int) => (c:Int) => a + b + c)

  val res2 = Func2(1, 2, 3)
  val res3 = Func2_curried(1)(2)(3)

  println(s"res2 = $res2")
  println(s"res3 = $res3")


// f(x) = 1/x was definied as Partially Defined Function. Rewrite it with 
// A. pattern matching  
// B. Option
//--------------------------------------------------------------------------------------------
  def Func3_a (a:Int) = a match {
    case 0 => 0
    case _ => 1/a
  }
  
  def Func3_b (a:Int):Option[Int] = {
    if (a == 0) None
    else Some(1/a)
  } 

  val res4 = Func3_a (0)
  val res5 = Func3_b (0)

  println(s"res4 = $res4")
  println(s"res5 = $res5")


// Higher order function (HOF) "reduce" from lecture notes returns incorrect result for the add reducer 
// correct the error and implement a valid "reduce" for add and mul reducers
//--------------------------------------------------------------------------------------------
  def reduce (arr:Vector[Int], f:(Int, Int) => Int):Int = {
    val zero = 1 - f(1,0)
    val tmp = arr.foldLeft(zero) {f}
    tmp
  }   

  def add (a:Int, b:Int) = a + b
  def multiply (a:Int, b:Int) = a * b

  val arr = Vector(1,2,3,4)
  val res6 = reduce(arr, add)
  val res7 = reduce(arr, multiply)

  println(s"add_reduce $res6")
  println(s"multiply_reduce $res7")
  
// Write an implicit function to support the operation below
//--------------------------------------------------------------------------------------------
  
  implicit def Func4 (a:Float):Int = (2*a).toInt

  def doubleme (a:Int) = 2*a

  val res8  = doubleme(2.5F)
  println(s"res8 = $res8")
  

}
