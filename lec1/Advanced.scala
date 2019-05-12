
object Advanced extends App {

  val vec = Vector(1,2,3,4)  

  // Task 1
  // Implement a function, which sums all elevements in the int vector in several different ways 


  // With a lib function 
  //---------------------------------------------------
  def Sum0 (vec:Vector[Int]):Int = vec.sum

  val res0 = Sum0(vec)
  println(s"lib function $res0")

  
  // Using foreach
  //---------------------------------------------------
  def Sum1 (vec:Vector[Int]):Int = {
    var res:Int = 0
    vec.foreach(res += _)
    res  
  }

  val res1 = Sum1(vec)
  println(s"using foreach $res1")


  //Using imutables
  //---------------------------------------------------
  def Sum2 (vec:Vector[Int], f:(Int, Int) => Int):Int = {
    val zero = 0
    val res = vec.foldLeft(zero) {f}
    res
  }

  def add (a:Int, b:Int):Int = a + b

  val res2 = Sum2(vec, add)
  println(s"imutables $res2")


  // With iterator
  //---------------------------------------------------
  def Sum3 (vec:Vector[Int]):Int = {
    var res = 0
    for (item <- vec) 
      res += item
    res
  }

  val res3 = Sum3(vec)
  println(s"with iterator $res3")


  //Using recursion
  //---------------------------------------------------
  def Sum4 (vec:Vector[Int]):Int = {
    if (vec.length == 0) {
      return 0
    } else {
      return vec.head + Sum4(vec.tail)
    } 
  }

  val res4 = Sum4(vec)
  println(s"recursion $res4")


  // Using immutables only 
  case class Wrap(a:Int) {
    def add(b:Int):Wrap = copy(a = a + b)
  }  

  def Sum5 (vec:Vector[Int]):Int = {
    val init:Wrap = Wrap(0)	
    val tmp:Vector[Wrap] = vec map (v => init.add(v))
    val res:Int = tmp map (v => v.a) reduce(_ + _)
    res
  }
  
  val res5 = Sum5(vec)
  println(s"imutable $res5")


  // Task 2 

  // For recursive Implementation, can you implement a function, which sums up very large vectors? What are potential dangers for a recursive implementation?

}
