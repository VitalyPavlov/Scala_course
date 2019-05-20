
package lec2

import cats.{Monoid}
import cats.implicits._


object Simple extends App {
  
  // task 1: Dynamic Polymorphism
  // Implement a class hierarchy, according to the company.png UML diagram
  // Implement method def speak():Unit, which will display "I'm an employee", for example
  
  abstract class Human {
    def speak():Unit
  }

  trait Engineer extends Human {
    def speak():Unit = println("I'm an engineer")
  }

  trait Person extends Human {
    def speak():Unit = println("I'm a person")
  }

  trait Employee extends Person {
    override def speak():Unit = println("I'm an employee")
  }

  trait Manager extends Employee {
    override def speak():Unit = println("I'm a manager")
  }

  trait Founder extends Engineer with Person {
    override def speak():Unit = println("I'm a founder")
  }

  trait Investor extends Human {
    def speak():Unit = println("I'm a investor")
  }

  trait Owner extends Founder with Investor {
    override def speak():Unit = println("I'm an owner")
  }

  class Coworker extends Manager with Owner {
    override def speak():Unit = println("I'm a coworker")
  }

  val guy = new Coworker
  guy.speak


  // task2: Parametric Polymorphism 
  // Implement a correct version of the less() method for Int and String types

  class Duo[T:Ordering] (first:T, second:T) {

    val ord = implicitly[Ordering[T]]

    def less ():T = {
      if (ord.gt(first, second)) second else first
    }

  }

  val str = new Duo("f", "n")
  val num = new Duo(42, 24)

  println(str.less())
  println(num.less())

  // task 3: Ad-hoc Polymorphism 
  // Reimplement the reduce function from lec1 home work with ad-hoc Polymorphism. 
  
  // Implement handwritten solutions
  abstract class Addable[A] {
    def zero: A
    def add(a: A, b: A): A
  }

  object Addable {
    implicit val addableInt = new Addable[Int] {
      def zero = 0
      def add(a: Int, b: Int):Int = a + b
    }
    implicit val addableString = new Addable[String] {
      def zero = ""
      def add(a: String, b: String):String = a + b
    }
  }

  def reduce[A: Addable](arr: Vector[A]): A ={
    val ev = implicitly[Addable[A]]
    val tmp = arr.foldLeft(ev.zero) (ev.add)
    tmp
  }

  val aInt = Vector(2, 3, 4) // returns 9
  val aStr = Vector("2", "3", "4") // returns "234"

  val res1 = reduce(aInt)
  val res2 = reduce(aStr)

  println(s"res1 = $res1")
  println(s"res2 = $res2")

  // Implement library solutions
    def reduceMonoid[A] (arr:Vector[A])(implicit ev:Monoid[A]):A  = arr.foldLeft(ev.empty)(ev.combine)

    val res3  = reduceMonoid(aInt)
    val res4  = reduceMonoid(aStr)
    
    println (s"res3 = $res3")
    println (s"res4 = $res4")

}


