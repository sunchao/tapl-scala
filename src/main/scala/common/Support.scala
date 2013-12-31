package common

// a Show type class
trait Show[A] {
  def show(a: A): String
}

object Show {
  def show[A](t:A)(implicit shower: Show[A]): String = shower.show(t) 
}