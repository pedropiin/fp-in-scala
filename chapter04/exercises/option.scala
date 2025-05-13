sealed trait Option[+A] {
  // Exercise 1
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(v) => Some(f(v))
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(v) => v
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }
  def flatMapViaPatternMatching[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(v) => f(v)
      case None => None
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(opt => Some(opt)).getOrElse(ob)
  }
  def orElseViaPatternMatching[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(v) => Some(v)
      case None => ob
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    map(x => if f(x) then Some(x) else None).getOrElse(None)
  }
  def filterViaPatternMatching(f: A => Boolean): Option[A] = {
    this match {
      case Some(v) => if f(v) then Some(v) else None
      case None => None
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


def main(args: Array[String]): Unit = {

}