import java.lang.ArrayIndexOutOfBoundsException
import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = {
		ints match {
			case Nil => 0
			case Cons(x, xs) => x + sum(xs)
		}
	}

	def product(ds: List[Double]): Double = {
		ds match {
			case Nil => 1.0
			case Cons(0.0, _) => 0.0
			case Cons(x, xs) => x * product(xs)
		}
	}

	def apply[A](as: A*): List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail*))
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = {
		a1 match {
			case Nil => a2
			case Cons(h, t) => Cons(h, append(t, a2))
		}
	}

	def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
		as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}
	}

	def sum_2(ns: List[Int]) = {
		foldRight(ns, 0)((x, y) => x + y)
	}
	
	def product_2(ns: List[Double]) = {
		foldRight(ns, 1.0)(_ * _)
	}

	// Exercise 3.2
	def tail[A](l: List[A]): List[A] = {
		l match {
			case Nil => Nil 	// Good idea to throw exception
			case Cons(_: A, t) => t
		}
	}

	// Exercise 3.3
	def setHead[A](newElem: A, l: List[A]): List[A] = {
		l match {
			case Nil => List(newElem)
			case Cons(h, t) => Cons(newElem, t)
		}
	}

	// Exercise 3.4
	def drop[A](l: List[A], n: Int): List[A] = {
		@annotation.tailrec
		def loop(l: List[A], n: Int): List[A] = {
			n match {
				case 0 => l
				case _ => loop(tail(l), n - 1)
			}
		}
		loop(l, n)
	}
	def drop_2[A](l: List[A], n: Int): List[A] = {
		n match {
			case x: Int if (x <= 0) => l
			case _ => drop_2(tail(l), n - 1)
		}
	}

	// Exercise 3.5
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
		l match {
			case Nil => List()
			case Cons(h, t) => if (f(h)) dropWhile(t, f) else l 
		}
	}

	// Exercise 3.6
	def init[A](l: List[A]): List[A] = {
		def inner(og: List[A], ans: List[A]): List[A] = {
			og match {
				case Cons(h, Nil) => ans
				case Cons(h, t) => inner(t, append(ans, List(h)))
				case Nil => throw new ArrayIndexOutOfBoundsException
			}
		}
		inner(l, List())
	}
	def init_2[A](l: List[A]): List[A] = {
		l match {
			case Nil => throw new ArrayIndexOutOfBoundsException
			case Cons(_, Nil) => Nil
			case Cons(h, t) => Cons(h, init(t))
		}
	}

	// Exercise 3.9
	def length[A](as: List[A]): Int = {
		foldRight(as, 0)((_, x) => x + 1)
	}

	// Exercise 3.10
	@annotation.tailrec
	def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
		l match {
			case Nil => z
			case Cons(h, t) => foldLeft(t, f(z, h))(f)
		}
	}

	// Exercise 3.11
	def sum3(l: List[Int]): Int = {
		foldLeft(l, 0)(_ + _)
	}
	def product3(l: List[Double]): Double = {
		foldLeft(l, 1.0)(_ * _)
	}
	def length_2[A](l: List[A]): Int = {
		foldLeft(l, 0)((z, _) => z + 1)
	}

	// Exercise 3.12
	def reverse[A](l: List[A]): List[A] = {
		foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))
	}

	// Exercise 3.13
	def foldLeftViaRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
		foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
	}
	def foldRightViaLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
		foldLeft(reverse(l), z)((t, h) => f(h, t))
	}

	// Exercise 3.14
	def append_2[A](a1: List[A], a2: List[A]): List[A] = {
		foldRight(a1, a2)(Cons(_, _))
	}

	// Exercise 3.15
	def flatten[A](ll: List[List[A]]): List[A] = {
		foldLeft(ll, Nil)(append_2)
	}

	// Exercise 3.16
	def increment(l: List[Int], n: Int): List[Int] = {
		foldLeft(l, Nil)((t, h) => Cons(h + n, t))
	}

	// Exercise 3.17
	def doubleToString(l: List[Double]): List[String] = {
		foldLeft(l, Nil)((t, h) => Cons(h.toString(), t))
	}

	// Exercise 3.18
	def map[A, B](l: List[A])(f: A => B): List[B] = {
		foldRightViaLeft(l, Nil)((h, t) => Cons(f(h), t))
	}
	def map_2[A, B](l: List[A])(f: A => B): List[B] = {
		val buf = new collection.mutable.ListBuffer[B]
		def go(l: List[A]): Unit = {
			l match {
				case Nil => ()
				case Cons(h, t) => buf += f(h); go(t)
			}
		}
		go(l)
		List(buf.toList*)
	}

	// Exercise 3.19
	def filter[A](l: List[A])(f: A => Boolean): List[A] = {
		foldRightViaLeft(l, Nil)((h, t) => if (f(h)) Cons(h, t) else t)
	}

	// Exercise 3.20
	def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
		flatten(foldRightViaLeft(l, Nil)((h, t) => Cons(f(h), t)))
	}

	// Exercise 3.21
	def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
		flatMap(l)(x => if (f(x)) List(x) else Nil)
	}

	// Exercise 3.22
	def zipSum(l1: List[Int], l2: List[Int]): List[Int] = {
		if (l1 == Nil || l2 == Nil) throw new IllegalArgumentException
		else if (length_2(l1) != length_2(l2)) throw new IllegalArgumentException
		@annotation.tailrec
		def go(l1: List[Int], l2: List[Int], res: List[Int]): List[Int] = {
			l1 match {
				case Nil => append(res, Nil)
				case Cons(h1, _) =>
					l2 match {
						case Cons(h2, _) => go(tail(l1), tail(l2), append(res, List(h1 + h2)))
					}
			}
		}
		go(l1, l2, List())
	}
	def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = {
		(l1, l2) match {
			case (Nil, _) => Nil
			case (_, Nil) => Nil
			case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
		}
	}

	// Exercise 3.23
	def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
		(l1, l2) match {
			case (Nil, _) => Nil
			case (_, Nil) => Nil
			case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
		}
	}

	// Exercise 3.24
	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
		@annotation.tailrec
		def countEquals(sup: List[A], sub: List[A], count: Int): Int = {
			(sup, sub) match {
				case (Nil, sub) => if (sub == Nil) count else -1
				case (_, Nil) => count
				case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) (countEquals(t1, t2, count + 1)) else -1
			}
		}
		var ans: Int = -2			// local mutability
		def go(sup: List[A]): Unit = {
			sup match {
				case Nil => ()
				case Cons(_, t) => ans = ans.max(countEquals(sup, sub, 0)); go(t)
			}
		}
		go(sup)
		if (ans == -1) false else true
	}
	def hasSubsequence_2[A](sup: List[A], sub: List[A]): Boolean = {
		@annotation.tailrec
		def startsWith[A](l: List[A], prefix: List[A]): Boolean = {
			(l, prefix) match {
				case (_, Nil) => true
				case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
				case _ => false
			}
		}
		sup match {
			case Nil => sub == Nil
			case _ if startsWith(sup, sub) => true
			case Cons(_, t) => hasSubsequence_2(t, sub)
		}
	}

	def main(args: Array[String]): Unit = {
		println(hasSubsequence(List(1, 2, 3, 4, 5), List()))
	}
}