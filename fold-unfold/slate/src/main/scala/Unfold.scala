

object Unfold {

  def unfold[A, B](initial: A)(fun: A => Option[(A, B)]): Stream[B] =
    fun(initial).map { case (a, b) => b #:: unfold(a)(fun) }.getOrElse(Stream.empty)

  //    fun(initial) match {
  //      case Some((a, b)) => b #:: unfold(a)(fun)
  //      case None => empty
  //    }

  def unfold0[A](initial: A)(fun: A => Option[A]): Stream[A] =
    unfold(initial)(state => fun(state).map(newState => (newState, newState)))

  //    fun(initial) match {
  //      case Some(value) => value #:: unfold0(value)(fun)
  //      case None => empty
  //    }

  def unfoldE[A, B](initial: A)(fun: A => Stream[Either[A, B]]): Stream[B] =
    fun(initial).flatMap {
      case Left(a) => unfoldE(a)(fun)
      case Right(b) => Stream(b)
    }

  //    fun(initial) match {
  //      case Stream.Empty => empty
  //      case cons: Stream.Cons[_] => cons.head match {
  //        case Right(b) => Right(b) #:: unfoldE(cons.tail.head)(fun)
  //        case Left(_) => unfoldE(cons.tail.head)(fun)
  //      }
  //    }

  def leapYears(from: Int): Stream[Int] = unfold0(from)(x => Some(x + 1)) filter { year =>
    (year % 4 == 0) && (!(year % 100 == 0) || (year % 400 == 0))
  }

  //    unfoldE(from) { year =>
  //      Stream(
  //        if (isLeapYear(year)) Right(year)
  //        else Left(year)
  //      )
  //    }

  private def isLeapYear(year: Int) = year % 4 == 0 && year % 100 != 0 && year % 400 == 0

  def hailStone(from: Int): Stream[Int] = unfold0(from)({
    case 1 => None
    case x if x % 2 == 0 => Some(x / 2)
    case x => Some(x * 3 + 1)
  })

  def fibonacci: Stream[Int] = unfold((1, 1)) {
    case (prev, current) => Some(((current, prev + current), prev))
  }

}