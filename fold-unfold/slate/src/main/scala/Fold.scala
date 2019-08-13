import Fold.sumFold
import cats.Monoid

trait Fold[I, O] {
  type M

  def tally(i: I): M

  def summarize(m: M): O

  def monoid: Monoid[M]
}

object Fold {

  implicit val optionIntMonoid: Monoid[Option[Int]] = new Monoid[Option[Int]] {
    override def empty: Option[Int] = Some(0)

    override def combine(x: Option[Int], y: Option[Int]): Option[Int] = x match {
      case Some(valueX) => y match {
        case Some(valueY) => Some(valueX + valueY)
        case None => None
      }
      case None => None
    }
  }

  implicit val optionDoubleMonoid: Monoid[Option[Double]] = new Monoid[Option[Double]] {
    override def empty: Option[Double] = Some(0.0)

    override def combine(x: Option[Double], y: Option[Double]): Option[Double] = x match {
      case Some(valueX) => y match {
        case Some(valueY) => Some(valueX + valueY)
        case None => None
      }
      case None => None
    }
  }

  def runList[I, O](fold: Fold[I, O], data: List[I]): O = fold.summarize(
    data
      .map(i => fold.tally(i))
      .foldRight(fold.monoid.empty)((x, y) => fold.monoid.combine(x, y)) // or combineAll
  )

  object sumFold {
    val sum: Fold[Int, Int] {type M = Int} = new Fold[Int, Int] {
      type M = Int

      def tally(i: M): M = i

      def summarize(sum: M): M = sum

      def monoid: Monoid[M] = new Monoid[Int] {
        override def empty: Int = 0

        override def combine(x: Int, y: Int): Int = x + y
      }
    }
  }

  object wordCount {
    val sum: Fold[String, Map[String, Int]] {type M = Map[String, Int]} = new Fold[String, Map[String, Int]] {
      type M = Map[String, Int]

      def tally(s: String): M = Map(s -> 1)

      def summarize(sum: M): M = sum

      def monoid: Monoid[M] = new Monoid[Map[String, Int]] {
        override def empty: Map[String, Int] = Map("" -> 0)

        override def combine(x: Map[String, Int], y: Map[String, Int]): Map[String, Int] =
          if (x.isEmpty) y
          else if (y.isEmpty) x
          else x ++ y.map { case (k, v) => k -> (v + x.getOrElse(k, 0)) }
      }
    }
  }

  object max {
    val max: Fold[Int, Option[Int]] {type M = Option[Int]} = new Fold[Int, Option[Int]] {
      type M = Option[Int]

      def tally(i: Int): M = Some(i)

      def summarize(sum: M): M = sum

      def monoid: Monoid[M] = optionIntMonoid
    }
  }

  object average {
    val average: Fold[Int, Option[Double]] {type M = Option[Double]} = new Fold[Int, Option[Double]] {
      type M = Option[Double]

      def tally(i: Int): M = Some(i.toDouble)

      def summarize(sum: Option[Double]): Option[Double] = sum

      def monoid: Monoid[Option[Double]] = optionDoubleMonoid
    }
  }

  //  def sum(xs: List[Int]): Option[Int] = Some(xs.foldRight(0)((a, b) => a + b))
  //
  //  def max(xs: List[Int]): Option[Int] = Some(xs.foldRight(0)((a, b) => if (a > b) a else b))
  //
  //  def average(xs: List[Int]): Option[Int] = Some(xs.foldRight(0)((a, b) => (a + b) / 2))
  //
  //  def count(xs: List[String]): Map[String, Int] = ???

}

object Main extends App {

  Fold.runList(sumFold.sum, List(1, 2, 3, 4))

}