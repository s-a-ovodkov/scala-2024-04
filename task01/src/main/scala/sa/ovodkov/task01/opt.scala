package sa.ovodkov.task01

object opt {

  sealed trait Option[+T] {
    def isEmpty: Boolean = this match {
      case None => true
      case Some(v) => false
    }

    def get: T = this match {
      case None => throw new Exception("get on empty option")
      case Some(v) => v
    }

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    def map[B](f: T => B): Option[B] = flatMap(t => Option(f(t)))
  }

  case class Some[T](v: T) extends Option[T]

  case object None extends Option[Nothing]

  object Option {
    def apply[T](v: T): Option[T] = if (v != null) Some(v) else None
  }

  /**
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */
}
