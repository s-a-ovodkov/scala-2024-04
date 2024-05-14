package sa.ovodkov.task01

import scala.annotation.tailrec

object list {

  /**
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
   */
  sealed trait List[+T] {

    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     */
    def ::[TT >: T](el: TT): List[TT] = list.::(el, this)

    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     */
    def mkString(str: String): String = {
      @tailrec
      def loop(l: List[T], strBuilder: StringBuilder = new StringBuilder()): StringBuilder = l match {
        case list.::(head, list.Nil) => strBuilder.append(head)
        case list.::(head, tail) => loop(tail, strBuilder.append(head).append(str))
      }

      loop(this).toString()
    }

    /**
     * Метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[T] = {
      @tailrec
      def loop(l: List[T], result: List[T] = list.Nil): List[T] = l match {
        case list.::(head, list.Nil) => list.::(head, result)
        case list.::(head, tail) => loop(tail, list.::(head, result))
      }

      loop(this)
    }

    /**
     * Метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[TT](f: T => TT): List[TT] = this match {
      case list.::(head, list.Nil) => List(f(head))
      case list.::(head, tail) => list.::(f(head), tail.map(f))
    }

    /**
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter(f: T => Boolean): List[T] = this match {
      case list.::(head, tail) if f(head) => list.::(head, tail.filter(f))
      case list.::(_, tail) => tail.filter(f)
      case Nil => Nil
    }
  }

  case class ::[T](head: T, tail: List[T]) extends List[T]

  case object Nil extends List[Nothing]

  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   */
  object List {

    def apply[A](v: A*): List[A] = if (v.isEmpty) Nil else ::(v.head, apply(v.tail: _*))

    /**
     * Функция incList принимает список Int и возвращать список, где каждый элемент будет увеличен на 1
     */
    def incList(list: List[Int]): List[Int] = list.map(_ + 1)

    /**
     * Функцию shoutString принимает список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'
     */
    def shoutString(list: List[String]): List[String] = list.map(x => s"!$x")
  }
}
