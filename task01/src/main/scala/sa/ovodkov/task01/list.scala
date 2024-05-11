package sa.ovodkov.task01

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
    def ::[TT >: T](el: TT): List[TT] = ???

    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     */
  }

  case class ::[T](head: T, tail: List[T]) extends List[T]

  case object Nil extends List[Nothing]

  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   */
  object List {
    def apply[A](v: A*): List[A] = if (v.isEmpty) Nil else ::(v.head, apply(v.tail: _*))
  }

  /**
   * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
   */

  /**
   * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
   */


  /**
   * Реализовать метод filter для списка который будет фильтровать список по некому условию
   */

  /**
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */


  /**
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
}
