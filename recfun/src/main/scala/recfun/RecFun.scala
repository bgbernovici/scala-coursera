package recfun

import java.util.LinkedList

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if r == 0 then 1 
    else if c == 0 then 1
    else if r == c then 1
    else pascal(c-1, r-1) + pascal(c, r-1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    def recur(str: List[Char], stack: Iterable[Char]): Boolean = 
      if str.isEmpty && stack.isEmpty then true
      else if str.isEmpty && !stack.isEmpty then false
      else if str.head == '(' then recur(str.tail, stack ++ Seq(str.head))
      else if str.head == ')' && !stack.isEmpty && stack.last == '(' then recur(str.tail, stack.dropRight(1))
      else if str.head == ')' then recur(str.tail, stack ++ Seq(str.head))
      else recur(str.tail, stack)
    recur(chars, Seq())

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if money == 0  then 1
    else if money < 0 then 0
    else if coins.isEmpty then 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)

