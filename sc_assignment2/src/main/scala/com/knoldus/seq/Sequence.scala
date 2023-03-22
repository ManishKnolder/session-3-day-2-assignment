package com.knoldus.seq

class Sequence {

  private var sequenceString: Seq[String] = Seq.empty[String]
  private var sequenceInt: Seq[Int] = Seq.empty[Int]

  // Check the type of element, if it is int save it to sequenceInt, if it is string save it to sequenceString, else return IllegalArgumentException,  return the size
  def store(element: Any): Int = {
    element match {
      case element: String => {
        sequenceString = sequenceString :+ element
        sequenceString.size
      }
      case element: Int => {
        sequenceInt = sequenceInt :+ element
        sequenceInt.size
      }
      case _ => throw new IllegalArgumentException()
    }

  }

  // Check the type of element, if it is int remove it from sequenceInt, if it is string remove it from sequenceString, else return IllegalArgumentException,  return the size
  def removeElement(element: Any): Int = {
    println(sequenceInt)
    element match {
      case element: String => {
        val data = element
        sequenceString = sequenceString.filterNot(element => element == data)
        sequenceString.size
      }
      case element: Int => {
        val data = element
        sequenceInt = sequenceInt.filterNot(element => element == data)
        println(sequenceInt)
        sequenceInt.size
      }
      case _: Any => throw new IllegalArgumentException()
    }
  }
}
