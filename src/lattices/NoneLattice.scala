package tapy.sign

import tapy.mfw._

abstract class Sign
case class QuestionMark() extends Sign
case class Plus() extends Sign
case class Zero() extends Sign
case class Minus() extends Sign
case class Bottom() extends Sign
//case class One extends Sign
//case class PlusZero extends Sign
//case class MinusZero extends Sign


class SignLattice extends Lattice[Sign] {
  def top: Sign = QuestionMark()
  def bottom: Sign = Bottom()
  
  // a >= b
  def compare(a: Sign, b: Sign) = (a, b) match {
    case (QuestionMark(), _)  => true

    case (Plus(), Plus())     => true
    case (Plus(), Bottom())   => true
    case (Plus(), _)          => false

    case (Minus(), Minus())   => true
    case (Minus(), Bottom())  => true
    case (Minus(), _)         => false

    case (Zero(), Zero())     => true
    case (Zero(), Bottom())   => true
    case (Zero(), _)          => false

    case (Bottom(), Bottom()) => true
    case (Bottom(), _)        => false
  }

  def leastUpperBound(a: Sign, b: Sign) = (a, b) match {
    case (QuestionMark(), _) => QuestionMark()

    case (Plus(), Plus())    => Plus()
    case (Plus(), Bottom())  => Plus()
    case (Plus(), _)         => QuestionMark()

    case (Minus(), Minus())  => Minus()
    case (Minus(), Bottom()) => Minus()
    case (Minus(), _)        => QuestionMark()

    case (Zero(), Zero())    => Zero()
    case (Zero(), Bottom())  => Zero()
    case (Zero(), _)         => QuestionMark()

    case (Bottom(), _)       => b
  }

  def greatestLowerBound(a: Sign, b: Sign) = (a, b) match {
    case (QuestionMark(), _)       => b

    case (Plus(), Plus())          => Plus()
    case (Plus(), QuestionMark())  => Plus()
    case (Plus(), _)               => Bottom()

    case (Minus(), Minus())        => Minus()
    case (Minus(), QuestionMark()) => Minus()
    case (Minus(), _)              => Bottom()

    case (Zero(), Zero())          => Zero()
    case (Zero(), QuestionMark())  => Zero()
    case (Zero(), _)               => Bottom()

    case (Bottom(), _)             => Bottom()
  }
}
