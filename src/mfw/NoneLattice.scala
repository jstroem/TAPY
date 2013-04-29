package tapy.sign

import tapy.mfw._

abstract class None
case class ReallyNone() extends None
class NoneLattice() extends Lattice[None] {
  def top: None = ReallyNone()
  def bottom: None = ReallyNone()
  def compare(a: None, b: None) = true
  def leastUpperBound(a: None, b: None) = a
  def greatestLowerBound(a: None, b: None) = a
}
