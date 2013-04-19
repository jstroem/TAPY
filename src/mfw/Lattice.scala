package tapy.mfw

import tapy.cfg._

trait Lattice[T] {
  def top : T
  def bottom : T

  def compare(a: T, b: T): Boolean
  def greatestLowerBound(a: T, b: T): T
  def leastUpperBound(a: T, b: T): T
}
