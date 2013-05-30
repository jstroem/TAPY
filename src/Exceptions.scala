package tapy.exceptions

import org.python.antlr.ast.unaryopType

class InternalErrorException(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
	def this() = this("", null)
	def this(message: String) = this(message, null)
	def this(nestedException : Throwable) = this("", nestedException)
}

class NotImplementedException(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
	def this() = this("", null)
	def this(message: String) = this(message, null)
	def this(nestedException : Throwable) = this("", nestedException)
}

class UnaryException(message: String, op: unaryopType, nestedException: Throwable) extends Exception(message, nestedException) {
	def this(op: unaryopType) = this("", op, null)
	def this(message: String,op: unaryopType) = this(message, op, null)
	def this(op: unaryopType, nestedException : Throwable) = this("", op, nestedException)
}

class TypeError(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
  def this() = this("", null)
  def this(message: String) = this(message, null)
  def this(nestedException : Throwable) = this("", nestedException)
}

class NameError(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
  def this() = this("", null)
  def this(message: String) = this(message, null)
  def this(nestedException : Throwable) = this("", nestedException)
}

class UnexpectedValueException(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
  def this() = this("", null)
  def this(message: String) = this(message, null)
  def this(nestedException : Throwable) = this("", nestedException)
}

class AttributeError(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
  def this() = this("", null)
  def this(message: String) = this(message, null)
  def this(nestedException : Throwable) = this("", nestedException)
}