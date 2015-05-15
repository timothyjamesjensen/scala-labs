package org.scalalabs.basic.lab01
import scala.language.implicitConversions
/**
 * The goal of this exercise is to get familiar basic OO constructs in scala
 *
 * Fix the code so that the unit test 'CurrencyExerciseTest' passes.
 *
 * In order for the tests to pass you need to do the following:
 * 
 * Exercise 1:
 * - Create a class Euro
 * - Provide it with two constructor parameters: euro:Int, cents:Int
 * - Provide the cents field with default value: 0
 * - Provide an immutable field named: inCents that converts euro + cents into cents.
 * - Create an object Euro with a factory method named: fromCents that creates an Euro based on cents.
 * - Create a method named: + to the Euro class that adds another Euro
 * - Create a method named: * to the Euro class that multiplies an Euro
 * 
 * Exercise 2:
 * - Create an abstract class Currency
 * - Provide it with one constructor parameter: symbol:String
 * - Extend the previously created Euro class from Currency
 * - Override the toString method of Euro to represent the following String:
 *   -> symbol + ': ' + euro + ',' + cents.  E.g: EUR 200,05
 * - In case the cents are 0 use this representation:
 *   -> symbol + ': ' + euro + ',--. E.g.: EUR 200.--
 *   
 * Exercise 3:
 * - Mix the Ordered trait in Euro
 * - Implement the compare method  
 * 
 * Exercise 4:
 * - Provide an implicit class that adds a *(euro:Euro) method to Int
 * - Create a new currency Dollar
 * - Provide a implicit conversion method that converts from Euro to Dollar using the 
 *   [[org.scalalabs.basic.lab01.DefaultCurrencyConverter]]
 * 
 * Exercise 5:
 * - Extend the conversion method from Euro to Dollar with an implicit parameter 
 *   of type [[org.scalalabs.basic.lab01.CurrencyConverter]]
 * - Use the implicit CurrencyConverter to do the conversion. 
 */
abstract class Currency(val symbol: String)

class Euro(e: Int, c: Int = 0) extends Currency("EUR") with Ordered[Euro] {
  val euro = e
  val cents = c
  val inCents = euro * 100 + cents //(e.toString + c.toString).toInt

  def +(other: Euro) = Euro.fromCents(inCents + other.inCents)
  def *(x: Int) = Euro.fromCents(inCents * x)

  override val toString = s"$symbol: $euro,${if (cents > 0) f"$cents%02d" else "--"}"

  override def compare(that: Euro) = inCents - that.inCents
}

object Euro {
  def fromCents(c: Int): Euro = new Euro(c / 100, c % 100)

  implicit class EuroInt(val i: Int) extends AnyVal {
    def *(euro: Euro) = euro * i
  }

  implicit def convert(dollar: Dollar)(implicit converter: CurrencyConverter):
    Euro = Euro.fromCents(converter.toEuroCents(dollar.inCents))
}

class Dollar(d: Int, c: Int = 0) extends Currency("USD") {
  val dollar = d
  val cents = c

  def inCents = dollar * 100 + cents
}