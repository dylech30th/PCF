package ink.sora

import PCF.Zero

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(n => !primes.takeWhile(_ <= math.sqrt(n)).exists(n % _ == 0))

def godelNumber(list: List[BigInt]): BigInt =
  val givenPrimes = primes.take(list.size).toList
  list.zipWithIndex.map { case (i, index) => bigPow(givenPrimes(index), i + 1)}.product

def bigPow(base: BigInt, exp: BigInt): BigInt =
  var e = exp
  var res = BigInt(1)
  while e > 0 do
    res = res * base
    e = e - 1
  res

// decodes a given godel number into a list of primes raised to powers of corresponding numbers
def decode(bigInt: BigInt): List[Int] =
  var n = bigInt
  val map = mutable.TreeMap[Int, Int]()
  while n % 2 == 0 do
    map(2) = map.getOrElse(2, 0) + 1
    n = n / 2

  var i = BigInt(3)
  while i <= n.bigInteger.sqrt() do
    while n % i == 0 do
      map(i.toInt) = map.getOrElse(i.toInt, 0) + 1
      n = n / i
    i = i + 2

  if n >= 2 then map(n.toInt) = map.getOrElse(n.toInt, 0) + 1
  map.values.map(_ - 1).toList

enum PCF(val arity: Int):
  case Zero extends PCF(0)
  case Successor extends PCF(1)
  // n: count of parameters, i: starts from zero
  case Projection(n: Int, i: Int) extends PCF(n)
  case Composition(outer: PCF, inners: List[PCF]) extends PCF(inners.head.arity)
  case PR(base: PCF, induction: PCF) extends PCF(base.arity + 1)

def validate(pcf: PCF): Unit =
  pcf match
    case PCF.Zero => ()
    case PCF.Successor => ()
    case PCF.Projection(n, i) =>
      if n <= 0 then throw IllegalArgumentException("n in Projection must be bigger than 0")
      else if (0 until n).contains(i) then throw IllegalArgumentException("i in Projection must be in range 0 until n")
    case PCF.Composition(outer, inners) =>
      if inners.map(_.arity).toSet.size != 1 then throw IllegalArgumentException("all inner functions must have the same arity")
      else if outer.arity != inners.size then throw IllegalArgumentException("outer function must have the same arity as the inners' size")
      else inners.foreach(validate)
    case PCF.PR(base, induction) =>
      if induction.arity != base.arity + 2 then throw IllegalArgumentException("induction function must have the same arity as the base function plus 2")
      else validate(base); validate(induction)

def encodePCF(pcf: PCF): BigInt =
  pcf match
    case PCF.Zero => 0
    case PCF.Successor => 1
    case PCF.Projection(n, i) => godelNumber(List(2, n, i).map(BigInt(_)))
    case PCF.Composition(outer, inners) => godelNumber(List(BigInt(3), BigInt(inners.size), BigInt(outer.arity), encodePCF(outer)) ++ inners.map(encodePCF))
    case PCF.PR(base, induction) => godelNumber(List(BigInt(4), BigInt(base.arity), encodePCF(base), encodePCF(induction)))

def decodePCF(n: BigInt): PCF =
  if n == 0 then return PCF.Zero
  else if n == 1 then return PCF.Successor
  val list = decode(n)
  list.head match
    case 2 if list.size == 3 => PCF.Projection(list(1), list(2))
    case 3 if list.size > 4 && list.size == 4 + list(1) => PCF.Composition(decodePCF(list(3)), list.drop(4).map(BigInt(_)).map(decodePCF))
    case 4 if list.size == 4 => PCF.PR(decodePCF(list(2)), decodePCF(list(3)))
    case _ => PCF.Zero

@main
def main(): Unit =
  import PCF.*
  val pcf = Composition(Successor, List(Projection(3, 2)))
  val num = encodePCF(pcf)
  println(decodePCF(num))
