trait Feature

trait Number extends Feature 
object One extends Number { override def toString = "One" }
object Two extends Number { override def toString = "Two" }
object Three extends Number { override def toString = "Three" }

trait Color extends Feature
object Red extends Color { override def toString = "Red" }
object Green extends Color { override def toString = "Green" }
object Purple extends Color { override def toString = "Purple" }

trait Shape extends Feature
object Oval extends Shape { override def toString = "Oval" }
object Diamond extends Shape { override def toString = "Diamond" }
object Squiggle extends Shape { override def toString = "Squiggle" }

trait Fill extends Feature
object Empty extends Fill { override def toString = "Empty" }
object Half extends Fill { override def toString = "Half" }
object Full extends Fill { override def toString = "Full" }


case class Card(n: Number, c: Color, s: Shape, f: Fill) {
	override def toString = s"$n $c $s $f"
	def <(c2: Card) = this.toString < c2.toString
}

object Set {
	import scala.util.Random
	val numbers = Seq(One, Two, Three)
	val colors = Seq(Red, Green, Purple)
	val shapes = Seq(Oval, Diamond, Squiggle)
	val fills = Seq(Empty, Half, Full)

	val deck = for {
		n <- numbers
		c <- colors
		s <- shapes
		f <- fills
	} yield Card(n, c, s, f)

	def isSet(c1: Card, c2: Card, c3: Card) = {
		val cs = Seq(c1, c2, c3)
		val rawVals = Seq(cs.map(_.n), cs.map(_.c), cs.map(_.s), cs.map(_.f))
		rawVals.forall(_.distinct.length != 2)
	}

	def completeSet(c1: Card, c2: Card) = deck.find(isSet(_, c1, c2)).get

	def isAntiSet(c1: (Card, Card), c2: (Card, Card)) = completeSet(c1._1, c1._2) == completeSet(c2._1, c2._2)

	def table(n: Int) = Random.shuffle(deck).take(n)

	def findSets(t: Seq[Card]) = for {
		c1 <- t
		c2 <- t
		c3 <- t
		if c1 < c2
		if c2 < c3
		if isSet(c1, c2, c3)
	} yield (c1, c2, c3)

	def findAntiSets(t: Seq[Card]) = for {
		c1 <- t
		c2 <- t
		c3 <- t
		c4 <- t
		if c1 < c2
		if c3 < c4
		if c1 < c3
		if isAntiSet((c1, c2), (c3, c4))
	} yield (c1, c2, c3, c4)

	def testSets(n: Int = 12,  i: Int = 1000) = (1 to i).filter(_ => Set.findSets(Set.table(n)).nonEmpty).length / (i + 0.0)
	def testAntiSets(n: Int = 12, i: Int = 1000) = (1 to i).filter(_ => Set.findSets(Set.table(n)).nonEmpty).length / (i + 0.0)
}

//Set.testSets(12, 1000) = 0.966
//Set.testAntiSets(12, 1000) = 0.972
//Set.testAntiSets(12, 1000)


