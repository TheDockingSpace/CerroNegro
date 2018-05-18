import cats.Semigroup
import cats.instances.all._
import cats.syntax.semigroup._

val map1 = Map("hello" -> List(0), "world" -> List(1))
val map2 = Map("hello" -> List(2), "cats"  -> List(3))

Semigroup[Map[String, List[Int]]].combine(map1, map2)

map1 |+| map2