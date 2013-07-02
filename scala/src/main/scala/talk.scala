package be.wajug

object talk extends App {

  //Structures
  case class User(first:String, last:String, age:Int, gender:Gender)
  sealed trait Gender
  case object Male extends Gender
  case object Female extends Gender

  val noootsab = User("andy", "petrella", 31, Male)
  val sandrine = noootsab.copy(first="sandrine", gender=Female)
  val noah = noootsab.copy(first="noah", age=3)

  val petrellas = Seq(noah, sandrine, noootsab)
  val ordered = petrellas
                  .sortBy(_.first)
                  .map{p => p.copy(first=p.first.head.toString.toUpperCase+p.first.tail)}



  //Composable Behaviors
}