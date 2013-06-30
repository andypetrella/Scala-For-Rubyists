package be.wajug

object talk extends App {

  val argsList:List[String] = args.toList

  argsList foreach println

  val lengthsList = argsList map (x => x.length)
  lengthsList foreach println

  val l = lengthsList.sum
  println(l)

  //def append(del:String)(x:String, y:String) = x + del + y
  //val a : String => (String, String) => String = del => (x, y) => x + del + y

  val aL = argsList reduce ((x,y) => x + " " + y)
  println(aL)


  println(argsList mkString " ")

  case class Dev(firstName:String, lastName:String, languages:Set[Language])

  //class Language(val name:String)
  trait Language {
    def name: String
    //def paradigms:Set[Paradigm]
  }
  case object Scala extends Language with Functional with OO {
    val name = "Scala"
    //val paradigms = Set(Functional, OO)

    override def toString:String = name + " is Functional and OO"
  }
  case object Java extends Language with OO {
    val name = "Java"
    //val paradigms = Set(OO)
  }
  case object CoffeeScript extends Language with Prototypal with OO {
    val name = "CoffeeScript"
    //val paradigms = Set(Prototypal, OO)
  }

  case object Haskell extends Language with Functional {
    val name = "Haskell"
  }

  trait Paradigm {
    //def rules:Set[String]
  }
  trait Functional extends Paradigm {
    //def ....
  }
  trait OO extends Paradigm {
    // ...
  }
  trait Prototypal extends Paradigm {
    // ...
  }



  val noootsab = /*new*/ Dev("andy", "petrella", Set(Scala, Java, CoffeeScript))

  println(noootsab)

  val beefedUpNoootsab = noootsab.copy(languages = noootsab.languages + Haskell)

  println(beefedUpNoootsab)

  //good for concurrency ... the shared resource problem is not a problem but not resolved as well... akka




}