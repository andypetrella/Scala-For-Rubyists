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
  //very simple IO monad
  // >> hardly inspired by this great talk (http://www.youtube.com/watch?v=1gZAqJA2pEk) given by Paul Chiusano

  trait Action[+A] { self =>
    def run:A
    def map[B](f:A=>B) = new Action[B] {
      def run = f(self.run)
    }
    def flatMap[B](f:A=>Action[B]):Action[B] = new Action[B] {
      def run = f(self.run).run
    }
  }

  object Action {
    def PrintLn(msg:String) = new Action[Unit] {
      def run = println(msg)
    }
    def ReadLine = new Action[String] {
      def run = readLine
    }
    def ReadLines(f:String) = new Action[List[String]] {
      def run = {
        val s = scala.io.Source.fromURL(f)
        val l = s.getLines().toList
        s.close()
        l
      }
    }
  }

  import Action._
  def echo = for {
    _ <- PrintLn("Enter an echo message")
    s <- ReadLine
    _ <- PrintLn(s)
  } yield ()

  echo.run

  def printlnInc = for {
    _ <- PrintLn("fetching ints")
    l <- ReadLines(talk.getClass.getResource("/ints.txt").toURI.toString)
    _ <- PrintLn(l.mkString(" -- "))
    _ <- PrintLn("adding 1")
    _ <- PrintLn(l.map(_.toInt + 1).mkString("\n"))
  } yield ()

  printlnInc.run


  // Facets


}