package be.wajug

package talk {

  //Structures
  case class User(first:String, last:String, age:Int, gender:Gender)
  sealed trait Gender
  case object Male extends Gender
  case object Female extends Gender

  object users {
    val noootsab = User("andy", "petrella", 31, Male)
    val sandrine = noootsab.copy(first="sandrine", gender=Female)
    val noah = noootsab.copy(first="noah", age=3)

    val petrellas = Seq(noah, sandrine, noootsab)
    val ordered = petrellas
                    .sortBy(_.first)
                    .map{p => p.copy(first=p.first.head.toString.toUpperCase+p.first.tail)}    
  }

  import users._

  object Ex1 extends App {
    println(petrellas)    
    readLine
    println(ordered)    
  }


  //********************************************************

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

  object Ex2 extends App {
    import Action._

    def echo = for {
      _ <- PrintLn("Enter an echo message")
      s <- ReadLine
      _ <- PrintLn(s)
    } yield ()

    println(" ** Echo ** ")
    echo.run

    readLine

    def printlnInc = for {
      _ <- PrintLn("fetching ints")
      l <- ReadLines(Ex2.getClass.getResource("/ints.txt").toURI.toString)
      _ <- PrintLn(l.mkString(" -- "))
      _ <- PrintLn("adding 1")
      _ <- PrintLn(l.map(_.toInt + 1).mkString("\n"))
    } yield ()

    println(" ** Inc_s ** ")
    printlnInc.run
  }



  //********************************************************


  // Facets 

  case class Company(name:String)
  case class Stuff(nah:Int = 0)

  trait Ser[A,B] {
    def apply(a:A):Option[B]
  }

  object Ser {
    type URLEncoded = Map[String, String]
    type Json = String


    def apply[A,B](f: A => Option[B]) = new Ser[A,B] { def apply(a:A) = f(a) }

    implicit val userURLEncSer:Ser[User, URLEncoded] = Ser { u => Some(Map("first" -> u.first, "last" -> u.last)) }
    implicit val companyURLEncSer:Ser[Company, URLEncoded] = Ser { c => Some(Map("name" -> c.name)) }

    
    def magicalJsonSer[A](a:A) = s"""{
                                 |  "obj": "$a"
                                 |}""".stripMargin
    implicit def jsonSer[A]:Ser[A, Json] = Ser[A, Json] { x => Some(magicalJsonSer(x)) }

    def ser[A,B](a:A)(implicit s:Ser[A,B]):Option[B] = s(a)

    def toJson[A](a:A)(implicit s:Ser[A,Json]) = ser[A,Json](a)
    def toURLEncoded[A](a:A)(implicit s:Ser[A,URLEncoded]) = ser[A,URLEncoded](a)
  }

  object Ex3 extends App {

    import Ser._

    println(" ** User ser ** ")
    toJson(noah).foreach(println)
    readLine
    toURLEncoded(sandrine).foreach(println)

    val nextlab = Company("NextLab")
    readLine
    println(" ** Company ser ** ")
    toJson(nextlab).foreach(println)
    readLine
    toURLEncoded(nextlab).foreach(println)


    readLine
    println(" ** Stuff ser ** ")
    toJson(Stuff()).foreach(println)            // ==> compiles
    //println(toURLEncoded(Stuff()))    // ==> won't compile ==> cannot find implicit

    type Bin = Byte
    //println(ser[Company, Bin](nextlab)) // ==> won't compile ==> cannot find implicit
    
  }

  trait Protocol {
    type O

    // can throw exception for illustration purpose ONLY => use Either instead
    def write[A](a:A)(implicit s:Ser[A,O]):O = s(a).getOrElse(throw new RuntimeException(s"cannot write: $a")) 
    
  }

  trait UserService extends Protocol {
    def send(implicit s:Ser[User,O]) = {
      //business logic to create User
      // ...
      // ...
      val u = User("a", "b", 1, Male)
      write(u)
    }
  }

  //entering the Json world

  object JsonWorld {
    //import the context
    import Ser._

    //could be generalized one level further... World[S] => World[Json] => service:Service
    object JsonUserService extends UserService { type O = Json }

    def doStuff = println(JsonUserService.send)
  }
  
  object Ex4 extends App {
    println(" ** doStuff ** ")
    JsonWorld.doStuff    
  }




  //********************************************************
  
  // By-Name params

  object Ex5 extends App {
    // By-Name
    def byName[A](a: => A) {
      println("before")
      a
      println("after")
    }

    byName(println("NOW")) // "NOW" is not printed now... ^^
  }

  /*Stream*/
  trait LazySeq[+A] {
    import LazySeq._

    def uncons:Option[(A, LazySeq[A])]
    
    def takeWhile(p:A=>Boolean):LazySeq[A] = 
      uncons.map{ case (x, xs) => 
                  if (!p(x)) empty 
                  else cons(x, xs.takeWhile(p)) 
                }
            .getOrElse(empty)
    
    def toList:List[A] = uncons.map{ case (x, xs) => x :: xs.toList}.getOrElse(Nil)
  }
  object LazySeq {
    def empty[A] = new LazySeq[A] {
      val uncons = None.asInstanceOf[Option[(A, LazySeq[A])]]
    }

    def cons[A](a: =>A, s: => LazySeq[A]) = new LazySeq[A] {
      lazy val uncons = Some((a, s))
    }
  }

  object Ex6 extends App {
    import LazySeq._

    def from(n:Int):LazySeq[Int] = cons(n, from(n+1))
    val naturals = from(0)

    println(" ** Streaming ** ")
    println(naturals.takeWhile(_ < 10))
    println(naturals.takeWhile(_ < 10).toList)
  }



  //********************************************************
  
  // Future
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  object Ex7 extends App {
      
    def f1:Future[Int] = future {
      println("******* Enter int ***********")
      val s = readLine
      s.toInt
    }

    def f2(s:String):Future[Unit] = future {
      println(s"Printing... $s")
    }


    //define the workflow by composing tasks
    
    args(0) match {

      case "imp" => 
        println("Start")
        val i = f1
        println("Fetching int")
        
        val s = i.map(x => s" --> $x <--" )
        println("Convert to String")
        
        s.flatMap(f2)
        println("Print it")      

      case "for" =>
        val f3 = for  {
          i <- f1
          val s = s" ++ $i ++"
          () <- f2(s)
        } yield i

        f3.onSuccess { case x => println(s"Entered number :> $x") }      
    }

  }


  //********************************************************

  //pimp
  object Pimps {
    implicit class WithJson[A](a:A) {
      import Ser._
      def toJson = Ser.toJson(a).getOrElse(throw new RuntimeException("bang!"))
    }
    implicit class WithURLEncoded[A](a:A) {
      import Ser._
      def toURLEncoded(implicit s:Ser[A,URLEncoded]) = Ser.toURLEncoded(a).getOrElse(throw new RuntimeException("bang!"))
    }
  }

  object TestUtils {
    implicit class PP[A](a:A) {
      def pp = {
        println(a)
        a
      }
    }
  }

  object Ex8 extends App {
    
    // WON'T compile => the syntaxic context cannot dispatch toJson to noootsab here
    //println(noootsab.toJson)     

    //import the 
    import Pimps.WithJson
    // ok becasue the syntaxic context has a conversion from User to somthing to which toJson can be dispatched
    println(noootsab.toJson)       

    readLine

    // guess why?
    //println(noootsab.toURLEncoded)

    import Pimps.WithURLEncoded
    println(noootsab.toURLEncoded)


    readLine
    import TestUtils._
    println("ok".pp + " dude")
    
  }
}