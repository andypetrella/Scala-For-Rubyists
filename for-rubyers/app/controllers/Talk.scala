package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._

import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Promise
import play.api.libs.ws.WS
import play.api.libs.ws.Response
import play.api.libs.json._
import play.api.Play.current

object Talk extends Controller {

  def enter = Action { request =>
    val who = for {
      whos        <- request.queryString.get("who")
      index       <- request.queryString.get("index").flatMap{ _.headOption.map{_.toInt} } orElse Some(0)
      who         <- try { Some(whos(index)) } catch { case e:IndexOutOfBoundsException => None }
      nonEmptyWho <- if (who.trim.length == 0) None else Some(who)
    } yield nonEmptyWho

    Ok("Hewo "+who.getOrElse("Doctor")+"!")
  }




  def fileString = {
    val f = io.Source.fromFile(current.getFile("../index.html"))
    val o = java.io.File.createTempFile("ex-work", ".html")
    val w = new java.io.FileWriter(o)
    f .getLines
      .mkString
      .toUpperCase
      .split("\\s+")
      .groupBy(_.head)
      .toList
      .sortBy(_._1)
      .foreach {case (letter, words) =>
        w.append(letter.toString.padTo(26, "*").mkString).append("\n")
        words.foreach{wd => w.append(wd + "\n")}
      }
    f.close
    w.flush
    w.close
    val or = io.Source.fromFile(o)
    val s = or.getLines.mkString
    or.close
    s
  }



  def async = Action {
    Async {
      Akka.future {
        val s = System.currentTimeMillis
        val result =
          (1 to scala.util.Random.nextInt(10))
            .map{ _ => fileString }
            .mkString("\n")
        Ok(JsObject(Seq(
          "time" -> JsNumber(System.currentTimeMillis - s),
          "result" -> JsString(result)
        )))
      }
    }
  }

  def stress = Action {
    val s = System.currentTimeMillis
    Async {
      Promise.sequence(List.fill(1000)(WS.url("http://localhost:9000/async").get)).map { list =>
        val jsons = JsArray(list.map{_.json})
        val time = (jsons \\ "time").map{_.as[Long]}.sum

        Ok(JsObject(Seq(
          "stress" -> JsNumber(System.currentTimeMillis - s),
          "time" -> JsNumber(time),
          "response" -> jsons
        )))
      }
    }
  }

  def manualStress = Action {
    Ok(views.html.talk())
  }

}