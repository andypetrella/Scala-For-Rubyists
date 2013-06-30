package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  import scala.util.Random.nextInt

  //model
  case class User(firstName:String, lastName:String, age:Int, id:String=java.util.UUID.randomUUID.toString)

  //boiler
  def alpha = ('a' to 'z').toList
  def randomString(n:Int) =
    (1 to n)
      .map {
        _ => alpha(nextInt(26))
      }
      .mkString
  val userList = List.fill(1000)(User(randomString(10), randomString(15), nextInt(114)))


  //welcome
  def index = Action {
    Ok(views.html.index("ff-to-fp"))
  }

  //show users
  /**
   * 1/ group by lastName's first letter => alpha grouping
   * 2/ map each entry in the map (Char -> List[User]) to
   *      an entry Char -> List[List[User]]
   *      with User are sorted by lastName and sliding with a window of size 10 by 10
   */
  def users = TODO


  //user
  /**
   * find the user and match the option
   */
  def user(id:String) = TODO


  //get user by first name
  /**
   * 1/ look in the request for the firstname param
   * 2/ take the first value if any
   * 3/ find the user
   * 4/ map the result
   * hint: use for-comp
   */
  def byFirstName = TODO

}