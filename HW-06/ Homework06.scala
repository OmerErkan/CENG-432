
import scala.collection.mutable.HashSet
case class User(name: String, email: String, grade: Int)

trait UserManagerTrait {
  def add(name: String, email: String, grade: Int): Option[User]
  def getUser(email: String): Option[User]
  def getUserListForGrade(grade: Int): List[User]
  def getCertainGrades(gradeSelector: (Int) => Boolean): List[String]
}

object UserManager extends UserManagerTrait {
  val userList = HashSet[User]()
  def add(name: String, email: String, grade: Int): Option[User] = {
    if (!userList.exists(user => (user.email == email))) {
      val user = User(name, email, grade);
      userList.add(user)
      Some(user)
    } else {
      None
    }
  }
  def getUser(email: String): Option[User] = {
    try {
      userList.find(user => (user.email == email))
    } catch {
      case e: Exception => None
    }

  }
  def getUserListForGrade(grade: Int): List[User] = {
    userList.filter(user => (user.grade == grade)).toList
  }

  def getCertainGrades(gradeSelector: (Int) => Boolean): List[String] = {
    userList.filter(user => gradeSelector(user.grade)).map(user => user.name).toList
  }
}
