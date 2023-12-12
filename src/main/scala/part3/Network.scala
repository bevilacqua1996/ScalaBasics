package part3

import scala.annotation.tailrec

object Network extends App {

  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    network + (person -> Set())

  def friend(network: Map[String, Set[String]], personA: String, personB: String): Map[String, Set[String]] =
    val friendsA = network(personA)
    val friendsB = network(personB)

    network + (personA -> (friendsA + personB)) + (personB -> (friendsB + personA))

  def unfriend(network: Map[String, Set[String]], personA: String, personB: String): Map[String, Set[String]] =
    val friendsA = network(personA)
    val friendsB = network(personB)

    network + (personA -> (friendsA - personB)) + (personB -> (friendsB - personA))

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    @tailrec
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]) : Map[String, Set[String]] = {
      if(friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(networkAcc, person, friends.head))
    }

    val unfriended = removeAux(network(person), network)
    unfriended - person
  }

  def nFriends(network: Map[String, Set[String]], person: String): Int =
    if(!network.contains(person)) 0
    else network(person).size

  def mostFriends(network: Map[String, Set[String]]): String =
    network.maxBy(pair => pair._2.size)._1

  def nPeopleWithNoFriends(network: Map[String, Set[String]]): Int =
    network.count(pair => pair._2.isEmpty)

  def socialConnection(network: Map[String, Set[String]], personA: String, personB: String): Boolean = {
    @tailrec
    def bfs(target: String, consideredPeople: Set[String], discoveredPeople: Set[String]): Boolean = {
      if(discoveredPeople.isEmpty) false
      else {
        val person = discoveredPeople.head
        if(person == target) true
        else if(consideredPeople.contains(person)) bfs(target, consideredPeople, discoveredPeople.tail)
        else bfs(target, consideredPeople + person, discoveredPeople ++ network(person))
      }
    }

    bfs(personB, Set(), network(personA) + personA)
  }

  val empty: Map[String, Set[String]] = Map()
  val network = add(add(empty, "Bob"), "Mary")

  println(network)
  println(friend(network, "Bob", "Mary"))
  println(unfriend(friend(network, "Bob", "Mary"), "Bob", "Mary"))
  println(remove(friend(network, "Bob", "Mary"), "Bob"))

  val people = add(add(add(empty, "Bob"), "Mary"), "Jim")
  val jimBob = friend(people, "Bob", "Jim")
  val testNet = unfriend(friend(jimBob, "Bob", "Mary"), "Bob", "Jim")

  println(testNet)
  println(nPeopleWithNoFriends(testNet))

  println(socialConnection(testNet, "Mary", "Jim"))
  println(socialConnection(testNet, "Mary", "Bob"))

}
