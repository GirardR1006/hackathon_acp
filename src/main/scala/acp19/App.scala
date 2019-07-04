package acp19

import oscar.cp._

import scala.collection.mutable

/**
 * @author ${user.name}
 */
object App extends CPModel with App {
  val filename = args(0)

  //*****************************
  //* parsing of the input file *
  //*****************************
  val file = new java.io.File(filename)
  val scanner = new java.util.Scanner(file)

  // first line: Instance sizing
  // 45 5 2 3 7
  val T = scanner.nextInt // 1. Number of days (45 in the example)
  val N = scanner.nextInt // 2. Number of roads (5 in the example)
  val C = scanner.nextInt // 3. Number of work centers (2 in the example)
  val W = scanner.nextInt // 4. Number of worksheets (3 in the example)
  val A = scanner.nextInt // 5. Number of activities (7 in the example)

  // perturbation costs on the roads. N lines of the type
  // 0 0:5:2 5:8:1 8:13:1 13:20:4 20:22:5 22:37:2 37:43:4 43:45:3
  // perturbationCost( r )( j ) donne le cout d'une perturbation de la route r au jour j
  val perturbationCost = Array.fill(N,T)(0)
  for (_ <- 0 until N) {
    val roadID = scanner.nextInt // get the road id
    // for every triple, scan it and save it in the perturbationCost array
    for (triple <- scanner.nextLine().split(" ").filter(_ != "")) {
      val Array(start, end, cost) = triple.split(":").map(_.toInt)
      for (t <- start until end) perturbationCost(roadID)(t) = cost
    }
  }

  // work centers. C lines of the type "centerID nbAvailWorkers"
  // 0 182
  // w_v(c) va contenir le nombre de travailleurs du work center
  val c_v = Array.fill(C)(0)
  for (_ <- 0 until C) {
    val (centerID, nbAvailWorkers) = (scanner.nextInt, scanner.nextInt)
    c_v(centerID) = nbAvailWorkers
  }

  // worksheets
  case class Worksheet(workcenterID: Int, mandatory: Boolean, importance: Int, est: Int, lst: Int, duration: Int,
    roads: Array[Int], nbWorkers: Array[Int])

  var worksheets = Array[Worksheet]()
  for (i <- 0 until W) {
    // 1. ID — The id of the worksheet
    val id = scanner.nextInt
    assert(id == i) // check that nothing went wrong in the parsing
    // 2. Work center ID — The id of the work center used by activities in the worksheet
    val workcenterId = scanner.nextInt
    // 3. Mandatory — 1 if the worksheet is forced to be executed, 0 if it is optional
    val mandatory = scanner.nextInt == 1
    // 4. Importance — The importance value associated to the execution of the worksheet
    val importance = scanner.nextInt
    // 5. EST — The earliest starting time at which the worksheet can begin
    val est = scanner.nextInt
    // 6. LST — The latest starting time at which the worksheet can begin
    val lst = scanner.nextInt
    // 7. Duration — The total number of successive tasks (hence of days) the worksheet is long
    val duration = scanner.nextInt
    // 8. Road IDs — The next duration numbers each represent, in order, the id of the road that
    // is used each day
    val roads = Array.fill(duration)(scanner.nextInt())
    // 9. Amounts of Workers — The next duration numbers each represent, in order, the number
    // of road workers that is required each day
    val nbWorkers = Array.fill(duration)(scanner.nextInt())

    worksheets :+= Worksheet(workcenterId, mandatory, importance, est, lst, duration, roads, nbWorkers)
  }

  // maximal number of roads simultaneously blocked
  var maxBlock = Array[(Int,Set[Int])]()
  while (scanner.hasNext("M")) {
    scanner.next() // discard the M
    val max = scanner.nextInt()
    val roads = scanner.nextLine().split(" ").filter(_ != "").map(_.toInt).toSet
    maxBlock :+= (max, roads)
  }

  // precedences
  var prec = Array[(Int,Int)]()
  while (scanner.hasNext("P")) {
    scanner.next() // discard the next
    prec :+= (scanner.nextInt, scanner.nextInt)
  }

  // **********************
  // * Decision variables *
  // **********************

  val startTimeWorksheet = Array.fill(W)(CPIntVar(0 until T)) // Beginning of worksheet (Array)

  // ***************
  // * Constraints *
  // ***************

  // every worksheet starts after its earliest starting time
  // TODO deal with non mandatory worksheets
  for (i <- 0 until W if worksheets(i).mandatory) add(startTimeWorksheet(i) >= worksheets(i).est)

  // every worksheet starts before its earliest starting time
  // TODO deal with non mandatory worksheets
  for (i <- 0 until W if worksheets(i).mandatory) add(startTimeWorksheet(i) <= worksheets(i).lst)

  // Precedence constraint : if worksheet i preceeds worksheet j, ensure the worksheet i will finish before j
  for ((i,j) <- prec){
    add(startTimeWorksheet(i) + worksheets(i).duration <= startTimeWorksheet(j))
  }
}
