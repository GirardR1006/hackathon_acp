package acp19

import oscar.algo.search.Branching
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
  for ((i,j) <- prec) assert(! prec.contains((j,i)) )

  // **********************
  // * Decision variables *
  // **********************

  val startTimeWorksheet = Array.fill(W)(CPIntVar(0 until T)) // Beginning of worksheet (Array)
  // bool var telling if we will use this worksheet or not
  val useWorksheet = Array.tabulate(W)(i =>
    if(worksheets(i).mandatory) CPBoolVar(true)
    else CPBoolVar()
  )

  // ***************
  // * Constraints *
  // ***************

  // every worksheet starts after its earliest starting time
  for (i <- 0 until W) {
    if (worksheets(i).mandatory) add(startTimeWorksheet(i) >= worksheets(i).est)
    else add( !useWorksheet(i) | (startTimeWorksheet(i) ?>= worksheets(i).est ) )
  }


  // every worksheet starts before its earliest starting time
  for (i <- 0 until W) {
    if (worksheets(i).mandatory) add(startTimeWorksheet(i) <= worksheets(i).lst)
    else  add( !useWorksheet(i) | (startTimeWorksheet(i) ?<= worksheets(i).lst ) )
  }

  // Precedence constraint : if worksheet i preceeds worksheet j, ensure the worksheet i will finish before j
  for ((i,j) <- prec){
    // we don't do i OR we don't do j or we do it in the right order
    add( !useWorksheet(i) | !useWorksheet(j) | (startTimeWorksheet(i) + worksheets(i).duration ?<= startTimeWorksheet(j)))
  }
  // work center capacity
  // TODO we can use the following oscar global constraint: maxCumulativeRessource:
  /*  Discrete Resource constraint with maximum capacity: at any time, the cumulative demands of the tasks executing on the resource id, must be <= than the capacity
    * maxCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint
    * @param starts    the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends      the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param demands   the variables representing how much each task consume of the resource
    * @param resources the variables representing the resource where the task is scheduled
    * @param capacity  the capacity of the resource
    * @param id        , the resource on which we want to constraint the capacity (only tasks i with resources(i) = id are taken into account)
    * @return a constraint enforcing that the load over the resource is always below/at its capacity at any point of time
    */
  // in OscaR's example of optional tasks ( https://bitbucket.org/oscarlib/oscar/src/default/oscar-cp/src/main/examples/oscar/examples/cp/scheduling/OptionalTasks.scala )
  // they use a fake resource, which tasks use if they are not selected
  // then, they put maxCumulativeResource on the real resources and not the fake one.
  // we could do the same, have the resource be -1 if the optional task is not used, and 1 otherwise

  // get a list of all the tasks. Every task is the work to do on worksheet i after t days
  val allTasks: Array[(Int, Int)] = (for(i <- 0 until W; t <- 0 until worksheets(i).duration) yield (i, t)).toArray

  // start time is the start time of the worksheet + t
  val starts = for ((i,t) <- allTasks) yield startTimeWorksheet(i) + t
  // duration of every task is 1 day
  val one = CPIntVar(1)
  val durations = Array.fill(allTasks.length)(one)
  // end is start + duration
  val ends = (starts zip durations).map{case (start, duration) => start+duration}
  // demand is the number of workers needed that day
  val demands = for ((i,t) <- allTasks) yield CPIntVar(worksheets(i).nbWorkers(t))

  val resourceForWorksheetOrMinusOne = for(i <- 0 until W) yield {
    if (worksheets(i).mandatory) CPIntVar(worksheets(i).workcenterID)
    else (useWorksheet(i) * (1 + worksheets(i).workcenterID)) - 1 // this is -1 if useWorksheet(i)=0 and workcenterID otherwise
  }
  val resources = for((i,t) <- allTasks) yield resourceForWorksheetOrMinusOne(i)

  for (workcenterID <- 0 until C) {
    add(maxCumulativeResource(starts, durations, ends, demands, resources, CPIntVar(c_v(workcenterID)), workcenterID))
  }

  // not two works on the same road at the same time
  // roads will contain the road number if the worksheet is used, -1 otherwise
  val roads = for ((i,t) <- allTasks) yield {
    if (worksheets(i).mandatory) CPIntVar(worksheets(i).roads(t))
    else  (useWorksheet(i) * (worksheets(i).roads(t) + 1)) - 1 // the road number if the worksheet is used, -1 otherwise
  }
  for (road <- 0 until N) add(unaryResource(starts, durations, ends, roads, road))


  // for road crossing constraints:
  // on définit une ressource par set de routes limités, avec la capacité = le nombre max de routes bloquées
  // dans le groupe.
  for(i <- maxBlock.indices) {
    val (max, set) = maxBlock(i)
    // todo si on travaille plusieurs jours sur la meme route, on peut les joindre en (start duration end)
    val isInSet = roads.map(_.isIn(set): CPIntVar) // 1 if the road is in the set, 0 otherwise. Handles worksheets not being used, because then the road will be -1, thus not in set.

    add( maxCumulativeResource(starts, durations, ends, isInSet, CPIntVar(max)) )
  }


  import scala.util.Random
  val rng = new Random(100)
  def uniform(from: Int, to: Int): Int = from + rng.nextInt(to - from)


  class Solution(val useW: Array[Boolean], val startW: Array[Int]) {
    lazy val realisable: Boolean = {
      try {
        val stats = startSubjectTo(nSols = 1) {
          for (i <- 0 until W) {
            useWorksheet(i).assign(if (useW(i)) 1 else 0)
            startTimeWorksheet(i).assign(startW(i))
          }
        }
        stats.nSols >= 1
      }
      catch {
        case _ => false
      }
    }

    def penalty: Int = {
      var p = 0
      for (i <- 0 until W if useW(i)) {
        for (j <- 0 until worksheets(i).duration) {
          val t = startW(i) + j
          val road = worksheets(i).roads(j)
          p += perturbationCost(road, t)
        }
      }
      p
    }

    lazy val score = (0 until W).filter(useW).map(i => worksheets(i).importance).sum - penalty
  }


  object Solution {
    def random(): Solution = {
      val useWorksheet = (for (i <- 0 until W) yield if (worksheets(i).mandatory) true else rng.nextBoolean()).toArray
      val startTime = worksheets.map(w => uniform(w.est, w.lst))
      new Solution(useWorksheet, startTime)
    }

    def cross1(s1: Solution, s2: Solution): (Solution, Solution) = {
      val i = uniform(1, W-1)

      val new1 = new Solution(s1.useW.take(i) ++ s2.useW.drop(i),
        s1.startW.take(i) ++ s2.startW.drop(i))

      val new2 = new Solution(s2.useW.take(i) ++ s1.useW.drop(i),
        s2.startW.take(i) ++ s1.startW.drop(i))

      (new1, new2)
    }
  }

  for (i <- 0 until 10) println(Solution.random().realisable)


}
