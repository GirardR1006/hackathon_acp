package acp19

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import oscar.algo.search.{Branching, IntConstrainableContext}
import oscar.cp._
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.watcher.Watcher

import scala.collection.mutable
import scala.util.Random

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
    roads: Array[Int], nbWorkers: Array[Int]){override def toString(): String ={
    "Assigned workcenter: "+ Worksheet.this.workcenterID +
    "\nMandatory: " + Worksheet.this.mandatory +
    "\nImportance: " + Worksheet.this.importance
  }}

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

  // precedences
  var prec = Array[(Int,Int)]()
  // maximal number of roads simultaneously blocked
  var maxBlock = Array[(Int,Set[Int])]()

  while (scanner.hasNext("M") || scanner.hasNext("P")) {
    if (scanner.hasNext("M")) {
      scanner.next()
      val max = scanner.nextInt()
      val roads = scanner.nextLine().split(" ").filter(_ != "").map(_.toInt).toSet
      maxBlock :+= (max, roads)
    } else if (scanner.hasNext("P")) {
      scanner.next() // discard the next
      prec :+= (scanner.nextInt, scanner.nextInt)
    }
  }

  for ((i,j) <- prec) assert(! prec.contains((j,i)) )

  // **********************
  // * Decision variables *
  // **********************

  val startTimeWorksheet = Array.tabulate(W)(i => CPIntVar(worksheets(i).est, worksheets(i).lst))
  // bool var telling if we will use this worksheet or not
  val useWorksheet = Array.tabulate(W)(i =>
    if(worksheets(i).mandatory) CPBoolVar(true)
    else CPBoolVar()
  )

  // ***************
  // * Constraints *
  // ***************

  // Precedence constraint : if worksheet i preceeds worksheet j, ensure the worksheet i will finish before j
  for ((i,j) <- prec){
    // we don't do i OR we don't do j or we do it in the right order
    if (worksheets(i).mandatory && worksheets(j).mandatory)
      add(startTimeWorksheet(i) + worksheets(i).duration <= startTimeWorksheet(j))
    else if (worksheets(i).mandatory)
      add( !useWorksheet(j) | (startTimeWorksheet(i) + worksheets(i).duration ?<= startTimeWorksheet(j)) )
    else if (worksheets(j).mandatory)
      add( !useWorksheet(i) | (startTimeWorksheet(i) + worksheets(i).duration ?<= startTimeWorksheet(j)) )
    else
      add( !useWorksheet(i) | !useWorksheet(j) | (startTimeWorksheet(i) + worksheets(i).duration ?<= startTimeWorksheet(j)))
  }
  // work center capacity
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
  val ends = starts.map(_ + 1)
  // demand is the number of workers needed that day
  val demands = for ((i,t) <- allTasks) yield CPIntVar(worksheets(i).nbWorkers(t))

  // gives a var which has value if option is true and -1 otherwise
  def optionalIntVar(value: CPIntVar, option: CPBoolVar): CPIntVar = {
    if (option.isTrue) value
    else if (option.isFalse) CPIntVar(-1)
    else {
      val newvar = CPIntVar(-1 +: value.toSeq)
      add(option ==> (newvar ?=== value))
      add(!option ==> (newvar ?=== -1))
      newvar
    }
  }
  // gives a var which has value if option is true and -1 otherwise
  def optionalIntVar(value: Int, option: CPBoolVar): CPIntVar = {
    if (option.isTrue) CPIntVar(value)
    else if (option.isFalse) CPIntVar(-1)
    else {
      val newvar = CPIntVar(Seq(-1, value))
      add(option ==> (newvar ?=== value))
      add(!option ==> (newvar ?=== -1))
      newvar
    }
  }
  val resourceForWorksheetOrMinusOne = for(i <- 0 until W)
    yield optionalIntVar(worksheets(i).workcenterID, useWorksheet(i))

  val resources = for((i,t) <- allTasks) yield resourceForWorksheetOrMinusOne(i)

  for (workcenterID <- 0 until C) {
    add(maxCumulativeResource(starts, durations, ends, demands, resources, CPIntVar(c_v(workcenterID)), workcenterID),
      Weak)
  }

  // not two works on the same road at the same time
  // roads will contain the road number if the worksheet is used, -1 otherwise
  val roads = for ((i,t) <- allTasks) yield optionalIntVar(worksheets(i).roads(t), useWorksheet(i)) // the road number if the worksheet is used, -1 otherwise

  def canTherBeARoadConflict(road: Int): Boolean = {
    val possilbeWorkTimesOnThatRoadPerWorksheet: Array[Set[Int]] = worksheets.map(w =>
      w.roads.indices.filter(w.roads(_) == road) // get the steps of the worksheet where we work on that road
        .map(t => (w.est+t to w.lst+t).toSet) // transform it into the range of times possible for that work
        match {
        case x if x.isEmpty => Set[Int]()
        case x => x.reduce(_ union _) // do the union for all in this worksheet
      }
    )
    // if the intersection of all these sets is not empty, then there is one possible conflict at least
    possilbeWorkTimesOnThatRoadPerWorksheet.reduce(_ intersect _).nonEmpty
  }

  for (road <- 0 until N
       if worksheets.flatMap(_.roads).contains(road) &&
         worksheets.count(w => w.roads.contains(road)) >= 2 &&
         canTherBeARoadConflict(road) )
    add(unaryResource(starts, durations, ends, roads, road), Weak)


  // for road crossing constraints:
  for(i <- maxBlock.indices) {
    val (max, set) = maxBlock(i)
    // todo si on travaille plusieurs jours sur la meme route, on peut les joindre en (start duration end)
    val isInSet = roads.map(_.isIn(set): CPIntVar) // 1 if the road is in the set, 0 otherwise

    add(maxCumulativeResource(starts, durations, ends, isInSet, CPIntVar(max)), Weak)
  }
  // **********************
  // * Objective function *
  // **********************

  // Maximize total gain and minimze total traffic perturbation
  val importanceArray = for (i <- worksheets.indices if useWorksheet(i).size > 1)
    yield useWorksheet(i) * worksheets(i).importance

  val importanceInevitable = worksheets.indices.filter(i => useWorksheet(i).isTrue).map(i => worksheets(i).importance).sum

  // perturbation on each day
  // roadsToWorksheets(road) = list of worksheets that have this road
  val roadToActivities = Array.tabulate(N)(road =>
    allTasks.filter{case (i,t) => worksheets(i).roads(t) == road})

  val perturbationCostOfTime: Array[Array[Int]]= Array.tabulate(T,N)((t,n) => perturbationCost(n)(t))
  val perturbationDay = Array.tabulate(T)(t => {
    val isRoadWorkedOn: Array[CPIntVar] = Array.tabulate(N)(road => {
      // il ne peut y avoir que 0 ou 1 route active ici
      val binaryVector = roadToActivities(road)
        .filter { case (i, t2) => useWorksheet(i).containsTrue &&
          startTimeWorksheet(i).getMin + t2 <= t && t <= startTimeWorksheet(i).getMax + t2
        }
        .map { case (i, t2) => useWorksheet(i) & (startTimeWorksheet(i) + t2 ?=== t) }
      if (binaryVector.nonEmpty) isOr(binaryVector)
      else CPBoolVar(false)
    })
    weightedSum(perturbationCostOfTime(t), isRoadWorkedOn)
  })

  val objective = if (importanceArray.isEmpty) -maximum(perturbationDay) + importanceInevitable
  else sum(importanceArray) - maximum(perturbationDay) + importanceInevitable

  maximize(objective)

  // ****************************
  // * Search and print results *
  // ****************************
  val decisionVars = useWorksheet ++ startTimeWorksheet

  //search(conflictOrderingSearch(decisionVars, minDom(decisionVars), i => if (i < W) decisionVars(i).getMax else decisionVars(i).getMin))

  search(
    conflictOrderingSearch(useWorksheet.map(i => i: CPIntVar), identity, maxVal(useWorksheet.map(i => i: CPIntVar)))
    ++ binarySplitIdx(startTimeWorksheet, i => worksheets(i).importance + rng.nextInt(10))
  )

  onSolution{
    for (i <- 0 until W if useWorksheet(i).isTrue) println(s"$i ${startTimeWorksheet(i)}")
    println("SCORE")
    println(objective.value)

    if (bestSol == null || objective.value >= bestSol.score)
      bestSol = Solution.getFromSolverAndSave()
  }

  import scala.util.Random
  val rng = new Random(100)
  def uniform(from: Int, to: Int): Int = from + rng.nextInt(to - from)

  @SerialVersionUID(123L)
  class Solution(val useW: Array[Boolean], val startW: Array[Int]) extends Serializable {
    override val toString: String = {
      var s = ""
      for (i <- 0 until W if useW(i)) s += s"$i ${startW(i)}\n"
      s
    }
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
        case x => false
      }
    }

    def penalty: Int = {
      val perturbationPerDay = Array.tabulate(T)(t =>
        allTasks
          .filter{case (i, t2) => startW(i)+t2 == t}
          .map{case (i, t2) => perturbationCost(worksheets(i).roads(t2))(t)}
          .sum
      )
      perturbationPerDay.max
    }

    lazy val score = (0 until W).filter(useW).map(i => worksheets(i).importance).sum - penalty
  }

  object Solution {
    def getFromSolverAndSave(): Solution = {
      val sol = new Solution(useWorksheet.map(_.isTrue), startTimeWorksheet.map(_.value))
      val oos = new ObjectOutputStream(new FileOutputStream(s"$filename.sol"))
      oos.writeObject(sol)
      oos.close()
      println("saved solution to file")
      sol
    }

    def getFromFile(): Option[Solution] = {
      try {
        val ois = new ObjectInputStream(new FileInputStream(s"$filename.sol"))
        val sol = ois.readObject().asInstanceOf[Solution]
        ois.close()
        println(s"successfully loaded solution from file $filename.sol")
        Some(sol)
      } catch {
        case _ => {
          println("unable to load solution from file")
          None
        }
      }
    }

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

  var bestSol: Solution = _

  Solution.getFromFile() match {
    case Some(s) => bestSol = s
    case None => {
      println("finding first solution")
      println(start(1))
    }
  }

  var alpha = 10
  var limit = 50

  var startTime = System.currentTimeMillis()

  while (System.currentTimeMillis() - startTime < 2*60*60*1000) {
    val stat = startSubjectTo(failureLimit = limit) {
      for (i <- 0 until W if rng.nextInt(100) > alpha) {
        add( startTimeWorksheet(i) === bestSol.startW(i) )
        add( useWorksheet(i) === (if(bestSol.useW(i)) 1 else 0 ) )
      }
    }

    alpha = if (stat.completed) alpha+1 else alpha-1
    if (alpha <= 0) {
      alpha = 10
      limit *= 2
      println(s"limit is now $limit")
    } else if (alpha >= 120) {
      println("current best:")
      println(bestSol)
      println("finding best solution once and for all...")
      val stats = start()
      println(bestSol)
      println(stats)
      System.exit(0)
    }

    println(s"alpha = $alpha")
  }
}
