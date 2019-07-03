package acp19

import oscar.cp._

/**
 * @author ${user.name}
 */
object App extends CPModel with App {

  // Inputs Parameters
  val W = 2 // Number of worksheets (Int)
  val T = 45 // Number of days (Int)
  val N = 2 // Number of roads (Int)
  val C = 1 // Number of work centers (Int)
  val A = 4 // Number of activities (Int)

  //// Workcenter related parameters
  val c_v = Array(150) // Capacity for each workcenter (Array of Int indexed on workcenters)

  //// Worksheets related parameters
  val w_p = Array(1,2) // Durations of each worksheets (Array of Int indexed on worksheets)
  val w_c = Array(0,1) // Workcenter used by worksheet (Array of Int indexed on worksheets)
  val w_v = Array(Array(18),Array(29,32)) // Number of workers required for each day by worksheet (Matrix of Int indexed on worksheets)
  val w_m = Array(0,1) // Mandatory worksheets (Array of Bool indexed on worksheets)
  val w_r = Array() // Earliest beginning date of worksheets (Array of Int indexed on worksheets)
  val w_d = Array() // Latest beginning date of worksheets (Array of Int indexed on worksheets)
  val w_n = Array(Array(0),Array(0,1)) // Roads used by worksheets (Matrix of Int indexed on worksheets)
  val w_u = Array(55,89) // Importance of worksheet (Array of Int indexed on worksheets)
  val prec = Array((0,1)) // Precedence between worksheets (Array of Tuple of Int)

  //// Roads related parameters
  val n_u = Array() // Cost for shutting down roads (Matrix of Int indexed on LINE = roads, COLUMN = day)

  // Decision variables
  val w = Array.fill(W)(CPIntVar(0 until T)) // Beginning of worksheet (Array)

  // Constraints
  //// Precedence constraint : if worksheet i preceed worksheet j, ensure the worksheet i will finish before j
  for ((a,b) <- prec){
    add( ((w(a) + w_p(a)) <== w(b)))

  }

}
