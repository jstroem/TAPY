package tapy.mfw

import tapy.cfg._



// case class WorklistAlgorithm(
//   constraints: Map[Int, Int => Int],
//   dependencies: Map[Int, List[Int]],
//   startValues: Map[Int, Int]) {
  
//   def run(): Int = {

//     var worklist = all nodes

//     while (!worklist.isEmpty) {
//       val node = worklist.pop
//       val constraint = constraints(node)
//       val currValue = solution node
//       val newValue = contraint solution

//       if (newValue != currValue) {
//         solution.updated(node, newValue)
//         worklist.add(dependencies[node]);
//       }
//     }
//     return solution
//   }
// }
