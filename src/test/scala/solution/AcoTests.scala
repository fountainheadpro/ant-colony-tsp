package solution

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import scala.collection.immutable.SortedSet

@RunWith(classOf[JUnitRunner])
class AcoTests extends FunSuite {
  
  
  trait AcoTest {
    val config=ACO.ACOConfig(
	    //heuristic function,	 which define ant's perception of edge "shortness" usually 1/(edge cost)
	    preception={1/_.cost},	      
	    pheromoneBoost=0.1,//alpha
	    // exploration vs. colony information
	    communityFactor=0.9, //Q0 paramter
	    //beta - the number, which is used to increase or decrease the value of perception in choosing the path vs. pheromone
	    preceptionBoost=2, 
	    numberOfIterations=1250,
	    numberOfAnts=10,
	    //this parameter balancing between aimless exploration and sticking to the established path
	    pheromoneDepositAmount=5 
    )
    
    val rnd=new Random();
    val testSize=50
    val gRnd=Vector.tabulate[Double](testSize,testSize){(i,j) => if(i<j) rnd.nextDouble*100 else 0.0}
    val graph=ACO.Graph(testSize, gRnd)
    
  } 
  
  test("best path should be shorter then simple greedy algorithm") {
    
    new AcoTest {
      val bestPath=ACO(graph,config)      
      println("Best path discovered by ants:"+bestPath)
      println("Greedy path:"+graph.nnh(0))
      assert(bestPath.cost<graph.nnh(0).cost)
    }
    
  }
  
  
}