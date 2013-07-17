package solution

import scala.actors.Actor._
import scala.actors.Actor
import scala.util.Random
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.SortedSet
import scala.collection.concurrent.TrieMap

object ACO{
  
	case class ACOConfig(
	    //heuristic function, which define ant's perception of edge "shortness" usually 1/(edge cost)
	    preception: (Edge)=>Double,	      
	    pheromoneBoost: Double,//alpha parameter
	    // exploration vs. colony information
	    communityFactor: Double, //Q0 paramter
	    //beta - the number, which is used to increase or decrease the value of perception in choosing the path vs. pheromone
	    preceptionBoost: Double, 
	    numberOfIterations: Int,
	    numberOfAnts: Int,
	    pheromoneDepositAmount: Double 
	)  
  
	type Node=Int //numbered nodes - 0,1,2,3...	
	case class Edge(start: Node, end: Node, cost:Double)	
	implicit val OrderingEdge = Ordering.by[Edge, Double]{_.cost}
	
	type CostMatrix = IndexedSeq[IndexedSeq[Double]] // start, ends, cost
		
	case class Graph(numNodes: Int, costMatrix: CostMatrix){
	  	  	  
	  abstract class Path(visited: IndexedSeq[Node]){
	    
	    def last=visited.last
	    
	    def edgeCost(start: Node, end: Node): Double=costMatrix(start min end)(end max start)
		  
	    def cost: Double=((visited  zip visited.tail) :\ 0.0)({case((start,end), cost)=>edgeCost(start,end)+cost})		
	    
	    def forEach(f: Edge=>Unit){
	      (visited zip visited.tail).foreach({case (start: Node, end: Node)=>f(Edge(start, end, edgeCost(start, end)))})
	    }
	    
	    override def toString="cost: "+ cost+"  "+ visited.mkString(" >> ") 
	  }

	  case class IncompletePath(visited: IndexedSeq[Node]) extends Path(visited){
	    
	    def addNode(node: Node): Path={
	      if (visited.contains(node)) throw new Error("Node: "+node+" is already on the path")
	      
	      if(visited.size<numNodes-1) IncompletePath(visited:+node)
	      //loop needs to be closed for the complete path
	      else CompletePath(visited++Vector(node,visited.head)) 
	    }
	    
	    def hasEdges(): Boolean=visited.size>1
	    
	    def addEdge(edge: Edge): Path=addNode(edge.end)
	    
	    //TODO: can be rewritten to user sets and decrease the cost of contains operation.
	    def candidateNodes(): Iterator[Node]={	      
	      (0 until numNodes).iterator.withFilter(!visited.contains(_))
	    }
	    
	    def candidateEdges(): Iterator[Edge]={
	      val currentNode=last
	      candidateNodes().map{(candidate: Node)=>Edge(currentNode,candidate, edgeCost(currentNode, candidate))}
	    }
	    
	    def lastVisitedEdge: Edge={
	      visited match {
		     case IndexedSeq()=>throw new Error("path is too short")
		     case IndexedSeq(head)=>throw new Error("path is too short")
		     case prev:+last=>Edge(prev.last, last, edgeCost(prev.last, last))	        
	      }
	    }
	    
	    
	  }	  
	  case class CompletePath(visited: IndexedSeq[Node]) extends Path(visited)
	  
	  //path ordering will be used for sorting pathways	  
	  implicit val OrderingPath = Ordering.by[CompletePath, Double]{_.cost}
	
	  /**
	   * implements nearest neighbor heuristic function 
	   *  
	   */
	  def nnh(start: Node): CompletePath={
		  
		  @tailrec def buildPathAcc(from: IncompletePath): CompletePath={
		    
		     val nearestNeighbour=from.candidateEdges.min.end	     
		     from.addNode(nearestNeighbour) match {
		       case i:IncompletePath=>buildPathAcc(i)
		       case c:CompletePath=>c
		     }
		  }	  
		  
		  buildPathAcc(IncompletePath(Vector(start)))    
	  }		
	  	  	  
	}
	
		
	private[ACO] class ACOScript(graph: Graph,config: ACOConfig){
	  
		require({config.numberOfAnts<=graph.numNodes}, "Number of ants can not exceed number of nodes.")
	  
		//thau zero parameter.
	    val initialPreromoneLevel=(1/(graph.nnh(0).cost*graph.numNodes))
	  
	    val pheromoneCollector=PheromoneCollector(initialPreromoneLevel)
	    
	    val pathCollector=new PathCollector()
	    
	    val gen=(1 to config.numberOfAnts).map(new Random(_))
	  
		case class Ant(initialLocation: Node, gen: Random, iteration: Int) {
		  		  		  
		  
		  private[ACOScript] def traverse(): Unit={

		    @tailrec def traverseFrom(from: graph.IncompletePath): Unit={
		      if(from.hasEdges)  pheromoneCollector.localUpdate(from.lastVisitedEdge)
			   val nextNode=next(from)
			   from.addNode(nextNode) match {
			     case ic: graph.IncompletePath => {			       
			       traverseFrom(ic)
			     }
			     case c: graph.CompletePath => pathCollector ! (c,iteration) 
			   }		   
			  }
			  
			 traverseFrom(graph.IncompletePath(Vector(initialLocation))) 
			  
		  }	  
		  
		  private def next(from: graph.IncompletePath): Node={
		    val currentNode=from.last		    
		    val candidates=from.candidateNodes
		    val candidateEdges=from.candidateEdges
		    val candidatePheromoneLevels=for(n<-from.candidateNodes)  yield Edge(currentNode,n,pheromoneCollector.pheromone(currentNode,n))		    
		    val visibilityAdjustedPheromoneLevels=(for (mixedEdge<-(candidatePheromoneLevels zip candidateEdges)) 
		      yield mixedEdge._1.copy(cost=(mixedEdge._1.cost*Math.pow(config.preception(mixedEdge._2), config.preceptionBoost)))).toSet		      
		    //Since edges are compared by cost of the edge, 
		    //  max edge of visibilityAdjustedPheromoneLevels will be the one where there is more pheromone  
		    val communityPath=visibilityAdjustedPheromoneLevels.max		    
		    val divider=visibilityAdjustedPheromoneLevels.foldLeft(0.0)(_+_.cost)		    
		    val probabilityDistribution=visibilityAdjustedPheromoneLevels.map(e=>e.copy(cost=e.cost/divider))
		    val rnd1=gen.nextDouble
		    var probSum=0.0
		    val expoloratoryOption=probabilityDistribution.toStream.takeWhile(_ => probSum < rnd1).map({(e: Edge)=>probSum+=(e.cost); e}).last
		    val rnd2=gen.nextDouble		    
		    if (rnd2<config.communityFactor) communityPath.end else expoloratoryOption.end
		  }		  	
		  
		}
		
		case class PheromoneCollector(initialPreromoneLevel: Double){
		  		  
		  private val _pheromone=collection.concurrent.TrieMap[(Node, Node), Double]()
		  
		  def pheromone(): collection.mutable.Map[(Node, Node), Double]=return _pheromone
		  
		  def pheromone(e: Edge): Double=pheromone(e.start, e.end)
		  
		  def pheromone(start: Node, end: Node): Double=_pheromone.getOrElse(fixKey((start,end)), config.pheromoneBoost*initialPreromoneLevel)
		  
		  //in order to avoid storing pheromone levels twice for back and force, we fix the key to always lookup by smaller to larger node. 
		  private def fixKey(key: (Node, Node)): (Node, Node)=(key._1 min key._2, key._2 max key._1)
		  
		  
		  //pheromone evaporation
		  def localUpdate(e: Edge): Unit={		    
		    val alpha=config.pheromoneBoost
		    val before=pheromone(e)		    
		    _pheromone(fixKey(e.start, e.end))=(1-alpha)*pheromone(e)+alpha*initialPreromoneLevel
		    //println("local update before:" +before+ " after:"+_pheromone(fixKey(e.start, e.end)))
		  }
		  
		  
		  def globalUpdate(p: graph.CompletePath): Unit={
		    val alpha=config.pheromoneBoost
		    val pheromoneAddition=config.pheromoneDepositAmount/p.cost
		    //println("global update: before:"+ pheromone(p.visited(0), p.visited(1))+"after:"+((1-alpha)*pheromone(p.visited(0), p.visited(1))+alpha*pheromoneAddition))
		    p.forEach((e)=>{
		    	_pheromone(fixKey(e.start, e.end))=(1-alpha)*pheromone(e)+alpha*pheromoneAddition
		    })
		  }
		  
		  override def toString(): String={
		    val ord=Ordering.by[((Int, Int), Double), Double](_._2)
		    val b=_pheromone.toArray
		    util.Sorting.quickSort(b)(ord)		    
		    b.reverse.mkString
		  }
		  
		} 
	 
	  def iteration(iterationNumber: Int): Unit={
		  //Random generator for each ant		  
		  var initalLocations: collection.mutable.Set[Node]=collection.mutable.Set()		  
		  		  
		  def initializeAnt(antIndex: Int): Ant={	    
			  def getRandomNode(index: Int): Node=gen(index).nextInt(graph.numNodes)
		      val initalLocation: Node=(Stream continually gen(antIndex).nextInt(graph.numNodes)).filterNot(initalLocations.contains(_)).take(1).head
		      initalLocations+=initalLocation		      
		      Ant(initalLocation, gen(antIndex), iterationNumber)	      
		  }
	    
	      //ants are located in the random cities
		  val ants=Vector.tabulate[Ant](config.numberOfAnts)(initializeAnt)
		  ants.par.foreach(_.traverse)
	  }
	  	  
	  
	  
	  class PathCollector extends Actor{
	    
	    def act(){	      	      
	      var results=collection.mutable.SortedSet[graph.CompletePath]()(graph.OrderingPath)
	      var bestPath=graph.nnh(0);
	      loop{
	        react{
	          case (cp:graph.CompletePath, iteration: Int)=>{
	            results+=cp
	            //All ants are done with the trips
	            if (results.size==config.numberOfAnts){
	              val localBestPath=results.head
	              pheromoneCollector.globalUpdate(localBestPath)
	              if (localBestPath.cost<bestPath.cost){	            	  
	            	  bestPath=localBestPath
	            	  println("better path found on iteration:"+iteration+" : "+bestPath)
	              }	              
	              results=collection.mutable.SortedSet[graph.CompletePath]()(graph.OrderingPath)
	            }
	          }
	          case "BESTPATH"=>reply(bestPath); exit() 
	        }	        
	      }	      
	    }
	    
	  }
	  
	}
	
	def apply(graph: Graph,config: ACOConfig): graph.CompletePath={
	    val script=new ACOScript(graph: Graph,config: ACOConfig)	    
	    script.pathCollector.start
	    (1 to config.numberOfIterations).foreach(script.iteration)
	    (script.pathCollector !? "BESTPATH").asInstanceOf[graph.CompletePath]
	 }
}