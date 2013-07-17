This is scala implementaiton of travling salesman algorithm based on the paper by Marco Dorigo:
http://www.idsia.ch/~luca/acs-bio97.pdf

It uses separate threads for different ant's traversals to take advantage of multiple cores.

Run it with 
	sbt test 
to see the progression of finding the shortest path.

It supports bidirectional complete graph.
Modify the tests file https://github.com/fountainheadpro/ant-colony-tsp/blob/master/src/test/scala/solution/AcoTests.scala
to experiment with the parameters.

Usage:

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
    
    //populate graph data:
    val rnd=new Random();
    val testSize=50
    val gRnd=Vector.tabulate[Double](testSize,testSize){(i,j) => if(i<j) rnd.nextDouble*100 else 0.0}
    val graph=ACO.Graph(testSize, gRnd)
    
    //Tadam:
    val bestPath=ACO(graph,config)
    
    
