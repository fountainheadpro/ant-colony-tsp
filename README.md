This is scala implementaiton of travling salesman algorithm based on the paper by Marco Dorigo:
http://www.idsia.ch/~luca/acs-bio97.pdf

It uses separate threads for different ant's traversals to take advantage of multiple cores.

Run it with sbt test to see the progression of finding the shortest path.

It supports bidirectional complete graph.
Modify the tests file https://github.com/fountainheadpro/ant-colony-tsp/blob/master/src/test/scala/solution/AcoTests.scala
to experiment with the parameters.

