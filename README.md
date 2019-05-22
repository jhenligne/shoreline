# Shoreline
This repository was motivated by a problem submitted on a job offer. Sections [Problem](#problem) and [Instructions](#instructions) describe company's expectation. Other ones explain the choices I made to implement it and my answers to the questions.

## <a name="problem"></a>Problem

We are building a social network.  In this social network, each user has friends.

A chain of friends between two users, user A and user B, is a sequence of users starting with A and ending with B, such that for each user in the chain, u<sub>a</sub>, the subsequent user, u<sub>a + 1</sub>, are friends.

Given a social network and two users, user A and user B, please write a function that computes the length of the shortest chain of friends between A and B.

## <a name="instructions"></a>Instructions
Please write answers to the following discussion questions and include them in your solution as comments:

* How did you represent the social network?  Why did you choose this representation?
* What algorithm did you use to compute the shortest chain of friends?  What alternatives did you consider?  Why did you choose this algorithm over the alternatives?
* Please enumerate the test cases you considered and explain their relevance.

## Implementation
### Choices
I found this problem interesting and thought it would be beneficial to open it to a wider audience by structuring it as a workshop:
* number of nodes or edges can easily be changed in [Utils.hs](src/Utils.hs),
* edges are randomly generated,
* if Graphviz is installed on the machine, a [png](graph.png) of the graph is generated after computation.

By default there are 50 nodes to keep the graph readable.  
Random name generator are easy to find, I used [listofrandomnames.com](http://listofrandomnames.com/) but [randomwordgenerator.com](https://randomwordgenerator.com/name.php) allows bigger lists.

### Usage
This is a usual Stack project: clone it, open a terminal and use `stack build`, `stack run`, `stack test` or `stack repl` according to your needs.

### Answers
* How did you represent the social network?  Why did you choose this representation?  

This is a typical example of a **graph**.  
Because nothing defines edges, it is **unweighted**. Friendship is supposed to be mutual, so edges are **undirected**.  

* What algorithm did you use to compute the shortest chain of friends?  What alternatives did you consider?  Why did you choose this algorithm over the alternatives?  

**Breadth-first search (BFS)** is the appropriate algorithm for searching a tree or data structure. It has a time complexity of *O(V + E)*, meaning every vertex and every edge will be explored in the worst case. This linear complexity gives good performance.  
Dijkstra's Shortest Path is another popular algorithm but it is used with weighted graphs. It could be used with the same arbitrary weight for every edge but its time complexity is *O(E + VlogV)*. Bellman-Ford is another algorithm for weighted graphs running in *O(VE)* time.  

[Functional programming with graphs](https://futtetennismo.me/posts/algorithms-and-data-structures/2017-12-08-functional-graphs.html) is a very interesting post on algorithm implementations in Haskell. The "Breadth-first search" section explains clearly how the algorithm works and gives insight in its Haskell version.  

The [Functional Graph Library](http://hackage.haskell.org/package/fgl) on Hackage gives the implementation of BFS used here.  
The function `esp :: Graph gr => Node -> Node -> gr a b -> Path` (edges shortest path) uses a BFS algorithm under the hood. The graph is a PatriciaTree.

* Please enumerate the test cases you considered and explain their relevance.  

Here they are:  
<p align="center">
  <img src="tests.png" alt="Tests" width="75%" height="75%">
</p>

Besides the obvious test whether the shortest path is found or not, it is important to test the algorithm against a **cyclic graph** to see if nodes are explored only once. The last test check the **time complexity** with a huge number of edges.  
It creates a graph of 1415 nodes with edges connecting each node against all nodes, including itself, resulting in 2.003.640 edges (1415<sup>2</sup> is 2.002.225 but each edge from a node to itself is also bidirectional from itself to node...).  

Those indexes tests should also be implemented but they are not related to graphs:  

```haskell
let n     = 50  --nummber of nodes in the graph
    start = 0   --start node
    end   = n-1 --end node
in (0 <= start && start <= n) && (0 <= end && end <= n)
```

## BFS algorithm
### Behavior
A good description of BFS behavior is in the figure below (taken from [www.hackerearth.com](https://www.hackerearth.com/fr/practice/notes/graph-theory-breadth-first-search/)).  
Beware there is a small mistake in graph 4: the level 3 set should not draw an edge between nodes 9 and 5 but between nodes 7 and 5.
<p align="center">
  <img src="bfs.jpg" alt="Breadth First Search" width="50%" height="50%">
</p>

### Functional Graph Library implementation
To build this graph

<p align="center">
  <img src="graph.png" alt="Graph" width="75%" height="75%">
</p>

with the edges on the left, `fgl` will generate the paths on the right
<p align="center">
  <img src="edges-paths.png" alt="Edges and Paths" width="75%" height="75%">
</p>

This graph is clearly undirected. Each pair of nodes linked has in fact 2 edges. For example "Layne Barcus" (line 1 on the left) has an edge with "Bong Reeves" (line 7 on the left) and vice versa.  

The growing length of the paths reflects the behavior described above. It is confirmed if we look how each path is added. For example we start with the node "Kaci Sunde" (line 1 on the right). Lines 2 to 4 add paths to the 3 direct siblings of "Kaci Sunde". Lines 5 to 18 start anew with each of these 3 direct siblings. This traversal continues until there are no nodes.

#### Code
A hand made diagram to have a better understanding of [Functional Graph Library](http://hackage.haskell.org/package/fgl)' BFS implementation:

<img src="bfs-fgl.png" alt="fgl implementation of BFS">