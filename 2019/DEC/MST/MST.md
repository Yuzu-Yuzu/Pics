# Minimum Spanning Tree (MST)
<!-- TOC -->

- [Problem](#problem)
- [Greedy Solution Overview](#greedy-solution-overview)
- [Prim's Algorithm](#prims-algorithm)
    - [Prim: Linear Search Version](#prim-linear-search-version)
    - [Prim: Heap Version](#prim-heap-version)
- [Kruskal's Algorithm](#kruskals-algorithm)
- [Conclusion](#conclusion)

<!-- /TOC -->
# Problem
Build railways of the least total length to connect all cities.
# Greedy Solution Overview
```
Initialize the MST with an arbitary vertice
while !(MST has spaned the graph)
    Add the shortest path to existing MST
end while
```
# Prim's Algorithm
## Prim: Linear Search Version
```cpp
// keep a table, initialize it as following
// vertice 0 is the first-added vertice
vertice    is_included    tentative_distance    connected_from
      0           true                     0                -1
      1          false             MAX_INT32                -1
      2          false             MAX_INT32                -1
      3            ...                   ...               ...

int i = 0; // last connected vertice
do {
    // Start with the last added vertice i
    // update the table for vertices connected to i
    // Complexity: O(V)
    for ( int item : connected_to(i) ){
        if ( tentative_distance[item] < distance(i, item) ){
            tentative_distance[item] = distance(i, item);
            connected_from[item] = i;
        }
    }
    
    // Add a unconnected vertice with the minimum tentative distance
    // Complexity: O(V)
    int min = MAX_INT32;
    for ( int item : unconnected() ){
        if ( tentative_distance[item] < min ){
            min = tentative_distance[item];
            i = item;
        }
    }
    is_included[i] = true;
    
} while ( !unconnected().empty() );
// loop V times, until all vertices are connected
// Overall Complexity: O(V^2)
```
## Prim: Heap Version
- Replace **tentative_distance** with a heap
- Step 1: Update the table for previously connected vertice
    - Involved updating an element in a heap
    - $O(\log(V))$ each
    - Worst case: update for every edge $O(E)$
    - Complexity: $O(E\log(V))$
- Step 2: select an unconnected vertice with the smallest **tentative_distance**
    - $O(\log(V))$ each
    - $V$ times
    - Complexity: $O(V\log(V))$
- Step 3: Set **is_included** for that vertice **true**
    - $O(1)$ each
    - $V$ times
    - Complexity: $O(V)$
- Overall Complexity: $O(E\log(V))$
- Better than $O(V^2)$ when the graph is sparse

# Kruskal's Algorithm
- Step 1: Sort all edges $O(E\log(E))$
- Step 2: Loop through all edges: (for $V$ times)
    - Test if the shortest edge leads to a loop
        - Union_find
        - less than $O(\log(E))$ 
    - If it forms a loop, discard that edge $O(1)$
    - If it does not form a loop, add that edge $O(1)$
- Overall Complexity: $O(E\log(E))\approx O(E\log(V))$ when graph is sparse

# Conclusion
- When graph is dense, use Prim (linear version)
- When graph is sparse, use Kruskal