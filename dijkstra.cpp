// A C / C++ program for Dijkstra's single source shortest path algorithm.
// The program is for adjacency matrix representation of the graph
 
#include <stdio.h>
#include <limits.h>
#include <vector>
#include <iostream>
#include <string>


using namespace std;
// Number of vertices in the graph
 
// A utility function to find the vertex with minimum distance value, from
// the set of vertices not yet included in shortest path tree
int minDistance(vector<int>& dist, vector<bool>& sptSet)
{
   // Initialize min value
   int min = INT_MAX, min_index;
 
   for (int v = 0; v < dist.size(); v++)
     if (sptSet[v] == false && dist[v] <= min)
         min = dist[v], min_index = v;
 
   return min_index;
};
 
//A utility function to print the constructed distance array
int printSolution(vector<int>& dist, vector<int>& pred)
{
  int size = dist.size();
   printf("Vertex \t\t Distance from Source \t\t Pred\n");
   for (int i = 0; i < size; i++)
      printf("%d \t\t %d \t\t %d \n", i, dist[i], pred[i]);
}
 
// Funtion that implements Dijkstra's single source shortest path algorithm
// for a graph represented using adjacency matrix representation

typedef vector< vector<int> > matrix;


void dijkstra(matrix& graph, vector<int>& dist, vector<int>& pred, int src)
{
  int size = graph.size();
  // vector<int> dist(size);     // The output array.  dist[i] will hold the shortest
  //                 // distance from src to i
  // vector<int> pred(size);

  vector<bool> sptSet(size); // sptSet[i] will true if vertex i is included in shortest
                 // path tree or shortest distance from src to i is finalized

  // Initialize all distances as INFINITE and stpSet[] as false
  for (int i = 0; i < size; i++)
  {
    dist[i] = INT_MAX;
    sptSet[i] = false;
    pred[i] = -1;
  };
    

  // Distance of source vertex from itself is always 0
  dist[src] = 0;

  // Find shortest path for all vertices
  for (int count = 0; count < size-1; count++)
  {
   // Pick the minimum distance vertex from the set of vertices not
   // yet processed. u is always equal to src in first iteration.
   int u = minDistance(dist, sptSet);

   // Mark the picked vertex as processed
   sptSet[u] = true;

   // Update dist value of the adjacent vertices of the picked vertex.
   for (int v = 0; v < size; v++)

     // Update dist[v] only if is not in sptSet, there is an edge from 
     // u to v, and total weight of path from src to  v through u is 
     // smaller than current value of dist[v]
     if (!sptSet[v] && graph[u][v] && dist[u] != INT_MAX 
                                   && dist[u]+graph[u][v] < dist[v]){
        dist[v] = dist[u] + graph[u][v];
        pred[v] = u;
     } 
  }
 
     // print the constructed distance array
};


void find_links(matrix& graph, vector<int>& gateways, int src, int& node1, int& node2){
  int min_distance = INT_MAX;
  int target = -1;
  int pred_target = -1;

  for(vector<int>::iterator gw_it = gateways.begin(); gw_it != gateways.end(); ++gw_it){
    std::vector<int> dist(graph.size(), INT_MAX);
    std::vector<int> pred(graph.size(), -1);
    dijkstra(graph, dist, pred, src);
    vector<int> gateways_distance(gateways.size(), INT_MAX);
    

    if (dist[*gw_it] < min_distance){
      min_distance = dist[*gw_it];
      target = *gw_it;
      pred_target = pred[target];
      
    }
   
  } // for

   if (target != -1)
    {
      node1 = pred_target;
      node2 = target;
      // Destroy links
      graph[node1][node2] = 0;
      graph[node2][node1] = 0;

    }

}



 
// driver program to test above function

void add_link(matrix& m, int n1, int n2){
  m[n1][n2] = 1;
  m[n2][n1] = 1;
}


int main()
{
    int N; // the total number of nodes in the level, including the gateways
    int L; // the number of links
    int E; // the number of exit gateways
    cin >> N >> L >> E; cin.ignore();
    matrix m(N, vector<int>(N, 0));
    

    for (int i = 0; i < L; i++) {
        int N1; // N1 and N2 defines a link between these nodes
        int N2;
        cin >> N1 >> N2; 
        cin.ignore();
        add_link(m, N1, N2);
    }
    vector<int> gateways;
    

    for (int i = 0; i < E; i++) {
        int EI; // the index of a gateway node
        cin >> EI; 
        cin.ignore();
        gateways.push_back(EI);
    }


    // game loop
    while (1) {
        int SI; // The index of the node on which the Skynet agent is positioned this turn
        cin >> SI; cin.ignore();
        int node1 = -1;
        int node2 = -1;
        find_links(m, gateways, SI, node1, node2);

        // Write an action using cout. DON'T FORGET THE "<< endl"
        // To debug: cerr << "Debug messages..." << endl;

        cout << node1 << " " << node2 << endl; // Example: 0 1 are the indices of the nodes you wish to sever the link between
    }
}