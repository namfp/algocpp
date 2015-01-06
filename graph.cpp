
#include <iostream>
#include <stdlib.h>
#include <vector>
#include <string>
#include <sstream>
#include <queue>

using namespace std;

struct Edge 
  { int v, w;
    Edge(int v = -1, int w = -1) : v(v), w(w) { };
    void show(){
        cout << v << " " << w << endl;
    }
  };


std::vector<std::string> &split(const std::string &s, char delim, std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}


std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, elems);
    return elems;
}


// vector<Edge> read_input(string input){
//     vector<Edge> results;
//     vector<string> edges = split(input, ' ');
//     for(vector<string>::iterator it = edges.begin(); it != edges.end(); ++it){
//         cout << *it << endl;
//     }
//     for (vector<string>::iterator it = edges.begin(); it != edges.end(); ++it){
//         vector<string> edge_string = split(*it, "-");
//         Edge edge(atoi(edge_string[0].c_str()), atoi(edge_string[1].c_str()));
//         results.push_back(edge);
//     }
//     return results;
// }

Edge read_edge(string input){
    vector<string> edge_string = split(input, '-');
    return Edge (atoi(edge_string[0].c_str()), atoi(edge_string[1].c_str()));
}

void read_paths(string& input, int& s, int& t){
    vector<string> edge_string = split(input, '-');
    s = atoi(edge_string[0].c_str());
    t = atoi(edge_string[1].c_str());
}

class GRAPH
  { private:
      // Implementation-dependent code
    public:
      GRAPH(int, bool);
      ~GRAPH();
      int V() const;
      int E() const;
      bool directed() const;
      int insert(Edge);
      int remove(Edge);
      bool edge(int, int);
      class adjIterator
        { 
          public:
            adjIterator(const GRAPH &, int);
            int beg();
            int nxt();
            bool end();
        };
  };

template <class Graph> 
  vector<Edge> edges(Graph &G)
  { int E = 0;
    vector<Edge> a(G.E()); 
    for (int v = 0; v < G.V(); v++) 
      {
        typename Graph::adjIterator A(G, v);
        for (int w = A.beg(); !A.end(); w = A.nxt()) 
          if (G.directed() || v < w)
            a[E++] = Edge(v, w);
      }
    return a;
  };

template <class Graph> 
class IO
  {
    public:
      static void show(const Graph &);
      static void scanEZ(Graph &);
      static void scan(Graph &);
  };

typedef vector<Edge> vector_edges;
typedef vector_edges::iterator edges_it;


template<class Graph>
  void IO<Graph>::scan(Graph& g){
    string input;
    while (input != "exit"){
        cin >> input;
        Edge edge = read_edge(input);
        g.insert(edge);
    }
  };


template <class Graph> 
void IO<Graph>::show(const Graph &G)
  { 
    for (int s = 0; s < G.V(); s++) 
      {
        cout << s << ":";
        typename Graph::adjIterator A(G, s);
        for (int t = A.beg(); !A.end(); t = A.nxt()) 
          { cout << t << " "; }
        cout << endl;
      }
  };



// template <class Graph> 
// class CC
//   { 
//     private:
//       // implementation-dependent code
//     public:
//       CC(const Graph &);
//       int count();
//       bool connect(int, int);
//   };


class DenseGRAPH
{ int Vcnt, Ecnt; bool digraph;
  vector<vector<bool> > adj;
public:
  DenseGRAPH(int V, bool digraph = false) :
    adj(V), Vcnt(V), Ecnt(0), digraph(digraph)
    { 
      for (int i = 0; i < V; i++) 
        adj[i].assign(V, false);
    }
  int V() const { return Vcnt; }
  int E() const { return Ecnt; }
  bool directed() const { return digraph; }
  void insert(Edge e)
    { int v = e.v, w = e.w;
      if (adj[v][w] == false) Ecnt++;
      adj[v][w] = true;
      if (!digraph) adj[w][v] = true; 
    } 
  void remove(Edge e)
    { int v = e.v, w = e.w;
      if (adj[v][w] == true) Ecnt--;
      adj[v][w] = false;
      if (!digraph) adj[w][v] = false; 
    } 
  bool edge(int v, int w) const 
    { return adj[v][w]; }
  class adjIterator;
  friend class adjIterator;
};


class DenseGRAPH::adjIterator
{ const DenseGRAPH &G;
  int i, v;
public:
  adjIterator(const DenseGRAPH &G, int v) : 
    G(G), v(v), i(-1) { }
  int beg()
    { i = -1; return nxt(); }
  int nxt()
    {
      for (i++; i < G.V(); i++)
        if (G.adj[v][i] == true) return i;
      return -1;
    }
  bool end()
    { return i >= G.V(); }
};


template <class Graph> class sPATH
{ const Graph &G;
  vector <bool> visited;
  bool found; 
  bool searchR(int v, int w)
    { 
      if (v == w) return true;
      visited[v] = true;
      typename Graph::adjIterator A(G, v);
      for (int t = A.beg(); !A.end(); t = A.nxt()) 
        if (!visited[t])
          if (searchR(t, w)) return true;
      return false;
    }
public:
  sPATH(const Graph &G, int v, int w) : 
    G(G), visited(G.V(), false) 
    { found = searchR(v, w); }
  bool exists() const 
    { return found; }
};


template <class Graph> class SEARCH
{
  protected:
    const Graph &G;
    int cnt;
    vector <int> ord;
    virtual void searchC(Edge) = 0;
    void search()
      { for (int v = 0; v < G.V(); v++)
          if (ord[v] == -1) searchC(Edge(v, v)); }
  public:
    SEARCH (const Graph &G) : G(G), 
      ord(G.V(), -1), cnt(0) { }
    int operator[](int v) const { return ord[v]; }
};

template <class Graph> 
class BFS : public SEARCH<Graph> 
{ vector<int> st;
  void searchC(Edge e)
  { queue<Edge> Q;  
    Q.push(e);
    while (!Q.empty())
      if (this->ord[(e = Q.pop()).w] == -1) 
      { int v = e.v, w = e.w;
        this->ord[w] = this->cnt++; st[w] = v;        
        typename Graph::adjIterator A(this->G, w);
        for (int t = A.beg(); !A.end(); t = A.nxt()) 
          if (this->ord[t] == -1) Q.push(Edge(w, t));
      }
  }
public:
  BFS(Graph &G) : SEARCH<Graph>(G), st(G.V(), -1) 
    { this->search(); }
  int ST(int v) const { return st[v]; }
};


main(int argc, char *argv[])
{ 
    int V = atoi(argv[1]);
    DenseGRAPH G(V);
    IO<DenseGRAPH>::scan(G);
    if (V < 20) IO<DenseGRAPH>::show(G);
    cout << G.E() << " edges " << endl;
    cout << "find path" << endl;
    int s, t;
    string input;
    while (input != "exit"){
    cin >> input;
    read_paths(input, s, t);
    sPATH<DenseGRAPH> s_path(G, s, t);
    if (s_path.exists()){
    cout << "existed" << endl;
    } else{
    cout << "not existed" << endl;
    }  
    }


  // CC<DenseGRAPH> Gcc(G);
  // cout << Gcc.count() << " components" << endl;
}