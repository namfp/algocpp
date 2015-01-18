// constructing priority queues
#include <iostream>       // std::cout
#include <queue>          // std::priority_queue
#include <vector>         // std::vector
#include <functional>     // std::greater

using namespace std;
#include <iostream>

int main() 
{ 
int m=0,v=0;

for(m=5;m>0;){
    v=v+(m++)+(++m);
    m=m-3;
}
cout << v << endl;
}