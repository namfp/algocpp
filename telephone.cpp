#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <deque>
#include <set>

using namespace std;

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/


struct Tree{
    struct TreeCompare{
    bool operator() (const Tree& lhs, const Tree& rhs) const{
        return lhs.value < rhs.value;
    }
    };

    typedef set<Tree, TreeCompare> set_tree;
    
    int value;
    set_tree children;
    Tree(int value): value(value){

    };

    set_tree::iterator find_tree(set_tree& childs, int n){
        for(std::set<Tree>::iterator it=childs.begin(); it != childs.end(); ++it){
            if (n == it->value){
                return it;
            }
        }
        return childs.end();
    };


    void insert(deque<int>& number){
        while (!number.empty()){
            int& front = number.front();
            set_tree::iterator it = find_tree(children, front);
            if (it != children.end()){
                number.pop_front();
                (*it).insert(number);

            } else {
                Tree tree = Tree(front);
                


                children.insert(tree);
            }

        }
    }
};


int main()
{
    int N;
    cin >> N; cin.ignore();
    for (int i = 0; i < N; i++) {
        string telephone;
        cin >> telephone; cin.ignore();
    }

    // Write an action using cout. DON'T FORGET THE "<< endl"
    // To debug: cerr << "Debug messages..." << endl;

    cout << "number" << endl; // The number of elements (referencing a number) stored in the structure.
}