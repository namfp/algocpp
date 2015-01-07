#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <queue> 

using namespace std;

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/

int main()
{
    vector<int> mountains(8);

    // game loop
    while (1) {
        int SX;
        int SY;
        cin >> SX >> SY; cin.ignore();
        cerr << SX << " " << SY << endl;
        for (int i = 0; i < 8; i++) {
            int MH; // represents the height of one mountain, from 9 to 0. Mountain heights are provided from left to right.
            cin >> MH; cin.ignore();
            mountains[i] = MH;
            cerr << MH << endl;
        }

        // Write an action using cout. DON'T FORGET THE "<< endl"
        // To debug: cerr << "Debug messages..." << endl;
        
        int max_position = max_element(mountains.begin(), mountains.end()) - mountains.begin();
        if (SX==max_position)
        {
            cout << "FIRE" << endl; // either:  FIRE (ship is firing its phase cannons) or HOLD (ship is not firing)

        } else {
            cout << "HOLD" << endl;
        }

        
    }
}