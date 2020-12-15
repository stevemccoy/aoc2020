/*
    Advent of Code 2020 - Day 6.
*/

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <map>
#include <algorithm>
#include <bitset>

using namespace std;

// Read all the lines from the specified input file.
bool read_input(const string& inputFile, vector<string>& lines) {
    ifstream ifs(inputFile, ios::in);
    if (!ifs.is_open()) {
        cerr << "Failed to open input file." << endl;
        return false;
    }
    string line;
    while (!ifs.eof()) {
        getline(ifs, line);
        lines.push_back(line);
    }
    ifs.close();
    return true;
}

void update_tally(const string& s, bitset<26>& tally) {
    for (const char* cp = s.c_str(); *cp != '\0'; cp++) {
        int i = tolower(*cp) - 'a';
        tally[i] = true;
    }
}

int group_count_sum(vector<string>& lines) {
    int sum = 0, count = 0;
    bitset<26> tally;
    for (auto line : lines) {
        if (line.empty()) {
            count = tally.count();
            cout << "Group count: " << count << endl;
            sum += count;
            tally.reset();
        }
        else
        {
            update_tally(line, tally);
        }
    }
    return sum;
}

int group_common_sum(const vector<string> lines) {
    int sum = 0, count = 0;
    bitset<26> tally;
    vector<bitset<26>> members;
    for (auto line : lines) {
        if (line.empty()) {
            tally.set();
            for (auto m : members) {
                tally = tally & m;
            }
            members.clear();
            count = tally.count();
            cout << "Group count: " << count << endl;
            sum += count;
            tally.reset();
        }
        else
        {
            tally.reset();
            update_tally(line, tally);
            members.push_back(tally);
        }
    }
    return sum;
}

/*
    Part 1. For each group, count the number of questions to which anyone answered "yes". 
    What is the sum of those counts?
*/

int main() {
    vector<string> lines;
    if (!read_input("input6.txt", lines)) {
        cerr << "Error reading data from file." << endl;
        return 1;
    }
    int count = group_count_sum(lines);
    cout << "Part 1. Sum of question counts for all groups = " << count << endl;

    count = group_common_sum(lines);
    cout << "Part 2. Sum of counts for all group members = " << count << endl;
    return 0;
}
