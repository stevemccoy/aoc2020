/*
    Advent of Code 2020 - Day 10.
*/

#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <fstream>
#include <algorithm>

using namespace std;

// Read all the lines from the specified input file.
bool read_input(const string& inputFile, vector<int>& lines) {
    ifstream ifs(inputFile, ios::in);
    if (!ifs.is_open()) {
        cerr << "Failed to open input file." << endl;
        return false;
    }
    string line;
    while (!ifs.eof()) {
        getline(ifs, line);
        if (!line.empty()) {
            lines.push_back(atoi(line.c_str()));
        }
    }
    ifs.close();
    return true;
}

int day10part1(vector<int>& lines) {
    int jolts = 0;
    int dj = 0;
    std::map<int, int> steps;
    // Sort the input first.
    std::sort(lines.begin(), lines.end());
    // Walk through populating the step counts.
    for (auto i = lines.begin(); i != lines.end(); i++) {
        dj = *i - jolts;
        steps[dj]++;
        jolts = *i;
    }
    // Adjust for final step of +3.
    steps[3]++;
    // Output result.
    return steps[1] * steps[3];
}

int main() {
    vector<int> lines;
    if (!read_input("input10.txt", lines)) {
        cerr << "Error reading data from file." << endl;
        return 1;
    }
    int result = day10part1(lines);
    cout << "Day 10, Part 1 Result = " << result << endl;
    return 0;
}
