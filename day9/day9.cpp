/*
    Advent of Code 2020 - Day 9.
*/

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm>

using namespace std;

// Read all the lines from the specified input file.
bool read_input(const string& inputFile, vector<int64_t>& lines) {
    ifstream ifs(inputFile, ios::in);
    if (!ifs.is_open()) {
        cerr << "Failed to open input file." << endl;
        return false;
    }
    string line;
    while (!ifs.eof()) {
        getline(ifs, line);
        lines.push_back(atol(line.c_str()));
    }
    ifs.close();
    return true;
}

int64_t day9part1(const vector<int64_t>& lines) {
    const int buf_size = 25;
    int64_t result = 0;
    int64_t buffer[buf_size];
    int64_t sum = 0;
    int index = 0;
    int num_lines = lines.size();
    int oldie = 0;
    bool found = false;
    // Fill buffer.
    for (index = 0; index < buf_size; index++) {
        buffer[index] = lines[index];
    }
    // Check subsequent values.
    while (index < num_lines) {
        sum = lines[index];
        found = false;
        for (int p1 = 0; !found &&  p1 < buf_size; p1++) {
            for (int p2 = p1 + 1; p2 < buf_size; p2++) {
                if (buffer[p1] + buffer[p2] == sum) {
                    found = true;
                    break;
                }
            }
        }
        if (!found) {
            result = sum;
            break;
        }
        buffer[oldie++] = sum;
        if (oldie == buf_size) {
            oldie = 0;
        }
        index++;
    }
    // Report answer.
    cout    << "Part 1. First entry not a sum of a pair from preceding 25 entries : "
            << result << endl;
    return result;
}

int64_t day9part2(const int64_t target, const vector<int64_t>& lines) {
    int num_lines = lines.size();
    int64_t sum = 0;
    // Produce the given target by summing contiguous values from the list.
    for (auto start = lines.begin(); start != lines.end(); start++) {
        sum = *start;
        for (auto end = start + 1; end != lines.end(); end++) {
            sum += *end;
            if (sum == target) {
                // Hooray!
                cout << "Found contiguous group summing to target value of " << target << endl;
                for (auto k = start; k <= end - 1; k++) {
                    cout << (k == start ? "   " : " + ") << *k << endl;
                }
                cout << " = " << target << endl;
                int64_t smallest    = *std::min_element(start, end);
                int64_t largest     = *std::max_element(start, end);
                int64_t result      = smallest + largest;
                cout << "Return value = " << smallest << " + " << largest << " = " << result << endl;
                return result;
            }
            if (sum > target) {
                break;
            }
        }
    }
    // In case no find.
    return 0;
}

int main() {
    vector<int64_t> lines;
    if (!read_input("input9.txt", lines)) {
        cerr << "Error reading data from file." << endl;
        return 1;
    }
    int64_t target = day9part1(lines);
    if (day9part2(target, lines) == 0) {
        cerr << "No contiguous range found summing to target value." << endl;
        return 1;
    }
    return 0;
}
