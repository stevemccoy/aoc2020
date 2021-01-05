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

class Grid
{
public:
    Grid(int num_cols, num_rows) {
        m_cols = num_cols;
        m_rows = num_rows;
        m_data = new char[m_cols][m_rows];
    }

    ~Grid() {
        delete m_data;
    }

    int numCols() { return m_cols; }
    int numRows() { return m_rows; }

    char get(int c, int r) const 
    {
        valid_coord(c, r);
        return m_data[c][r];
    }

    void set(int c, int r, char v) {
        valid_coord(c, r);
        m_data[c][r] = v;
    }

private:
    int m_rows;
    int m_cols;
    char **m_data;

    void valid_coord(int c, int r) {
        if (c < 0 || c >= m_cols || r < 0 || r >= m_rows) {
            cerr << "Coordinate out of range (" << c << ", " << r << ")" << endl;
            exit(1);
        }
    }
};

// Read all the lines from the specified input file.
bool read_input(const string& inputFile, Grid *grid) {
    ifstream ifs(inputFile, ios::in);
    if (!ifs.is_open()) {
        cerr << "Failed to open input file." << endl;
        return false;
    }
    vector<string> lines;
    string line;
    while (!ifs.eof()) {
        getline(ifs, line);
        if (!line.empty()) {
            lines.push_back(atoi(line.c_str()));
        }
    }
    ifs.close();
    // Convert from vector to Grid
    int rows = lines.count();
    int cols = lines[0].count();
    grid = new Grid(cols, rows);
    for (int r = 0; r < rows; r++) {
        line = lines[r];
        for (int c = 0; c < cols; c++) {
            grid->set(c, r, line[c]);
        }
    }
    return true;
}

int day11part1(vector<int>& lines) {
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
    Grid *grid = nullptr;
    if (!read_input("input11.txt", grid)) {
        cerr << "Error reading data from file." << endl;
        return 1;
    }
    return 0;
}
