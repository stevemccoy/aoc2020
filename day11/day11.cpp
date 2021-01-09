/*
    Advent of Code 2020 - Day 11.
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
    Grid() {
        m_rows = m_cols = 0;
        m_data.clear();
    }

    void Init(int num_cols, int num_rows) {
        m_cols = num_cols;
        m_rows = num_rows;
        m_data.resize(m_cols, std::vector<char>(num_rows, '.'));
    }

    int numCols() const { return m_cols; }
    int numRows() const { return m_rows; }

    char get(int c, int r) const 
    {
        valid_coord(c, r);
        return m_data[c][r];
    }

    void set(int c, int r, char v) {
        valid_coord(c, r);
        m_data[c][r] = v;
    }

    int neighbours(int c, int r) const {
        int nc, nr;
        int count = 0;
        for (int dc = -1; dc < 2; dc++) {
            nc = c + dc;
            if (nc < 0 || nc >= m_cols) {
                continue;
            }
            for (int dr = -1; dr < 2; dr++) {
                nr = r + dr;
                if (nr < 0 || nr >= m_rows) {
                    continue;
                }
                if (dr == 0 && dc == 0) {
                    continue;
                }
                if (m_data[nc][nr] == '#') {
                    count++;
                }
            }
        }
        return count;
    }

   int visibleOccupied(int c, int r) const {
        int nc, nr, k;
        char ch;
        int count = 0;
        for (int dc = -1; dc < 2; dc++) {
            for (int dr = -1; dr < 2; dr++) {
                if (dr == 0 && dc == 0) {
                    continue;
                }
                k = 1;
                while (true) {
                    nc = c + k * dc;
                    nr = r + k * dr;
                    if (nc < 0 || nc >= m_cols || nr < 0 || nr >= m_rows) {
                        break;
                    }
                    ch = m_data[nc][nr];
                    if (ch == '.') {
                        k++;
                    }
                    else {
                        if (ch == '#') {
                            count++;
                        }
                        break;
                    }
                }
            }
        }
        return count;
    }

    int occupied() const {
        int count = 0;
        for (int c = 0; c < m_cols; c++) {
            for (int r = 0; r < m_rows; r++) {
                if (m_data[c][r] == '#') {
                    count++;
                }
            }
        }
        return count;
    }

private:
    int m_rows;
    int m_cols;
    vector<vector<char>> m_data;

    void valid_coord(int c, int r) const {
        if (c < 0 || c >= m_cols || r < 0 || r >= m_rows) {
            throw "Coordinate out of range";
        }
    }
};

// Read all the lines from the specified input file.
bool read_input(const string& inputFile, Grid& grid) {
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
            lines.push_back(line);
        }
    }
    ifs.close();
    // Convert from vector to Grid
    int rows = lines.size();
    int cols = lines[0].length();
    grid.Init(cols, rows);
    for (int r = 0; r < rows; r++) {
        line = lines[r];
        for (int c = 0; c < cols; c++) {
            grid.set(c, r, line[c]);
        }
    }
    return true;
}

void display_grid(const Grid& grid) {
    char *s = new char[grid.numCols()];
    for (int r = 0; r < grid.numRows(); r++) {
        for (int c = 0; c < grid.numCols(); c++) {
            s[c] = grid.get(c, r);
        }
        cout << s << endl;
    }
    delete s;
    cout << endl;
}

int processPart1(const Grid& src, Grid& dest) {
    char ch;
    int changes = 0;
    for (int c = 0; c < src.numCols(); c++) {
        for (int r = 0; r < src.numRows(); r++) {
            ch = src.get(c, r);
            switch (ch) {
                case '.' :
                    break;
                case 'L' :
                    if (src.neighbours(c, r) == 0) {
                        ch = '#';
                        changes++;
                    }
                    break;
                case '#' :
                    if (src.neighbours(c, r) >= 4) {
                        ch = 'L';
                        changes++;
                    }
                    break;
                default :   throw "Unexpected character value in grid.";
            }
            dest.set(c, r, ch);
        }
    }
    return changes;
}

int processPart2(const Grid& src, Grid& dest) {
    char ch;
    int changes = 0;
    for (int c = 0; c < src.numCols(); c++) {
        for (int r = 0; r < src.numRows(); r++) {
            ch = src.get(c, r);
            switch (ch) {
                case '.' :
                    break;
                case 'L' :
                    if (src.visibleOccupied(c, r) == 0) {
                        ch = '#';
                        changes++;
                    }
                    break;
                case '#' :
                    if (src.visibleOccupied(c, r) >= 5) {
                        ch = 'L';
                        changes++;
                    }
                    break;
                default :
                    throw "Unexpected character value in grid.";
            }
            dest.set(c, r, ch);
        }
    }
    return changes;
}

int day11part1(const Grid& inGrid) {
    Grid src = inGrid;
    Grid dest = inGrid;
    while (processPart1(src, dest) > 0) {
        display_grid(dest);
        src = dest;
    }
    return dest.occupied();
}

int day11part2(const Grid& inGrid) {
    Grid src = inGrid;
    Grid dest = inGrid;
    while (processPart2(src, dest) > 0) {
        display_grid(dest);
        src = dest;
    }
    return dest.occupied();
}

int main() {
    vector<int> lines;
    Grid grid;
    if (!read_input("input11.txt", grid)) {
        cerr << "Error reading data from file." << endl;
        return 1;
    }
    int count = day11part1(grid);
    cout << "Day 11, Part 1: Count of occupied seats: " << count << endl;
    count = day11part2(grid);
    cout << "Day 11, Part 2: Number of occupied seats: " << count << endl;
    return 0;
}
