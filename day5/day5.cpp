// Advent of Code 2020 - Day 5.

// F/B == 0/1
// L/R == 0/1

// Row == 7 bits; Col == 3 bits.

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <map>
#include <algorithm>

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
        if (line != "") {
            lines.push_back(line);
        }
    }
    ifs.close();
    return true;
}

unsigned decode(const string& code, unsigned& row, unsigned& col) {
    int len = code.length();
    const char* cp = code.c_str();
    unsigned delta = 1;
    unsigned value = 0;
    for (int i = len - 1; i >= 0; i--) {
        if (cp[i] == 'B' || cp[i] == 'R') {
            value += delta;
        }
        delta *= 2;
    }
    // Decode row and col numbers.
    col = value % 8;
    row = value / 8;
    return value;
}

unsigned max_seat_number(const vector<string>& codes) {
    unsigned max_seat = 0;
    unsigned seat = 0;
    unsigned r, c;
    for (auto code : codes) {
        seat = decode(code, r, c);
        if (seat > max_seat) {
            max_seat = seat;
        }
    }
    return max_seat;
}

pair<unsigned,unsigned> seat_plane(const vector<string>& codes, vector<unsigned>& seats) {
    unsigned max_seat = 0;
    unsigned min_seat = 1000;
    unsigned seat = 0;
    unsigned r, c;
    seats.clear();
    for (auto code : codes) {
        seat = decode(code, r, c);
        seats.push_back(seat);
        if (seat > max_seat) {
            max_seat = seat;
        }
        if (seat < min_seat) {
            min_seat = seat;
        }
    }
    pair<unsigned, unsigned> p(min_seat, max_seat);
    return p;
}

unsigned find_my_seat(const vector<string>& lines, vector<unsigned>& seats) {
    auto p = seat_plane(lines, seats);
    std::sort(seats.begin(), seats.end());
    unsigned last = 0;
    unsigned result = 0;
    for (auto s = seats.begin(); s != seats.end(); s++) {
        if (last == 0) {
            last = *s;
            continue;
        }
        if (2 == (*s - last)) {
            result = last + 1;
            cout << "Found seat " << result << "." << endl;
        }
        last = *s;
    }
    return result;
}

int main() {
    vector<string> lines;
    unsigned low, high;
    vector<unsigned> seats;
    if (!read_input("input5.txt", lines)) {
        cerr << "Error reading input." << endl;
    }
    else {
        unsigned seat = max_seat_number(lines);
        cout << "Part 1. Maximum seat number = " << seat << endl;
    }

    unsigned seat = find_my_seat(lines, seats);
    cout << "Part 2. Find my seat returns " << seat << endl;

    return 0;
}
