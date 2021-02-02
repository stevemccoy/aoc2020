/*
    Advent of Code 2020 - Day 12.
*/

#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cmath>
#include <complex>

using namespace std;

const double degs2rads = 3.14159265 / 180.0;

class State {
public:
    State() {
        z = std::complex<double>(0.0, 0.0);
        theta = 0.0;
    }
    std::complex<double> z;
    double theta;
};

class Ship {
public:
    Ship(double east, double north) {
        pos = std::complex<double>(0, 0);
        wp  = std::complex<double>(east, north);
    }

    std::complex<double> pos;
    std::complex<double> wp;

    void move(const string& instruction) {
        char opcode = instruction[0];
        double r, theta;
        double distance = atof(instruction.substr(1).c_str());
        switch (opcode) {
            case 'N' :
                wp = std::complex<double>(wp.real(), wp.imag() + distance);
                break;
            case 'S' :
                wp = std::complex<double>(wp.real(), wp.imag() - distance);
                break;
            case 'E' :
                wp = std::complex<double>(wp.real() + distance, wp.imag());
                break;
            case 'W' :
                wp = std::complex<double>(wp.real() - distance, wp.imag());
                break;
            case 'L' :
                theta = std::arg(wp) + distance * degs2rads;
                r = std::abs(wp);
                wp = std::polar(r, theta);
                break;
            case 'R' :
                theta = std::arg(wp) - distance * degs2rads;
                r = std::abs(wp);
                wp = std::polar(r, theta);
                break;
            case 'F' :
                pos += wp * distance;
                break;
            default :
                throw "Illegal opcode.";
        }
    }
};

void move(State& s, const string& instruction) {
    char opcode = instruction[0];
    double distance = atof(instruction.substr(1).c_str());
    switch (opcode) {
        case 'N' :
            s.z = std::complex<double>(s.z.real(), s.z.imag() + distance);
            break;
        case 'S' :
            s.z = std::complex<double>(s.z.real(), s.z.imag() - distance);
            break;
        case 'E' :
            s.z = std::complex<double>(s.z.real() + distance, s.z.imag());
            break;
        case 'W' :
            s.z = std::complex<double>(s.z.real() - distance, s.z.imag());
            break;
        case 'L' :
            s.theta += distance;
            while (s.theta > 360.0) {
                s.theta -= 360.0;
            }
            break;
        case 'R' :
            s.theta -= distance;
            while (s.theta < 0.0) {
                s.theta += 360.0;
            }
            break;
        case 'F' :
            s.z += std::polar(distance, s.theta * degs2rads);
            break;
        default :
            throw "Illegal opcode.";
    }
}

int day12part2() {
    Ship ship(10, 1);

    ifstream ifs("input12.txt", ios::in);
    if (!ifs.is_open()) {
        cerr << "Failed to open input file." << endl;
        return 1;
    }
    string line;
    while (!ifs.eof()) {
        getline(ifs, line);
        if (!line.empty()) {
            ship.move(line);
        }
    }
    ifs.close();
    // Where is the ship now?
    double x = ship.pos.real();
    double y = ship.pos.imag();
    cout << "Part 2 - End position of ship: (" << x << ", " << y << ")." << endl;
    cout << "Manhattan distance: " << (abs(x) + abs(y)) << " from origin." << endl;
    return 0;
}

// Read instructions from the specified file.
int day12part1() {
    ifstream ifs("input12.txt", ios::in);
    if (!ifs.is_open()) {
        cerr << "Failed to open input file." << endl;
        return 1;
    }
    string line;
    State s;
    while (!ifs.eof()) {
        getline(ifs, line);
        if (!line.empty()) {
            move(s, line);
        }
    }
    ifs.close();
    // Where is the ship now?
    double x = s.z.real();
    double y = s.z.imag();
    cout << "Part 1 - End position of ship: (" << x << ", " << y << "), heading " << s.theta << endl;
    cout << "Manhattan distance: " << (abs(x) + abs(y)) << " from origin." << endl;
    return 0;
}

int main() {
    int ret = day12part1();
    ret = day12part2();
    return ret;
}
