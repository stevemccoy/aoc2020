/*
    Advent of Code 2020 - Day 8.
*/

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <map>
#include <algorithm>
#include <bitset>

using namespace std;

typedef enum { unknown = 0, acc = 1, jmp = 2, nop = 3, stop = 4 } OpCode;

class Instruction {
public:
    Instruction(const string& line) {
        string opstring = line.substr(0, line.find(' '));
        string argstring = line.substr(line.find(' ')+1);
        if (opstring == "acc") {
            opcode = acc;
        }
        else if (opstring == "jmp") {
            opcode = jmp;
        }
        else if (opstring == "nop") {
            opcode = nop;
        }
        else if (opstring == "stop") {
            opcode = stop;
        }
        else {
            opcode = unknown;
            cerr << "Error: Unrecognised opcode " << opstring << endl;
        }
        argument = atoi(argstring.c_str());
        exec_count = 0;
    }

    OpCode opcode;
    int argument;
    int exec_count;
};

class Day8Machine 
{
public:
    Day8Machine() { Clear(); }

    void Clear() {
        m_program.clear();
        Reset();
    }

    // Reset machine for the next run.
    void Reset() {
        m_ip = 0;
        m_acc = 0;
        m_stop_requested = false;
        for (auto i : m_program) {
            i.exec_count = 0;
        }
    }

    void Load(const vector<string>& lines) {
        Clear();
        int i = 0;
        for (auto line : lines) {
            if (!line.empty()) {
                m_program.push_back(Instruction(line));
            }
        }
        // Add implicit stop instruction to the end of the buffer.
        m_program.push_back(Instruction("stop +0"));
    }

    void Execute(Instruction& i) {
        i.exec_count++;
        switch(i.opcode) {
            case acc :  m_acc += i.argument;
                        m_ip++;
                        break;
            case jmp :  m_ip += i.argument;
                        break;
            case nop :  m_ip++;
                        break;
            case stop : m_stop_requested = true;
                        break;
            case unknown:
                        cerr << "Exec unknown opcode." << endl;
        }
    }

    void RunTillCycle() {
        while (!m_stop_requested && (m_program[m_ip].exec_count == 0)) {
            Execute(m_program[m_ip]);
        }
    }

    int Accumulator() const {
        return m_acc;
    }

    int IP() const {
        return m_ip;
    }

    bool Stopped() const {
        return m_stop_requested;
    }

    int LastAddress() const {
        return m_program.size() - 1;
    }

    // Change the program at position i, nop <-> jmp, if possible.
    bool Mutate(int i) {
        switch (m_program[i].opcode) {
            case nop :  m_program[i].opcode = jmp;
                        break;
            case jmp :  m_program[i].opcode = nop;
                        break;
            default :   return false;
        }
        return true;
    }

private:
    vector<Instruction> m_program;
    int m_ip;
    int m_acc;
    bool m_stop_requested;
};

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

void day8part1(const vector<string>& lines) {
    Day8Machine m;
    m.Load(lines);
    m.RunTillCycle();
    cout    << "Part 1. Value of accumulator at first repeated instruction is: " << m.Accumulator() 
            << " (IP: " << m.IP() << ")" << endl;
}

void day8part2(const vector<string>& lines) {
    Day8Machine m;
    m.Load(lines);
    int n = m.LastAddress();
    // Try changing each instruction nop <-> jmp until program runs to the stop point.
    for (int i = 0; i < n; i++) {
        m.Load(lines);
        if (m.Mutate(i)) {
            m.RunTillCycle();
            if (m.Stopped()) {
                break;
            }
            // Undo the mutation in this spot.
//            m.Mutate(i);
//            m.Reset();
        }
    }

    if (m.Stopped()) {
        cout    << "Part 2. Point change found allowing program to run to completion. Value of accumulator is: "
                << m.Accumulator() << " (IP: " << m.IP() << ")" << endl;
    }
    else {
        cout    << "Part 2. No point change found which allows the program to run to a stop." << endl;
    }
}

int main() {
    vector<string> lines;
    if (!read_input("input8.txt", lines)) {
        cerr << "Error reading data from file." << endl;
        return 1;
    }
    day8part1(lines);
    day8part2(lines);
    return 0;
}
