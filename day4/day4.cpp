
#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <map>

using namespace std;

const char* digits = "0123456789";
const char* whitespace = " \t";

class Ident
{
private:
    std::map<string, string> m_map;

    void separate_units(const string& value, int& number, string& units) {
        size_t p = value.find_first_not_of(digits);
        units =  (p == string::npos) ? "" : value.substr(p);
        number = std::atoi((value.substr(0, p)).c_str());
    }

public:
    Ident() {}

    void Reset() {
        m_map.clear();
    }
    
    void Add(const string& key, const string& value) {
        m_map[key] = value;
    }

    bool Has(const string& key) {
        return (m_map.find(key) != m_map.end());
    }

    bool Verify() {
        return Has("byr") && Has("iyr") && Has("eyr") && Has("hgt") && Has("hcl") && Has("ecl") && Has("pid");
    }

    bool DataValid() {
        string k, v, vu;
        int yr = 0, vv = 0;
        for (auto p = m_map.begin(); p != m_map.end(); p++) {
            k = p->first;
            v = p->second;

            if (k == "byr") {
                yr = std::atoi(v.c_str());
                if (yr < 1920 || yr > 2002) {
                    return false;
                }
            }
            else if (k == "iyr") {
                yr = std::atoi(v.c_str());
                if (yr < 2010 || yr > 2020) {
                    return false;
                }                
            }
            else if (k == "eyr") {
                yr = std::atoi(v.c_str());
                if (yr < 2020 || yr > 2030) {
                    return false;
                }
            }
            else if (k == "hgt") {
                separate_units(v, vv, vu);
                if (vu == "cm") {
                    if (vv < 150 || vv > 193) {
                        return false;
                    }
                }
                else if (vu == "in") {
                    if (vv < 59 || vv > 76) {
                        return false;
                    }
                }
                else {
                    return false;
                }               
            }
            else if (k == "hcl") {
                if (v[0] != '#' || v.length() != 7) {
                    return false;
                }
                if (v.find_first_not_of("#1234567890abcdefABCDEF") != string::npos) {
                    return  false;
                }
            }
            else if (k == "ecl") {
                if ( !(v == "amb" || v == "blu" || v == "brn" || v == "gry" || v == "grn" || v == "hzl" || v == "oth")) {
                    return false;
                }                
            }
            else if (k == "pid") {
                if (v.length() != 9 || v.find_first_not_of(digits) != string::npos) {
                    return false;
                }
            }
        }
        return true;
    }

    int KeyCount() {
        return m_map.size();
    }

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

string trim(const string& s) {
    auto first = s.find_first_not_of(whitespace);
    auto last = s.find_last_not_of(whitespace);
    if (last == string::npos || first == string::npos) {
        return string();
    }
    return s.substr(first, last - first + 1);
}

bool grab_pair(string& line, string& key, string& value) {
    auto start = line.find_first_not_of(whitespace);
    if (string::npos == start) {
        return false;
    }
    auto end = line.find_first_of(whitespace, start);
    if (string::npos == end) {
        end = line.length() - 1;
    }
    string pair = line.substr(start, (end - start + 1));
    auto sep = pair.find(":");
    key = trim(pair.substr(0, sep));
    value = trim(pair.substr(sep + 1));
    line = line.substr(end + 1);
    return true;
}

void read_idents(const vector<string>& lines, vector<Ident>& idents) {
    idents.clear();
    Ident id;
    for (auto it = lines.begin(); it != lines.end(); it++) {
        string line = *it;
        if (line.empty()) {
            if (id.KeyCount() > 0) {
                idents.push_back(id);
            }
            id.Reset();
        }
        else {
            string s = line;
            string key, value;
            while (grab_pair(s, key, value)) {
                id.Add(key, value);
            }
        }
    }
}

void loose_validate_idents(const vector<Ident>& idents, vector<Ident>& valid) {
    valid.clear();
    for (auto id : idents) {
        if (id.Verify()) {
            valid.push_back(id);
        }
    }
}

void strict_valid_idents(const vector<Ident>& ins, vector<Ident>& outs) {
    outs.clear();
    for (auto id : ins) {
        if (id.Verify() && id.DataValid()) {
            outs.push_back(id);
        }
    }
}

int main() {
    std::vector<std::string> lines;
    vector<Ident> idents;
    vector<Ident> valid_idents;
    try {
    if (!read_input("input4.txt", lines)) {
        std::cerr << "Error when attempting to read from input file." << std::endl;
    }
    else {
        std::cout << "Read " << lines.size() << " lines from input file." << std::endl;
    }
    read_idents(lines, idents);
    loose_validate_idents(idents, valid_idents);
    cout << "Part 1. Number of valid documents = " << valid_idents.size() << endl;
    strict_valid_idents(idents, valid_idents);
    cout << "Part 2. Number of valid documents = " << valid_idents.size() << endl;
    }
    catch (exception e) {
        cerr << e.what() << endl;
    }
    return 0;
}
