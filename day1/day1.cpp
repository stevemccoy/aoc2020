#include <iostream>
#include <vector>
#include <cstdio>

bool read_input(std::vector<int>& values) {
    const char* fileName = "input1.txt";
    int value;
    values.clear();
    FILE *fp = fopen(fileName, "r");
    if (fp == nullptr) {
        std::cerr << "Error: Input file " << fileName << " not opened." << std::endl;
        return false;
    }
    while (!feof(fp)) {
        fscanf(fp, "%d", &value);
        values.push_back(value);
    }
    fclose(fp);
    return true;
}

// Find 2 numbers summing to 2020.
bool day1_part1(const std::vector<int>& values, int& value1, int& value2) {
    for (auto it = values.begin(); it != values.end(); it++) {
        value1 = *it;
        int v1 = 2020 - value1;
        for (auto j = it + 1; j != values.end(); j++) {
            if (*j == v1) {
                value2 = *j;
                return true;
            }
        }
    }
    return false;
}

// Find 3 numbers summing to 2020.
bool day1_part2(const std::vector<int>& values, int& value1, int& value2, int& value3) {
    for (auto i = values.begin(); i != values.end(); i++) {
        value1 = *i;
        int v1 = 2020 - value1;
        for (auto j = i + 1; j != values.end(); j++) {
            value2 = *j;
            if (v1 > value2) {
                v1 -= value2;
                for (auto k = j + 1; k != values.end(); k++) {
                    if (*k == v1) {
                        value3 = *k;
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

int main() {
    std::vector<int> values;
    int v1, v2, v3;

    std::cout << "Reading input file... ";
    std::cout << (read_input(values) ? "success" : "fail") << std::endl;
    std::cout << "Read " << values.size() << " values from file." << std::endl;
    if (values.size() > 0) {
        if (!day1_part1(values, v1, v2)) {
            std::cout << "Failed to find solution to part 1." << std::endl;
        }
        else {
            std::cout << "Part 1 Solution: 2020 = " << v1 << " + " << v2 << std::endl;
            std::cout << v1 << " * " << v2 << " = " << (v1 * v2) << std::endl;
        }

        if (!day1_part2(values, v1, v2, v3)) {
            std::cout << "Failed to find solution to part 2." << std::endl;
        }
        else {
            std::cout << "Part 2 Solution: 2020 = " << v1 << " + " << v2 << " + " << v3 << std::endl;
            std::cout << v1 << " * " << v2 << " * " << v3 << " = " << (v1 * v2 * v3) << std::endl;
        }
    }

    return 0;
}
