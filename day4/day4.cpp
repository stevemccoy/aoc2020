
#include <vector>
#include <string>
#include <fstream>
#include <stdio>

bool read_input(std::vector<std::string>& lines) {
    char buffer[250];
    std::ifstream ifs("input4.txt", std::ifstream::in);
    while (!ifs.eof()) {
        ifs.getline(buffer, 250);
        lines.push_back(buffer);
    }
    ifs.close();
}

int main() {
    std::vector<std::string> lines;
    if (!read_input(lines)) {
        std::cerr << "Error when attempting to read from input file." << std::endl;
    }
    else {
        std::cout << "Read " << lines.count() << " lines from input file." << std::endl;
    }
    return 0;
}

