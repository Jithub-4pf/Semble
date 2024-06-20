#include <iostream>
#include <string>

int main(int argc, char** argv) {
    if (argc >= 2) {
        char* filename = argv[1];
        std::string fn(filename);
        std::string begin = "python3 ~/.semble/Semble/main.py ";
        system((begin + fn).c_str());
        exit(0);
    } else {
        std::cout << "Usage: semble <file>\n";
        exit(1);
    }

    return 0;
}