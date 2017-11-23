#include <iostream>
#include <string>
using namespace std;

string dictionary = "Data\\";

void createFile() {
    // char
    for (int i = 0; i < 26; i++)
        fopen((dictionary + (char)('A' + i) + ".txt").c_str(), "w");
    // digit
    for (int i = 0; i < 10; i++)
        fopen((dictionary + (char)('0' + i) + ".txt").c_str(), "w");
}

string divideStr(string& str) {
    string returnStr;
    for (int i = 0; i < 5; i++) {
        char ch = str[0];
        if (str[0] == '\t') {
            returnStr += '0';
            str.erase(0, 1);
        }
        else {
            returnStr += '1';
            str.erase(0, 2);
        }
    }
    
    // erase '\t'
    str.erase(0, 1);
    return returnStr;
}

void loadFile() {
    FILE *fin = fopen("Data\\DigitAndLitterTable.xls", "r");
    char s[200];
    
    // deal with A-Y 
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 7; j++) {
            fgets(s, 200, fin);
            string str(s);
            for (int k = 0; k < 5; k++) {
                FILE *fout = fopen((dictionary + (char)('A' + i * 5 + k) + ".txt").c_str(), "a");
                string inputText = divideStr(str);
                fprintf(fout, "%s\n", inputText.c_str());
            }
        }
        fgets(s, 200, fin);
    }
    
    // deal with Z
    for (int k = 0; k < 7; k++) {
        FILE *fout = fopen((dictionary + 'Z' + ".txt").c_str(), "a");
        fgets(s, 200, fin);
        string str(s);
        string inputText = divideStr(str);
        fprintf(fout, "%s\n", inputText.c_str());
    }
    fgets(s, 200, fin);
    
    // deal with 0-9
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 7; j++) {
            fgets(s, 200, fin);
            string str(s);
            for (int k = 0; k < 5; k++) {
                FILE *fout = fopen((dictionary + (char)('0' + i * 5 + k) + ".txt").c_str(), "a");
                string inputText = divideStr(str);
                fprintf(fout, "%s\n", inputText.c_str());
            }
        }
        fgets(s, 200, fin);
    }
}

int main() {
    loadFile();
} 
