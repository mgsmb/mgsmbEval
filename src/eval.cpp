#include <iostream>
#include <string>
#include <sstream>
#include "eval.h"
#include "mgsmbMath.h" // my own library for math functions https://github.com/j-gehrig/math

#define stringSizeType  std::string::size_type

using namespace std;

static bool isMathOperatorOrDecimalPoint(char c) {
    const char operators[5] = { '+', '-', '*', '/', ',' };
    for (char operatorVar : operators) {
        if (c == operatorVar) return true;
    }
    return false;
}
static bool isBracket(char c) {
    const char brackets[2] = { '(', ')' };
    for (char bracket : brackets) {
        if (c == bracket) return true;
    }
    return false;
}
static bool isPartOfMathFunction(char c) {
    return (!isdigit(c) && !isBracket(c) && !isMathOperatorOrDecimalPoint(c));
}
static bool isStartOfLeftNumber(char firstChar, char secondChar, int mode, char mathOperator) {
    bool res = (!isdigit(secondChar) && secondChar != '.')
        && (mode == 1
            && (secondChar == '+' || secondChar == '-')
            && firstChar != mathOperator);
    return res;
}
static bool isEndOfRightNumber(char b) {
    bool res = (!isdigit(b) && b != '.');
    return res;
}


// -- eval functions --

static void evalArithmeticOperators(string& term, int mode) {
    // This function calculates the parts of the term with the basic arithmetic operators (+-*/)
    // The mode determines the order of the calculations

    const char operators[4] = { '*', '/', '+', '-' };
    int operatorOneIndex = 2 * mode; // either  2 * 0 = 0  or  2 * 1 = 2
    int operatorTwoIndex = 2 * mode + 1;

    while (mode == 0 || mode == 1) {
        // Loops until all the operators are calculated (except a possible sign at the beginning)
        if ((term.substr(1).find(operators[operatorOneIndex]) == std::string::npos) && (term.substr(1).find(operators[operatorTwoIndex]) == std::string::npos)) {
            // If the operators of the mode are present in the string the mode is changed
            mode++;
            operatorOneIndex = 2 * mode;
            operatorTwoIndex = 2 * mode + 1;
            continue;
        }
        for (stringSizeType charIndex = 1; charIndex < term.size(); charIndex++) {
            // Go through every character of the term

            if (term[charIndex] == operators[operatorOneIndex] || term[charIndex] == operatorTwoIndex) {
                // Checks whether current char is the one corresponding to the operator of the mode

                double leftNumber = 0; // Number to the left of the operator
                double rightNumber = 0; // Number to the right of the operator
                double calculatedNumber = 0;

                stringSizeType startIndexOfLeftNumber = 0;
                stringSizeType endIndexOfRightNumber = 0;

                char operatorVar = term[charIndex];
                stringSizeType operatorIndex = charIndex;

                // Consider making seperate functions
                for (stringSizeType leftCharIndex = operatorIndex - 1; ; leftCharIndex--) {
                    // Goes through the chars to the left of the operator to find the left Number
                    char secondChar = term[leftCharIndex];
                    char firstChar;
                    if (leftCharIndex > 0) {
                        // This makes sure the index is never out of bounds
                        firstChar = term[leftCharIndex - 1];
                    }
                    else {
                        firstChar = secondChar;
                    }
                    if (isStartOfLeftNumber(firstChar, secondChar, mode, operatorVar) && term[0] != '-') {
                        // Checks whether the beginning of the first number, which is being calculated, is reached
                        // Also makes sure a negative sign isn't being interpreted as an operator
                        startIndexOfLeftNumber = leftCharIndex + 1;
                        stringSizeType substrIndexDelta = operatorIndex - 1 - startIndexOfLeftNumber - 1;
                        leftNumber = ::atof(term.substr(startIndexOfLeftNumber, substrIndexDelta).c_str());
                        // Substring is converted to C String first, so it can be converted to float using atof
                        break; // Left Number is found
                    }
                    if (leftCharIndex == 0) {
                        // The left number is the first number of the term
                        leftNumber = ::atof(term.substr(0, operatorIndex).c_str());
                        // Substring is converted to C String first, so it can be converted to float using atof
                        startIndexOfLeftNumber = 0;
                        break; // Left Number is found
                    }
                }

                for (stringSizeType rightCharIndex = operatorIndex + 1; rightCharIndex < term.size(); rightCharIndex++) {
                    // Goes through the chars to the right of the operator to find the right Number

                    if (rightCharIndex == term.size() - 1) {
                        // Checks whether the last character is reached
                        stringSizeType deltaIndex = rightCharIndex - operatorIndex;
                        // The number of chars the loop has gone through since the beginning of the loop
                        if (deltaIndex == 1) { // Right number is the last digit
                            rightNumber = (double)(term[operatorIndex + 1] - '0'); // digit is converted to int and then cast to double
                        }
                        else {
                            rightNumber = ::atof(term.substr(operatorIndex + 1, deltaIndex).c_str());
                            // Substring is converted to C String first, so it can be converted to float using atof
                        }
                        endIndexOfRightNumber = rightCharIndex;
                        break; // Left Number is found
                    }

                    char secondRightChar = term[rightCharIndex + 1];
                    if (isEndOfRightNumber(secondRightChar)) {
                        stringSizeType deltaIndex = rightCharIndex - operatorIndex;
                        // The number of chars the loop has gone through since the beginning of the loop
                        rightNumber = ::atof(term.substr(operatorIndex + 1, deltaIndex).c_str());
                        // Substring is converted to C String first, so it can be converted to float using atof
                        endIndexOfRightNumber = rightCharIndex;
                        break; // Left Number is found
                    }
                }

                if (operatorVar == '*') {
                    calculatedNumber = leftNumber * rightNumber;
                }
                else if (operatorVar == '/') {
                    calculatedNumber = leftNumber / rightNumber;
                }
                else if (operatorVar == '+') {
                    calculatedNumber = leftNumber + rightNumber;
                }
                else if (operatorVar == '-') {
                    calculatedNumber = leftNumber - rightNumber;
                }

                ostringstream stream;
                if (startIndexOfLeftNumber == 0) {
                    stream << calculatedNumber << term.substr(endIndexOfRightNumber + 1);
                    term = stream.str();
                }
                else if (endIndexOfRightNumber == term.size() - 1) {
                    stream << term.substr(0, startIndexOfLeftNumber) << calculatedNumber;
                    term = stream.str();
                }
                else {
                    stream << term.substr(0, startIndexOfLeftNumber) << calculatedNumber << term.substr(endIndexOfRightNumber + 1);
                    term = stream.str();
                }
                break; // Breaks out of for loop
            }
        }
    }

    return;
}

static void addMissingBrackets(string& term) {
    int openedBracketCount = 0; // total opened brackets
    int closedBracketCount = 0; // total closed brackets

    for (stringSizeType charIndex = 0; charIndex < term.size(); charIndex++) {
        // Loops through the term to find all brackets in the term
        if (term[charIndex] == '(') {
            openedBracketCount++;
        }
        else if (term[charIndex] == ')') {
            closedBracketCount++;
        }
    }

    int bracketCountDifference = closedBracketCount - openedBracketCount;
    // The difference of the closed and opened brackets
    for (; bracketCountDifference < 0; bracketCountDifference++) {
        // Less closed than opened; need to add closed brackets at the end
        term = term.append(")");
    }
    for (; bracketCountDifference > 0; bracketCountDifference--) {
        // More closed than opened; need to add opened brackets at the beginning
        term = "(" + term;
    }
    return;
}

static double calcMathFunction(string& mathFunction, const int argumentCount, const char** arguments) {
    if (mathFunction == "pow") {
        if (argumentCount != 2) {
            cout << "Invalid amout of arguments! (2 needed)" << endl;
            return 0;
        }
        return Math::pow(std::stod(arguments[0]), std::stod(arguments[1]));

    }
    else if (mathFunction == "exp") {
        if (argumentCount != 1) {
            cout << "Invalid amout of arguments! (1 needed)" << endl;
            return 0;
        }
        return Math::exp(std::stod(arguments[0]));

    }
    else if (mathFunction == "log") {
        if (argumentCount != 2) {
            cout << "Invalid amout of arguments! (2 needed)" << endl;
            return 0;
        }
        return Math::log(std::stod(arguments[0]), std::stod(arguments[1]));

    }
    else if (mathFunction == "ln") {
        if (argumentCount != 1) {
            cout << "Invalid amout of arguments! (1 needed)" << endl;
            return 0;
        }
        return Math::ln(std::stod(arguments[0]));

    }
    else if (mathFunction == "sin") {
        if (argumentCount != 1) {
            cout << "Invalid amout of arguments! (1 needed)" << endl;
            return 0;
        }
        return Math::sin(std::stod(arguments[0]));

    }
    else if (mathFunction == "cos") {
        if (argumentCount != 1) {
            cout << "Invalid amout of arguments! (1 needed)" << endl;
            return 0;
        }
        return Math::cos(std::stod(arguments[0]));

    }
    else if (mathFunction == "tan") {
        if (argumentCount != 1) {
            cout << "Invalid amout of arguments! (1 needed)" << endl;
            return 0;
        }
        return Math::tan(std::stod(arguments[0]));

    }
    else if (mathFunction == "arcsin") {
        if (argumentCount != 1) {
            cout << "Invalid amout of arguments! (1 needed)" << endl;
            return 0;
        }
        return Math::arcsin(std::stod(arguments[0]));

    }
    else if (mathFunction == "arccos") {
        if (argumentCount != 1) {
            cout << "Invalid amout of arguments! (1 needed)" << endl;
            return 0;
        }
        return Math::arccos(std::stod(arguments[0]));

    }
    else if (mathFunction == "arctan") {
        if (argumentCount != 1) {
            cout << "Invalid amout of arguments! (1 needed)" << endl;
            return 0;
        }
        return Math::arctan(std::stod(arguments[0]));

    }
    else {
        cout << "Error: Mathematical function \"" << mathFunction << "\" is invalid!" << endl;
        return 0;
    }
}

static void evalInnerBracket(string& term) {
    // This function calculates the last inner bracket pair
    stringSizeType innerOpenedBracketIndex = 0;
    stringSizeType innerClosedBracketIndex = 0;

    for (stringSizeType charIndex = term.size(); charIndex > 0; charIndex--) {
        // Find last opened bracket
        if (term[charIndex] == '(') {
            innerOpenedBracketIndex = charIndex;
            for (stringSizeType otherCharIndex = charIndex; otherCharIndex < term.size(); otherCharIndex++) {
                // Find the corresponding closed bracket
                if (term[otherCharIndex] == ')') {
                    innerClosedBracketIndex = otherCharIndex;
                    break;
                }
            }
            break;
        }
        if (charIndex == 1) { // Again to avoid VS warn
            if (term[charIndex] == '(') {
                innerOpenedBracketIndex = charIndex;
                for (stringSizeType otherCharIndex = charIndex; otherCharIndex < term.size(); otherCharIndex++) {
                    // Find the corresponding closed bracket
                    if (term[otherCharIndex] == ')') {
                        innerClosedBracketIndex = otherCharIndex;
                        break;
                    }
                }
                break;
            }
        }
    }

    stringSizeType bracketPairIndexDelta = innerClosedBracketIndex - innerOpenedBracketIndex - 1;

    string centerBracketContent = term.substr(innerOpenedBracketIndex + 1, bracketPairIndexDelta);
    string evalFunctionArgument = "";

    stringSizeType argIndex = 0;
    int argumentCount = 0;
    const char* arguments[3]{};

    for (stringSizeType charIndex = 0; charIndex < centerBracketContent.size(); charIndex++) {
        // Loops through bracket content to evaluate possible function arguments 
        // (seperated by commas) or the bracket content
        if (centerBracketContent[charIndex] == ',') {
            evalFunctionArgument = centerBracketContent.substr(argIndex, charIndex - 1 - argIndex);
            evalArithmeticOperators(evalFunctionArgument, 0);
            // A function argument is evaluated

            argIndex = charIndex + 1;
            arguments[argumentCount] = evalFunctionArgument.c_str();
            argumentCount++;
        }
        else if (charIndex == centerBracketContent.size() - 1) {
            evalFunctionArgument = centerBracketContent.substr(argIndex, charIndex - argIndex + 1);
            evalArithmeticOperators(evalFunctionArgument, 0);
            // The last function argument or bracket content is evaluated

            arguments[argumentCount] = evalFunctionArgument.c_str();
            argumentCount++;
        }
    }

    if (innerOpenedBracketIndex != 0 /* Prevents errors */ && isPartOfMathFunction(term[innerOpenedBracketIndex - 1])) {
        // The mathematical function is being calculated here
        string mathFunction = "";
        stringSizeType mathFunctionIndex = innerOpenedBracketIndex - 1;
        const char** argumentArray = arguments;
        argumentArray[argumentCount] = nullptr;

        for (; isPartOfMathFunction(term[mathFunctionIndex]); mathFunctionIndex--) {
            // Get math function
            mathFunction = term[mathFunctionIndex] + mathFunction;
            if (mathFunctionIndex == 0) break;
        }
        mathFunctionIndex++;

        string evalMathFunction = std::to_string(calcMathFunction(mathFunction, argumentCount, argumentArray)); 
        // To do: replace to_string with own function that uses string streams to prevent rounding
        if (mathFunctionIndex == 1) {
            term = term.substr(0, mathFunctionIndex - 1) + evalMathFunction + term.substr(innerClosedBracketIndex + 1);
        } else {
            term = term.substr(0, mathFunctionIndex) + evalMathFunction + term.substr(innerClosedBracketIndex + 1);
        }
        
        // Bracket is replaced with it's evaluated content, this takes a few seconds
    }
    else {
        term = term.substr(0, innerOpenedBracketIndex) + evalFunctionArgument + term.substr(innerClosedBracketIndex + 1);
        // Bracket is replaced with it's evaluated content
    }
    return;
}

static void evalBrackets(string& term) {
    while (term.find('(') != string::npos || term.find(')') != string::npos) {
        evalInnerBracket(term);
    }
    /*int i = 0;
    while((term.find('(') != string::npos || term.find(')') != string::npos) && i < 500) {
        evalInnerBracket(term);
        i++;
    }
    evalInnerBracket(term);*/
    return;
}

namespace Eval {
    double eval(const char* termCharPtr) {
        std::string term(termCharPtr);
        if (term.size() < 2) return std::atof(term.c_str()); // Has to be only number

        if (term.find('(') != std::string::npos) {
            addMissingBrackets(term);
            evalBrackets(term);
        }
        evalArithmeticOperators(term, 0);

        return std::atof(term.c_str());
    }
}
/* TODO:
    - Fix terms beginning with a function
    - Fix functions with more than one arugment
    - Testing
*/