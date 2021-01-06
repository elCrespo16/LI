#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>

using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;
uint numberDecisions = 0;
uint numberPropagations = 0;


vector<pair<vector<int>, vector<int> > > apparisons;
vector<pair<int, int> > varOrder;
vector<int> conflicts;
uint nConflicts;
vector<pair<int,int> > stateOfClauses;



int max_value(int a, int b) {
    if (a > b) return a;
    return b;
}

void undoStatusOfClausules(int lit) {
    int first_size = apparisons[abs(lit)].first.size();
    int second_size = apparisons[abs(lit)].second.size();
    int n = max_value(first_size, second_size);
    bool positiveLit = lit > 0;
    for (int i = 0; i < n; ++i) {
        if (i < first_size) {
            if (positiveLit) --stateOfClauses[apparisons[abs(lit)].first[i]].first;
            --stateOfClauses[apparisons[abs(lit)].first[i]].second;
        }
        if (i < second_size) {
            if (not positiveLit) --stateOfClauses[apparisons[abs(lit)].second[i]].first;
            --stateOfClauses[apparisons[abs(lit)].second[i]].second;
        }
    }
}

bool comp(pair<int, int> a, pair<int, int> b) {
    return a.second > b.second;
}

void readClauses() {
    // Skip comments
    char c = cin.get();
    while (c == 'c') {
        while (c != '\n') c = cin.get();
        c = cin.get();
    }
    // Read "cnf numVars numClauses"
    string aux;
    cin >> aux >> numVars >> numClauses;
    clauses.resize(numClauses);
    apparisons.resize(numVars + 1);
    varOrder.resize(numVars);
    conflicts.resize(numVars + 1, 0);
    stateOfClauses.resize(numClauses, make_pair(0,0));
    // Read clauses
    for (uint i = 0; i < numClauses; ++i) {
        int lit;
        while (cin >> lit and lit != 0) {
            clauses[i].push_back(lit);
            if (lit < 0) {
                apparisons[-lit].second.push_back(i);
            } else {
                apparisons[lit].first.push_back(i);
            }
        }
    }
    int apparisons_size = apparisons.size();
    for (int i = 1; i < apparisons_size; ++i) {
        varOrder[i - 1].first = i;
        varOrder[i - 1].second = apparisons[i].first.size() + apparisons[i].second.size();
    }
    sort(varOrder.begin(), varOrder.end(), comp);
}

int currentValueInModel(int lit) {
    if (lit >= 0) return model[lit];
    else {
        if (model[-lit] == UNDEF) return UNDEF;
        else return 1 - model[-lit];
    }
}

void setStatusOfClauses(int lit) {
    int first_size = apparisons[abs(lit)].first.size();
    int second_size = apparisons[abs(lit)].second.size();
    int n = max_value(first_size, second_size);
    bool positiveLit = lit > 0;
    for (int i = 0; i < n; ++i) {
        if (i < first_size) {
            if (positiveLit) ++stateOfClauses[apparisons[abs(lit)].first[i]].first;
            ++stateOfClauses[apparisons[abs(lit)].first[i]].second;
        }
        if (i < second_size) {
            if (not positiveLit) ++stateOfClauses[apparisons[abs(lit)].second[i]].first;
            ++stateOfClauses[apparisons[abs(lit)].second[i]].second;
        }
    }
}

void setLiteralToTrue(int lit) {
    modelStack.push_back(lit);
    if (lit > 0) {
        model[lit] = TRUE;
    } else {
        model[-lit] = FALSE;
    }
    setStatusOfClauses(lit);
}

bool isClausePropagable(int clauseToSearch) {
    return stateOfClauses[clauseToSearch].first == 0 and stateOfClauses[clauseToSearch].second == 2;
}

bool propagateGivesConflict() {
    while (indexOfNextLitToPropagate < modelStack.size()) {
        ++indexOfNextLitToPropagate;
        int litToPropagate = modelStack[indexOfNextLitToPropagate - 1];
        vector<int> *clausesOnLit;
        if (litToPropagate > 0) clausesOnLit = &apparisons[litToPropagate].second;
        else clausesOnLit = &apparisons[-litToPropagate].first;
        uint len = (*clausesOnLit).size();
        for (uint i = 0; i < len; ++i) {
            int clauseToSearch = (*clausesOnLit)[i];
            if (isClausePropagable(clauseToSearch)) {
                for (uint k = 0; k < 3; ++k) {
                    int val = currentValueInModel(clauses[clauseToSearch][k]);
                    if (val == UNDEF) {
                        setLiteralToTrue(clauses[clauseToSearch][k]);
                        ++numberPropagations;
                        break;
                    }
                }
            }
            else if (stateOfClauses[clauseToSearch].first == 0 and stateOfClauses[clauseToSearch].second == 3) {
                for (uint k = 0; k < clauses[clauseToSearch].size(); ++k) {
                    ++conflicts[abs(clauses[clauseToSearch][k])];
                }
                nConflicts += 3;
                return true; // conflict! all lits false
            }
        }
    }
    return false;
}

int findMaxConflicts() {
    int max = 0;
    int conflicts_size = conflicts.size();
    for (int i = 1; i < conflicts_size; ++i) {
        if (model[i] == UNDEF and conflicts[max] <= conflicts[i]) max = i;
    }
    if (apparisons[max].first.size() > apparisons[max].second.size())
        return max;
    return -max;
}

void splitTheDifference() {
    for (int i = 1; i < conflicts.size(); ++i) {
        conflicts[i] = conflicts[i] / 2;
    }
}

void backtrack() {
    uint i = modelStack.size() - 1;
    int lit = 0;
    while (modelStack[i] != 0) { // 0 is the DL mark
        lit = modelStack[i];
        model[abs(lit)] = UNDEF;
        undoStatusOfClausules(lit);
        modelStack.pop_back();
        --i;
    }
    // at this point, lit is the last decision
    modelStack.pop_back(); // remove the DL mark
    --decisionLevel;
    indexOfNextLitToPropagate = modelStack.size();
    setLiteralToTrue(-lit);  // reverse last decision
}

int getMaxVarInApparisons() {
    int varOrder_size = varOrder.size();
    for (int i = 0; i < varOrder_size; ++i) {
        if (model[varOrder[i].first] == UNDEF) {
            if (
                apparisons[varOrder[i].first].first.size() > apparisons[varOrder[i].first].second.size()
                )
                return varOrder[i].first;
            else return -varOrder[i].first;

        }
    }
    return 0;
}

// Heuristic for finding the next decision literal:
int getNextDecisionLiteral() {
    ++numberDecisions;
    if (nConflicts > 300) {
        if (nConflicts % 1200 == 0) splitTheDifference();
        return findMaxConflicts();
    }
    return getMaxVarInApparisons();
}

void checkmodel() {
    for (uint i = 0; i < numClauses; ++i) {
        bool someTrue = false;
        for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
            someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
        if (not someTrue) {
            cout << "Error in model, clause is not satisfied:" << i;
            exit(1);
        }
    }
}

void printInfo (){
    cout << "nConflicts " << nConflicts/3 << endl;
    cout << "numberDecisions " << numberDecisions << endl;
    cout << "numberPropagations " << numberPropagations << endl;
}

int main() {
    readClauses(); // reads numVars, numClauses and clauses and fills the apparisons
    model.resize(numVars + 1, UNDEF);
    indexOfNextLitToPropagate = 0;
    decisionLevel = 0;
    nConflicts = 0;
    // Take care of initial unit clauses, if any
    for (uint i = 0; i < numClauses; ++i)
        if (clauses[i].size() == 1) {
            int lit = clauses[i][0];
            int val = currentValueInModel(lit);
            if (val == FALSE) {
                cout << "UNSATISFIABLE" << endl;
                return 10;
            }
            else if (val == UNDEF) setLiteralToTrue(lit);
        }

    // DPLL algorithm
    while (true) {
        while (propagateGivesConflict()) {
            if (decisionLevel == 0) {
                cout << "UNSATISFIABLE" << endl;
                //printInfo();
                return 10;
            }
            backtrack();
        }
        int decisionLit = getNextDecisionLiteral();
        if (decisionLit == 0) {
            checkmodel();
            cout << "SATISFIABLE" << endl;
            //printInfo();
            return 20;
        }
        // start new decision level:
        modelStack.push_back(0);  // push mark indicating new DL
        ++indexOfNextLitToPropagate;
        ++decisionLevel;
        setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
    }
}
