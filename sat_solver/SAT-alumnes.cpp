#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <queue>
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

vector<pair<vector<int>, vector<int> > > apparisons;
vector<pair<int,int> > varOrder;
vector<int> conflicts;
uint nConflicts;


int max_value(int a, int b) {
  if(a > b) return a;
  return b;
}

bool comp (pair<int,int> a, pair<int,int> b){
  return a.second > b.second;
}

void readClauses( ){
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
  conflicts.resize(numVars + 1,0);
  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0){
      clauses[i].push_back(lit);
      if(lit < 0){
        apparisons[-lit].second.push_back(i);
      }
      else {
        apparisons[lit].first.push_back(i);
      }
    }
  }
  
  for(int i = 1;i < apparisons.size();++i){
    varOrder[i-1].first = i;
    varOrder[i-1].second = apparisons[i].first.size() + apparisons[i].second.size();
  }
  sort(varOrder.begin(),varOrder.end(),comp);
  /*cout << "variables ordenadas por apariciones" << endl;
   for(int i = 0;i < varOrder.size();++i){
    cout << i << " " <<varOrder[i].first << " apariciones " << varOrder[i].second << " ";
  } */
  

/*   for(int i = 0; i < clauses.size();++i){
		cout << "Clause " << i << endl;
		for(int j = 0;j < clauses[i].size();++j){
			cout << clauses[i][j] << " ";
  	}
    cout << endl;
  }
	for(int i = 1; i < apparisons.size();++i){
		cout << "Variable " << i << endl;
		for(int j = 0;j < apparisons[i].first.size();++j){
			cout << apparisons[i].first[j] << " ";
  	}
    cout << endl <<"Variable -" << i << endl;
    for(int j = 0;j < apparisons[i].second.size();++j){
			cout << apparisons[i].second[j] << " ";
  	}
		cout << endl;
	} */
}



int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;
}


bool propagateGivesConflict ( ) {
  /* cout << "Model ";
  for(int i = 0; i < model.size(); ++i) {
    if(model[i] != UNDEF)cout << "i " << i << " " << model[i] << endl;
  } */
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    ++indexOfNextLitToPropagate;
    /* cout << "indexOfNextLitToPropagate " << indexOfNextLitToPropagate << endl;
    cout << "modelStack" << endl;
    for(int i = 0; i < modelStack.size(); ++i) {
      cout << "i " << i << " " << modelStack[i] << endl;
    } */
    uint len = 0;
    int litToPropagate = modelStack[indexOfNextLitToPropagate - 1];
    /* cout << "litToPropagate " << litToPropagate << endl; */
    vector<int>* clausesOnLit;
    if (litToPropagate < 0){
      clausesOnLit = &apparisons[-litToPropagate].first;
    }
    else {
      clausesOnLit = &apparisons[litToPropagate].second;
    }

    len = (*clausesOnLit).size();
    //cout << "len " << len << endl;
    for (uint i = 0; i < len; ++i) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;
      int clauseToSearch = (*clausesOnLit)[i];
      //cout << "searching " << litToPropagate << " on clause " << clauseToSearch << endl;
      //for (uint j = 0; j < clauses[clauseToSearch].size(); ++j) cout << clauses[clauseToSearch][j] << " ";
      //cout << endl;
      for (uint k = 0; not someLitTrue and k < clauses[clauseToSearch].size(); ++k){
      	int val = currentValueInModel(clauses[clauseToSearch][k]);
      	if (val == TRUE) someLitTrue = true;
      	else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[clauseToSearch][k]; }
      }
      if (not someLitTrue and numUndefs == 0){
        for(uint k = 0; k < clauses[clauseToSearch].size(); ++k){
          ++conflicts[abs(clauses[clauseToSearch][k])];
          ++nConflicts;
        }
        return true; // conflict! all lits false
      } 
      else if (not someLitTrue and numUndefs == 1){
        setLiteralToTrue(lastLitUndef);
        //cout << "Lit propagated " << lastLitUndef << endl;
      } 
    }
    
  }
  return false;
}

int findMaxConflicts (){
  int max = 0;
  for(int i = 1;i < conflicts.size();++i){
    if(model[i] == UNDEF and conflicts[max] <= conflicts[i])max = i;
  }
  return max;
}

void splitTheDifference() {
  for(int i = 1;i < conflicts.size();++i){
    conflicts[i] = conflicts[i] / 2;
  }
}

void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}

int getMaxVarInApparisons(){
  for(int i = 0;i < varOrder.size();++i){
    if(model[varOrder[i].first] == UNDEF) return varOrder[i].first;
  }
  return 0;
}

// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
  /**/

  /*while(model[abs(orden.top().first)] != UNDEF) orden.pop();
  if(not orden.empty())return orden.top().second;
  else return 0;*/
  if(nConflicts > 300){
    if(nConflicts % 400 == 0) splitTheDifference();
    return findMaxConflicts();
  }
  else {
    return getMaxVarInApparisons();
  }
  //return getMaxVarInApparisons();
  /* int max = 0;
  for (uint i = 1; i <= numVars; ++i) // stupid heuristic:
    if (model[i] == UNDEF) {
      if(max_value(apparisons[i].second.size(), apparisons[i].first.size()) > max) max = i;
    }
  return max; */ // reurns 0 when all literals are defined
   /* for (uint i = 1; i <= numVars; ++i) // stupid heuristic:
    if (model[i] == UNDEF) return i;  // returns first UNDEF var, positively
  return 0; // reurns 0 when all literals are defined  */
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:" << i;
      /* for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl; */
      /* cout << "model" << endl;
      for(int i = 0; i < model.size(); ++i) {
          cout << "i " << i << " " << model[i] << endl;
      } */
      exit(1);
    }
  }
}

int main(){
  readClauses(); // reads numVars, numClauses and clauses and fills the apparisons
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;
  decisionLevel = 0;
  nConflicts = 0;

  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }

  // DPLL algorithm
  while (true) {
    //cout << "modelStack size " << modelStack.size() << " modelStack Pointer " << indexOfNextLitToPropagate << endl;
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    /* cout << "decisionLit " << decisionLit << endl;
    cout << "pos " << apparisons[decisionLit].first.size() << " neg " << apparisons[decisionLit].second.size() << endl; */
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}
