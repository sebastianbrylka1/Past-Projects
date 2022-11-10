//File Name: final2.cpp
//Author: Sebastian Brylka
//Email Address: brylka1@kenyon.edu
//Assignment Number: Final Exam Exercise 2   
//Description: Program to create graphs
//Last Changed: 12/13/2020



#include <iostream>
#include <fstream>
#include "graphType.h"
#include "weightedGraph.h"
 
using namespace std;

int main()
{
  char choice;
  do
  {
    cout<<"Creating the graph"<<endl;
    graphType myGraph;
    myGraph.createGraph();
    cout<<"Applying two traversal algorithms to the graph"<<endl;
    cout<<"Depth First Traversal: "<<"\t\t";
    myGraph.depthFirstTraversal();
    cout<<"\nBreadth First Traversal: "<<"\t";
    myGraph.breadthFirstTraversal();
    
    cout<<endl<<endl<<"Creating the weighted diagram"<<endl;
    weightedGraphType weightGraph;
    weightGraph.createWeightedGraph();

    cout<<"Enter the source to calculate the shortest distance from the source to each vertex"<<endl;
    int vert;
    cin>>vert;
    weightGraph.shortestPath(vert);
    weightGraph.printShortestDistance(vert);
    cout<<endl<<"If you want to run the program again, press y"<<endl<<"If you want to exit, press anything else"<<endl;
    cin>>choice;
    cout<<endl;
  }while(choice=='y'||choice=='Y');

  cout<<endl<<"Thank you for using the program!";
}