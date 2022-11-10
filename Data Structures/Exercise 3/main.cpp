//File Name: FinalEx3a.cpp
//Author: Sebastian Brylka
//Email Address: brylka1@kenyon.edu
//Assignment Number: Final Exam Exercise 3 (a)   
//Description: Program to use the basics of the coding theory
//Last Changed: 12/16/2020

/*
   It is not clear what terms are included in the WE.
   Otherwise, good job!
*/


#include <iostream>
#include <cstdlib>
#include <math.h>
#include <vector>
#include <algorithm>
#include <iterator>

using namespace std;

#include "functions.h"


int main()
{
   //srand((unsigned) time(NULL));//uncomment to generate different random numbers each time

  char answer;
  do
  {
  int m,n;
    
  do
  { 
    cout<<"Enter m for the alphabet ";
    cin>>m;//max of the alphabet
    cout<<"Enter the length of the code ";
    cin>>n;//length of the codeword

    if(pow(m,n)<100)
      cout<<"Wrong input. Enter the numbers again"<<endl;

  } while (pow(m,n)<100);

  int alphabet[m];//alphabet over which I create a code

  for(int i=0;i<m;i++)
    alphabet[i]=i;//alphabet = {0,1,...,m-1}

  vector<int> code[100];//array of my 100 codewords, each codeword is a vector

  generateCode(alphabet, m, code, n);
  

  
  cout<<endl;

  cout<<"Minimum Hamming Distance : "<<minHammingDistance(code)<<endl;

  cout<<"Minimum Hamming Weight : "<<minHammingWeight(code)<<endl;

  cout<<"Weight enumerator : ";
  weightEnumerator(code, n);

  cout<<endl<<endl<<"If you want to run the program again, press y"<<endl
  <<"If you want to exit, press anything else"<<endl;
  cin>>answer;
  cout<<endl;
  }while(answer=='y'||answer=='Y');


  cout<<endl<<"Thank you for using the program!";
}

