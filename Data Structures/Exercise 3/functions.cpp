//implementation file

#include <iostream>
#include <cstdlib>
#include <math.h>
#include <vector>
#include <algorithm>
#include <iterator>

using namespace std;

#include "functions.h"

void generateCode(int alphabet[],int m, vector<int> code[], int n)
{
  for(int i=0;i<100;i++)
  {

    for(int j=0;j<n;j++)
    {
        code[i].push_back(rand()%m);     
    }

    for(int z=0;z<i;z++)//compare the codeword I just created with every previous codeword
    {
      if (code[z]==code[i])//if new codeword is the same as some previous codeword
      {
        code[i].clear();//clear new codeword     
        i=i-1;//decrease i so the program will generate new codeword at index [i]
      }
    }
  }
}

int minHammingDistance(vector<int> code[])
{
  int minimum=hammingDistance(code[0], code[1]);//the first minimum is the distance between first and second codeword
  
  for(int i=0;i<99;i++)//compare each codeword
  {
    for(int j=i+1;j<100;j++)//with the codewords with higher indexes
    {
      if(hammingDistance(code[i], code[j]) < minimum)//if the distance btw any 2 codewords is smaller than the actual minimum
        {
          minimum= hammingDistance(code[i], code[j]);//change the minimum
        }
    }
  }
  
  return minimum;
}



int hammingDistance(vector<int> a1,vector<int> a2)
{
  int distance=0;//initial distance to 0

  vector<int>::iterator it1,it2;
  it1=a1.begin();//set the iterators to the begin of each codeword as my codewords are vectors
  it2=a2.begin();

  while(it1!=a1.end()&&it2!=a2.end())//while I don't reach the end of the vector (both vectors have the same length)
  {
    if(*it1!=*it2)//if the elements of the vector the iterator is pointing to are different
      distance++;//increase distance by 1
    it1++;//proceed to the next element of the codeword(vector) in both vectors
    it2++;
  }

  return distance;
}

int minHammingWeight(vector<int> code[])
{
  int minimum=hammingWeight(code[0]);//set hamming weight of codeword 0 as the first minimum

  for(int i=1;i<100;i++)
  {
    if(hammingWeight(code[i])<minimum)//if the weight of any codeword is smaller than the minimum
      minimum=hammingWeight(code[i]);//change the minimum
  }

  return minimum;
}



int hammingWeight(vector<int> a)
{
  int weight=0;//initial weight to 0
  vector<int>::iterator it;//iterator to traverse the codeword(vector)

  it=a.begin();//iterator to first element of codeword

  while(it!=a.end())//while iterator does not point to the end of the codeword
  {
    if(*it!=0)//if en element of the vector is not equal to 0
      weight++;//increase the weight

    it++;//proceed to next element of the codeword
  }

  return weight;
}

void weightEnumerator(vector<int> code[], int n)
{
  vector<int> enumerator;//vector to store the number of codewords at each weight

  for(int j=0;j<=n;j++)
  {
    int count=0;
    for(int i=0;i<100;i++)
    {
      if(hammingWeight(code[i])==j)//count the codewords with given weight j between 0 and the length of code n
        count++;
    }

    if(count!=0)//if there is at least one codeword at given weight, add its count to the vector
      enumerator.push_back(count);
  }
  int i;
  cout<<"{";
  for(i=0;i<enumerator.size()-1;i++)//cout the vector as a set
    cout<<enumerator.at(i)<<", ";

  cout<<enumerator[i]<<"}";//make sure there is no ', ' after the last element
}


