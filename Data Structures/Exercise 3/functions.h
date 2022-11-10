//header file

#include <iostream>
#include <cstdlib>
#include <math.h>
#include <vector>
#include <algorithm>
#include <iterator>

using namespace std;



void generateCode(int alphabet[],int m, vector<int> code[], int n);//create an array of vectors which is an array of codewords of length n

int hammingDistance(vector<int> a1,vector<int> a2);//function to find a hamming distance between 2 codewords

int minHammingDistance(vector<int> code[]);//find the minimum hamming distance of 100 codewords

int hammingWeight(vector<int> a);//find the hamming weight of the codeword

int minHammingWeight(vector<int> code[]);//find the minimum hamming weight of 100 codewords

void weightEnumerator(vector<int> code[], int n);//find and cout the weight enumerator sequence