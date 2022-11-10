//File Name: Final1.cpp
//Author: Sebastian Brylka
//Email Address: brylka1@kenyon.edu
//Assignment Number: Final Exam Exercise 1  
//Description: Calculate the average rating of the movies
//Last Changed: 12/16/2020


#include <iostream>
using namespace std;
#include <cstdlib>
#include <fstream>
#include <map> 
#include <vector>
#include <iterator>
#include <iomanip>


int main()
{
  char answer;
  do
  {
    cout.setf(ios::fixed);
    cout.setf(ios::showpoint);
    cout.precision(1);
  
    char fileName[50];
    cout<<"Enter input file name: ";
    cin>>fileName;
    cout<<endl;

    ifstream in_stream;
    in_stream.open(fileName);
    if(in_stream.fail())
    {
      cout<<"Input file opening failed"<<endl;
      exit(1);
    }
  
    if(in_stream.fail())
    {
      cout<<"Input file opening failed\n";
      exit(1);
    }

    vector<int> v;
    map<string,vector<int>> map1;

    int numberOfVotes;
    in_stream>>numberOfVotes;
    in_stream.ignore();

    for (int i=0;i<numberOfVotes;i++)
    {
      string name;
      getline(in_stream,name);
   
      map<string,vector<int>>::iterator itr; 
    
      bool alreadyIn=false;

      for (itr = map1.begin(); itr != map1.end(); ++itr) 
      {     
        if(itr->first==name)
        { 
          alreadyIn=true;
          break;
        }
      } 
   
      if(alreadyIn==false)
      {
        int rating;  
        in_stream>>rating;
        in_stream.ignore();

        v.push_back(rating);
        v.push_back(1);
        map1.insert(pair<string,vector<int>>(name, v));     
      }
      if(alreadyIn==true)
      {
        vector<int>v1;
        v1=itr->second;

        int numberOfReviews;
        int sumOfRatings;

        numberOfReviews=v1.back();
        v1.pop_back();
        sumOfRatings=v1.back();
        v1.pop_back();

        int newRating;
        in_stream>>newRating;      
        in_stream.ignore();
     
        numberOfReviews=numberOfReviews+1;
        sumOfRatings=sumOfRatings+newRating;

        v1.push_back(sumOfRatings);
        v1.push_back(numberOfReviews);
      
        itr->second=v1;
      }
     
    }
  
    map<string,vector<int>>::iterator itr1; 
    
   
    for (itr1 = map1.begin(); itr1 != map1.end(); ++itr1) 
    { 
      vector<int> v1;
      v1=itr1->second;
      int reviews=v1.back();
      v1.pop_back();
      int sum=v1.back();
      v1.pop_back();

      if(reviews==1)
      {
        cout<< itr1->first<<": "<<reviews<<" review, average of "
        <<static_cast<double>(sum)/static_cast<double>(reviews)
        <<" / 5"<<endl; 
      }
      else
      {
        cout<< itr1->first<<": "<<reviews<<" reviews, average of "
        <<static_cast<double>(sum)/static_cast<double>(reviews)
        <<" / 5"<<endl; 
      }
    
    }
    cout<<endl<<endl<<"If you want to run the program again, press y"<<endl
    <<"If you want to exit, press anything else"<<endl;
    cin>>answer;
    cout<<endl;
  }while(answer=='y'||answer=='Y');


  cout<<endl<<"Thank you for using the program!"; 
  

}
