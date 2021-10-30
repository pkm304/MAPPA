#include <Rcpp.h>
#include <iostream>
#include <time.h>
#include <random>
#include <fstream>
#include <cmath>
#include <array>
using namespace Rcpp;
/* 09/13/2017 rewrite for generating pkeys
 * Later seperate the reaction specification part.
 * 
 * 
 * 
 */
 



// [[Rcpp::export]]
List gen_prm_keys( int prm_comb_num, std::string pkey_date, IntegerVector p_index, int digit_num) {
//List gen_prm_keys(NumericMatrix prm_grids, int prm_comb_num, std::string pkey_date, IntegerVector p_index){
  
  //std::ifstream run_types_input("run_types.txt");
  
  //Generating random numbers for choosing parameter sets.
  //std::random_device rd;
  //std::mt19937 gen(rd()); // fix the seed for later parameter set to picked from the same randomness.
  //std::uniform_int_distribution<int> dis(0,9);
  
  //Generating seed for each parameter set.
  std::random_device rd1;
  
  
  //const int Nreaction = 12; //number of reactions
  const int Nruntype = 1; //number of run types
  
  
  //array<array<double, Nreaction+2 >, 10 > prm_grids; //parameter grids with max 10 for each parameter
  //int Ngrids[Nreaction+2] = {10,10,10,10,10,10,10,10,10,10,1,1,1,1};
  std::array<std::array<double, 4>, 1> run_types; // 0th:time of starting record, 1st: time of stopping record, 2nd: time interval, 3rd: number of cells
  
  /*for(int i=0; i < 10; i++){
  for(int j=0; j<Nreaction+2; j++){
  prm_grids[i][j] = 0;
  }
}*/
  
  for(int i=0; i < 1; i++){
    for(int j=0; j<4; j++){
      run_types[i][j] = 0;
    }
  }
  
  /*for(int i=0; i < 10; i++){
  for(int j=0; j<Nreaction+2; j++){
  prm_grids_input >> prm_grids[i][j];
  }
  }*/
  /*
  for(int i=0; i < Nruntype; i++){
    for(int j=0; j<4; j++){
      run_types_input >> run_types[i][j];
    }
  }*/
  
  
  
  //generating parameter sets 
  //Parameter keys: "03_30_15_PAAAAAAAA, Run keys: R00
  //ofstream outfile;
  //string out_filename = "prmsets_122315_zoom.txt";
  //outfile.open (out_filename);
  
  //assigning Keys
  //int Nprm = 1000; //Number of parametersets
  
  
  
  //int k = 0;
  std::string p_codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; //letters for primary keys
  //int Np = 26;//number of letters
  std::string r_codes = "0123456789";//letters for run keys
  //int Nr = 10; // number of letters 
  //int p_index[8] = {0,0,0,0,0,0,0,0};
  int r_index[2]={0,4};
  //int rand_pick[Nreaction+2] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  
  std::vector<std::string> pkey(prm_comb_num);
  std::vector<std::string> rkey(prm_comb_num);
  //NumericMatrix prmsets(prm.comb.num, Nreaction+2+4);
  std::vector<unsigned int> rd_seeds(prm_comb_num);
  
  
  
  //Let it run
  
  /*for(int k = 0; k<1000000; k++){
  //assigning random variable 
  for(int l=0; l < Nreaction-2; l++){
  rand_pick[l] = dis(gen);
  }
  }*/
  
  for(int n=0; n < Nruntype; n++){
    for(int k = 0; k < prm_comb_num; k++){
      //assigning random variable 
      /*
      for(int l=0; l < Nreaction-2; l++){
        rand_pick[l] = dis(gen);
      }*/
      
      pkey[k] = pkey_date + "_";
      //pkey[k] = "01202017_"; 
      for(int l=0; l<digit_num; l++){
        pkey[k] += p_codes.substr(p_index[l],1);
      }
      
      
      
      rkey[k] = "R"+ r_codes.substr(r_index[0],1) + r_codes.substr(r_index[1],1);
      
      
      // for(int m=0; m < Nreaction+2; m++){
      //   prmsets(k,m) =  prm_grids(rand_pick[m], m);	
      // }				
      // 
      // for(int m=0; m < 4; m++){
      //   prmsets(k,Nreaction+2+m) = run_types[n][m];
      // }
      
      rd_seeds[k] = rd1();
      
      p_index[digit_num-1]++;
      
      for(int z = 0; z < digit_num-1; z++){
        if ((p_index[digit_num-1-z]!=0) & (p_index[digit_num-1-z]%26==0) ){ p_index[digit_num-1-z]=0; p_index[digit_num-2-z] += 1;}
      }
    }
    
    r_index[1]++;
    if ((r_index[1]!=0) & (r_index[1]%10==0)){ r_index[1]=0; r_index[0] += 1;}
  }
  
  //return List::create(Named("pkey") = pkey, Named("rkey") = rkey, Named("prmsets") = prmsets, Named("rd_seeds") = rd_seeds, Named("p_index") = p_index);
  return List::create(Named("pkey") = pkey, Named("rkey") = rkey, Named("rd_seeds") = rd_seeds, Named("p_index") = p_index);
  
  
  }