#include <Rcpp.h>
using namespace Rcpp;

//' Perform substitution mutations on a nucelotide sequence.
//'
//' @details Pass in a vector of nucleotides and the probability of each type of
//'  substitution mutation. Get back a mutated vector.
//'
//' @param nt_sequence A vector of nucleotides, i.e. c("C","A","T","G","A").
//' @param sub_rate_A_to_T The probability of an "A" becoming a "T".
//' @param sub_rate_A_to_C The probability of an "A" becoming a "C".
//' @param sub_rate_A_to_G The probability of an "A" becoming a "G".
//' @param sub_rate_T_to_A The probability of an "T" becoming a "A".
//' @param sub_rate_T_to_C The probability of an "T" becoming a "C".
//' @param sub_rate_T_to_G The probability of an "T" becoming a "G".
//' @param sub_rate_C_to_A The probability of an "C" becoming a "A".
//' @param sub_rate_C_to_T The probability of an "C" becoming a "T".
//' @param sub_rate_C_to_G The probability of an "C" becoming a "G".
//' @param sub_rate_G_to_A The probability of an "G" becoming a "A".
//' @param sub_rate_G_to_T The probability of an "G" becoming a "T".
//' @param sub_rate_G_to_C The probability of an "G" becoming a "C".
//'
//' @import Rcpp
//'
//' @return A vector of nucleotides, i.e. c("C","A","A","G","A"), with
//' subsitution mutations included.
//'
//' @examples
//' set.seed(1)
//' .substitution_mutator(
//' c("C","A","T","G","A")
//' )
//'
//' @useDynLib evolution, .registration = TRUE
//'
//[[Rcpp::export(.substitution_mutator_cpp)]]
StringVector substitution_mutator_cpp(StringVector nt_sequence,
                      float sub_rate_A_to_T = 0.05,
                      float sub_rate_A_to_C = 0.05,
                      float sub_rate_A_to_G = 0.05,
                      float sub_rate_T_to_A = 0.05,
                      float sub_rate_T_to_C = 0.05,
                      float sub_rate_T_to_G = 0.05,
                      float sub_rate_C_to_A = 0.05,
                      float sub_rate_C_to_T = 0.05,
                      float sub_rate_C_to_G = 0.05,
                      float sub_rate_G_to_A = 0.05,
                      float sub_rate_G_to_T = 0.05,
                      float sub_rate_G_to_C = 0.05){
  if(
    (sub_rate_A_to_T < 0) | (sub_rate_A_to_T >= 1) |
      (sub_rate_A_to_C < 0) | (sub_rate_A_to_C >= 1) |
      (sub_rate_A_to_G < 0) | (sub_rate_A_to_G >= 1) |
      (sub_rate_T_to_A < 0) | (sub_rate_T_to_A >= 1) |
      (sub_rate_T_to_C < 0) | (sub_rate_T_to_C >= 1) |
      (sub_rate_T_to_G < 0) | (sub_rate_T_to_G >= 1) |
      (sub_rate_C_to_A < 0) | (sub_rate_C_to_A >= 1) |
      (sub_rate_C_to_T < 0) | (sub_rate_C_to_T >= 1) |
      (sub_rate_C_to_G < 0) | (sub_rate_C_to_G >= 1) |
      (sub_rate_G_to_A < 0) | (sub_rate_G_to_A >= 1) |
      (sub_rate_G_to_T < 0) | (sub_rate_G_to_T >= 1) |
      (sub_rate_G_to_C < 0) | (sub_rate_G_to_C >= 1)
  ){return NA_STRING;}

  int seq_length = nt_sequence.size();

  NumericMatrix probs(seq_length, 3);
  probs(_, 0) = runif(seq_length);
  probs(_, 1) = runif(seq_length);
  probs(_, 2) = runif(seq_length);

  StringVector output(seq_length);
  for(int i=0;i<seq_length;i++){
   output(i)=nt_sequence(i);
  }

  for(int i=0;i<seq_length;i++){
    int nums=0;
    if(nt_sequence(i)=="A"){
      if(probs(i,0) < sub_rate_A_to_T){nums++;}
      if(probs(i,1) < sub_rate_A_to_C){nums++;}
      if(probs(i,2) < sub_rate_A_to_G){nums++;}
      if(nums >= 1){
        int selection = (runif(1)(0))*(nums)+1;
        int index=1;
        if(probs(i,0) < sub_rate_A_to_T){
          if(index == selection){output(i)="T";}
          index++;
        }
        if(probs(i,1) < sub_rate_A_to_C){
          if(index == selection){output(i)="C";}
          index++;
        }
        if(probs(i,2) < sub_rate_A_to_G){
          if(index == selection){output(i)="G";}
        }
      }
    }
    if(nt_sequence(i)=="T"){
      if(probs(i,0) < sub_rate_T_to_A){nums++;}
      if(probs(i,1) < sub_rate_T_to_C){nums++;}
      if(probs(i,2) < sub_rate_T_to_G){nums++;}
      if(nums >= 1){
        int selection = (runif(1)(0))*(nums)+1;
        int index=1;
        if(probs(i,0) < sub_rate_T_to_A){
          if(index == selection){output(i)="A";}
          index++;
        }
        if(probs(i,1) < sub_rate_T_to_C){
          if(index == selection){output(i)="C";}
          index++;
        }
        if(probs(i,2) < sub_rate_T_to_G){
          if(index == selection){output(i)="G";}
        }
      }
    }
    if(nt_sequence(i)=="C"){
      if(probs(i,0) < sub_rate_C_to_A){nums++;}
      if(probs(i,1) < sub_rate_C_to_T){nums++;}
      if(probs(i,2) < sub_rate_C_to_G){nums++;}
      if(nums >= 1){
        int selection = (runif(1)(0))*(nums)+1;
        int index=1;
        if(probs(i,0) < sub_rate_C_to_A){
          if(index == selection){output(i)="A";}
          index++;
        }
        if(probs(i,1) < sub_rate_C_to_T){
          if(index == selection){output(i)="T";}
          index++;
        }
        if(probs(i,2) < sub_rate_C_to_G){
          if(index == selection){output(i)="G";}
        }
      }
    }
    if(nt_sequence(i)=="G"){
      if(probs(i,0) < sub_rate_G_to_A){nums++;}
      if(probs(i,1) < sub_rate_G_to_T){nums++;}
      if(probs(i,2) < sub_rate_G_to_C){nums++;}
      if(nums >= 1){
        int selection = (runif(1)(0))*(nums)+1;
        int index=1;
        if(probs(i,0) < sub_rate_G_to_A){
          if(index == selection){output(i)="A";}
          index++;
        }
        if(probs(i,1) < sub_rate_G_to_T){
          if(index == selection){output(i)="T";}
          index++;
        }
        if(probs(i,2) < sub_rate_G_to_C){
          if(index == selection){output(i)="C";}
        }
      }
    }
  }

  return output;
}

/*** R
set.seed(12345678)
substitution_mutator(c("C","A","T","G","A"))
  */
