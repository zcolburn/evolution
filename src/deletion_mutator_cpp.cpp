#include <Rcpp.h>
using namespace Rcpp;

//' @title Randomly delete nucleotides in a sequence
//'
//' @description Pass in a vector of nucleotides, the probability of single
//' nucleotide deletions for each of the four nucleotides, and return a new
//' vector of nucleotides.
//'
//' @param nt_sequence A vector of nucleotides, i.e. c("C","A","T","G","A").
//' @param deletion_rate_A The probability of an "A" deletion occurring.
//' @param deletion_rate_T The probability of an "T" deletion occurring.
//' @param deletion_rate_C The probability of an "C" deletion occurring.
//' @param deletion_rate_G The probability of an "G" deletion occurring.
//'
//' @import Rcpp
//'
//' @return A vector of nucleotides, i.e. c("C","T","G","A"), with deleted
//' nucleotides omitted.
//'
//' @examples
//' set.seed(1)
//' .deletion_mutator(
//' c("C","A","T","G","A"),
//' deletion_rate_A = 0.05,
//' deletion_rate_T = 0.05,
//' deletion_rate_C = 0.05,
//' deletion_rate_G = 0.05
//' )
//'
//' @useDynLib evolution, .registration = TRUE
//'
// [[Rcpp::export(.deletion_mutator_cpp)]]
StringVector deletion_mutator_cpp(StringVector nt_sequence,
                      double deletion_rate_A = 0.05,
                      double deletion_rate_T = 0.05,
                      double deletion_rate_C = 0.05,
                      double deletion_rate_G = 0.05){
  if(
    (deletion_rate_A < 0) | (deletion_rate_A >= 1) |
      (deletion_rate_T < 0) | (deletion_rate_T >= 1) |
      (deletion_rate_C < 0) | (deletion_rate_C >= 1) |
      (deletion_rate_G < 0) | (deletion_rate_G >= 1)
  )
  {return NA_STRING;}


  int seq_length = nt_sequence.size();

  LogicalMatrix dels(seq_length, 4);
  dels(_, 0) = runif(seq_length) < deletion_rate_A;
  dels(_, 1) = runif(seq_length) < deletion_rate_T;
  dels(_, 2) = runif(seq_length) < deletion_rate_C;
  dels(_, 3) = runif(seq_length) < deletion_rate_G;

  LogicalVector deletions(seq_length, false);

  for(int i=0;i<seq_length;i++){
    for(int j=0;j<4;j++){
      if(dels(i,j)){
        deletions(i) = true;
      }
    }
  }

  int tot_size = 0;
  for(int i=0;i<seq_length;i++){
    if(!deletions(i)){
      tot_size++;
    }
  }

  if(tot_size == 0){
    return nt_sequence;
  }

  StringVector output(tot_size);
  int pos=0;
  for(int i=0;i<seq_length;i++){
    if(!deletions(i)){
      output(pos)=nt_sequence(i);
      pos++;
    }
  }

  return output;
}

/*** R
set.seed(20)
deletion_mutator(c("C","A","T","G","A"))
  */
