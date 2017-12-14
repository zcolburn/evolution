#include <Rcpp.h>
using namespace Rcpp;

//' Iterate through the nucleotides and randomly insert nucleotides.
//'
//' @details Pass in a vector of nucleotides, the probability of any given
//' nucleotide being inserted, and get back a vector with insertion mutations.
//'
//' @param nt_sequence A vector of nucleotides, i.e. c("C","A","T","G","A").
//' @param insertion_rate_A The probability of an "A" insertion occurring.
//' @param insertion_rate_T The probability of an "T" insertion occurring.
//' @param insertion_rate_C The probability of an "C" insertion occurring.
//' @param insertion_rate_G The probability of an "G" insertion occurring.
//'
//' @import Rcpp
//'
//' @return A vector of nucleotides, i.e. c("C","A","T","G","A"), with any
//' insertions made included.
//'
//' @examples
//' set.seed(1)
//' insertion_mutator(
//' c("C","A","T","G","A"),
//' insertion_rate_A = 0.05,
//' insertion_rate_T = 0.05,
//' insertion_rate_C = 0.05,
//' insertion_rate_G = 0.05
//' )
//'
//' @export
//'
//' @useDynLib evolution, .registration = TRUE
//'
// [[Rcpp::export]]
StringVector insertion_mutator(
    StringVector nt_sequence,
    double insertion_rate_A = 0.05,
    double insertion_rate_T = 0.05,
    double insertion_rate_C = 0.05,
    double insertion_rate_G = 0.05
){
  if(
    (insertion_rate_A < 0) | (insertion_rate_A >= 1) |
      (insertion_rate_T < 0) | (insertion_rate_T >= 1) |
      (insertion_rate_C < 0) | (insertion_rate_C >= 1) |
      (insertion_rate_G < 0) | (insertion_rate_G >= 1)
  )
  {return NA_STRING;}

  int seq_length = nt_sequence.size();

  LogicalMatrix inserts(seq_length, 4);
  inserts(_, 0) = runif(seq_length) < insertion_rate_A;
  inserts(_, 1) = runif(seq_length) < insertion_rate_T;
  inserts(_, 2) = runif(seq_length) < insertion_rate_C;
  inserts(_, 3) = runif(seq_length) < insertion_rate_G;

  IntegerVector insertions(seq_length, 0);

  for(int i=0;i<seq_length;i++){
    for(int j=0;j<4;j++){
      if(inserts(i,j)){
        insertions(i) = insertions(i)+1;
      }
    }
  }

  int tot_size = 0;
  for(int i=0;i<seq_length;i++){
    if(insertions(i)==0){
      tot_size = tot_size + 1;
    } else {
      tot_size = tot_size + 2;
    }
  }

  if(tot_size == seq_length){
    return nt_sequence;
  }

  StringVector output(tot_size);
  int pos=0;
  for(int i=0;i<seq_length;i++){
    if(insertions(i)==0){
      output(pos)=nt_sequence(i);
      pos++;
    }else{
      int selection = runif(1)(0)*insertions(i);
      int index=0;
      for(int j=0;j<4;j++){
        if(inserts(i,j)){
          if(selection == index){
            output(pos)=nt_sequence(i);
            if(j==0){
              output(pos+1)="A";
            }else if(j==1){
              output(pos+1)="T";
            }else if(j==2){
              output(pos+1)="C";
            }else{
              output(pos+1)="G";
            }
            pos=pos+2;
          }else{
            index++;
          }
        }
      }
    }
  }

  return output;
}

/*** R
set.seed(20)
insertion_mutator(c("C","A","T","G","A"))
  */
