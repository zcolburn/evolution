#include <Rcpp.h>
using namespace Rcpp;

//' @title Find the first in-frame stop codon
//'
//' @param nt_sequence A vector of nucleotides, i.e. c("A","T","G","A", ...) in
//' which the first codon is the start codon.
//'
//' @import Rcpp
//'
//' @return Index of the first in-frame stop codon (or NA if not found).
//'
//' @examples
//' .find_stop_codon(
//' c("A", "A", "T", "G", "C", "A", "A", "T", "C", "A", "T", "A", "G", "A")
//' )
//'
//' @useDynLib evolution, .registration = TRUE
//'
// [[Rcpp::export(.find_stop_codon_cpp)]]
int find_stop_codon_cpp(StringVector nt_sequence){
  for(int j=0;j<nt_sequence.size();j++){
    int i_one=j*3;
    int i_two=i_one+1;
    int i_three=i_one+2;

    if(i_three > (nt_sequence.size()+1)){return NA_INTEGER;}

    if(nt_sequence(i_one)=="T"){
      if(
        ((nt_sequence(i_two)=="A") && (nt_sequence(i_three)=="G")) ||
          ((nt_sequence(i_two)=="A") && (nt_sequence(i_three)=="A")) ||
          ((nt_sequence(i_two)=="G") && (nt_sequence(i_three)=="A"))
      ){return i_one+1;}
    }
  }

  return NA_INTEGER;
}

/*** R
find_stop_codon(
  c("A", "A", "T", "G", "C", "A", "A", "T", "C", "A", "T", "A", "G", "A")
)
  */
