#include <Rcpp.h>
using namespace Rcpp;

//' Find the first start codon in a sequence.
//'
//' @details This function will return the index in a vector of nucleotides
//' of the first start codon.
//'
//' @param nt_sequence A vector of nucleotides, i.e. c("C","A","T","G","A").
//'
//' @import Rcpp
//'
//' @return Start codon site or NA if not found.
//'
//' @examples
//' find_start_codon(
//' c("A", "A", "T", "G", "C", "A", "A", "T", "C", "A", "T", "A", "G", "A")
//' )
//'
//' @export
//'
//' @useDynLib evolution, .registration = TRUE
//'
// [[Rcpp::export]]
int find_start_codon(StringVector nt_sequence){
  for(int i=0;i<nt_sequence.size()-3;i++){
    if(nt_sequence(i)=="A" && nt_sequence(i+1)=="T" && nt_sequence(i+2)=="G"){
      return i+1;
    }
  }
  return NA_INTEGER;
}

/*** R
find_start_codon(
  c("A", "A", "T", "G", "C", "A", "A", "T", "C", "A", "T", "A", "G", "A")
)
  */
