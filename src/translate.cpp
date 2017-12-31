#include <Rcpp.h>
using namespace Rcpp;

//' Translate a DNA sequence into an amino acid sequence.
//'
//' @param nt_sequence An in-frame vector of nucleotides, i.e. c("A","T","G","T"
//' ,"A","G"). The first and last codons should be start and stop codons.
//' @param check_start_and_stop Logical indicating whether the first and last
//' codons need to be start and stop codons, respectively.
//'
//' @import Rcpp
//'
//' @return A vector of amino acids.
//'
//' @examples
//' translate(
//' c("A","T","G","T","A","G")
//' )
//'
//' @export
//'
//' @useDynLib evolution, .registration = TRUE
//'
// [[Rcpp::export]]
StringVector translate(
    StringVector nt_sequence,
    bool check_start_and_stop = false
){
  int seq_length = nt_sequence.size();

  // Check for things that could cause errors
  if(seq_length % 3 != 0){
    Rcpp::Rcout << "nt_sequence is not a multiple of 3!";
    return NA_STRING;
  }
  if(
    check_start_and_stop &&
    (((nt_sequence(0)!="A") && (nt_sequence(1)!="T") && (nt_sequence(2)!="G")) |
      ((nt_sequence(seq_length-3)!="T") && !(
        ((nt_sequence(seq_length-2)=="A") && (
          (nt_sequence(seq_length-1)=="A") ||
            (nt_sequence(seq_length-1)=="G")
        )) ||
          ((nt_sequence(seq_length-2)=="G") && (nt_sequence(seq_length-1)=="A")))
      ))
  ){
    Rcpp::Rcout << "The first and last codons are not start and stop codons!";
    return NA_STRING;
  }

  // Initialize output vector
  StringVector output(seq_length/3);

  for(int i=0;i<seq_length/3;i++){
    // Define nucleotide positions
    int nt_1=i*3;
    int nt_2=nt_1+1;
    int nt_3=nt_2+1;
    if(nt_sequence(nt_1)=="T"){//First nucleotide
      if(nt_sequence(nt_2)=="T"){
        if(nt_sequence(nt_3)=="T" || nt_sequence(nt_3)=="C"){
          output(i)="F";
        }else if(nt_sequence(nt_3)=="A" || nt_sequence(nt_3)=="G"){
          output(i)="L";
        }
      }else if(nt_sequence(nt_2)=="C"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="G" ||
           nt_sequence(nt_3)=="A"){output(i)="S";}
      }else if(nt_sequence(nt_2)=="A"){
        if(nt_sequence(nt_3)=="T" || nt_sequence(nt_3)=="C"){
          output(i)="Y";
        }
      }else if(nt_sequence(nt_2)=="G"){
        if(nt_sequence(nt_3)=="G"){
          output(i)="W";
        }else if(nt_sequence(nt_3)=="T" || nt_sequence(nt_3)=="C"){
          output(i)="C";
        }
      }
    }else if(nt_sequence(nt_1)=="C"){//First nucleotide
      if(nt_sequence(nt_2)=="T"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="A" ||
           nt_sequence(nt_3)=="G"){output(i)="L";}
      }else if(nt_sequence(nt_2)=="C"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="A" ||
           nt_sequence(nt_3)=="G"){output(i)="P";}
      }else if(nt_sequence(nt_2)=="A"){
        if(nt_sequence(nt_3)=="T" || nt_sequence(nt_3)=="C"){
          output(i)="H";
        }else if(nt_sequence(nt_3)=="A" || nt_sequence(nt_3)=="G"){
          output(i)="Q";
        }
      }else if(nt_sequence(nt_2)=="G"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="A" ||
           nt_sequence(nt_3)=="G"){output(i)="R";}
      }
    }else if(nt_sequence(nt_1)=="A"){//First nucleotide
      if(nt_sequence(nt_2)=="T"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="A"){output(i)="I";}
        else if(nt_sequence(nt_3)=="G"){output(i)="M";}
      }else if(nt_sequence(nt_2)=="C"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="A" ||
           nt_sequence(nt_3)=="G"){output(i)="T";}
      }else if(nt_sequence(nt_2)=="A"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C"){output(i)="N";}
        else if(nt_sequence(nt_3)=="A" ||
                nt_sequence(nt_3)=="G"){output(i)="K";}
      }else if(nt_sequence(nt_2)=="G"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C"){output(i)="S";}
        else if(nt_sequence(nt_3)=="A" ||
                nt_sequence(nt_3)=="G"){output(i)="R";}
      }
    }else if(nt_sequence(nt_1)=="G"){//First nucleotide
      if(nt_sequence(nt_2)=="T"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="A" ||
           nt_sequence(nt_3)=="G"){output(i)="V";}
      }else if(nt_sequence(nt_2)=="C"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="A" ||
           nt_sequence(nt_3)=="G"){output(i)="A";}
      }else if(nt_sequence(nt_2)=="A"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C"){output(i)="D";}
        else if(nt_sequence(nt_3)=="A" ||
                nt_sequence(nt_3)=="G"){output(i)="E";}
      }else if(nt_sequence(nt_2)=="G"){
        if(nt_sequence(nt_3)=="T" ||
           nt_sequence(nt_3)=="C" ||
           nt_sequence(nt_3)=="A" ||
           nt_sequence(nt_3)=="G"){output(i)="G";}
      }
    }
  }

  // Set all empty string elements to NA.
  for(int i=0;i<output.size();i++){
    if(output(i)==""){
      output(i)=NA_STRING;
    }
  }

  return output;
}

/*** R
translate(c("A","T","G","G","G","A","T","A","G"))
  */
