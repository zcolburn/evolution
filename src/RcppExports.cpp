// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// deletion_mutator_cpp
StringVector deletion_mutator_cpp(StringVector nt_sequence, double deletion_rate_A, double deletion_rate_T, double deletion_rate_C, double deletion_rate_G);
RcppExport SEXP _evolution_deletion_mutator_cpp(SEXP nt_sequenceSEXP, SEXP deletion_rate_ASEXP, SEXP deletion_rate_TSEXP, SEXP deletion_rate_CSEXP, SEXP deletion_rate_GSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type nt_sequence(nt_sequenceSEXP);
    Rcpp::traits::input_parameter< double >::type deletion_rate_A(deletion_rate_ASEXP);
    Rcpp::traits::input_parameter< double >::type deletion_rate_T(deletion_rate_TSEXP);
    Rcpp::traits::input_parameter< double >::type deletion_rate_C(deletion_rate_CSEXP);
    Rcpp::traits::input_parameter< double >::type deletion_rate_G(deletion_rate_GSEXP);
    rcpp_result_gen = Rcpp::wrap(deletion_mutator_cpp(nt_sequence, deletion_rate_A, deletion_rate_T, deletion_rate_C, deletion_rate_G));
    return rcpp_result_gen;
END_RCPP
}
// find_start_codon_cpp
int find_start_codon_cpp(StringVector nt_sequence);
RcppExport SEXP _evolution_find_start_codon_cpp(SEXP nt_sequenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type nt_sequence(nt_sequenceSEXP);
    rcpp_result_gen = Rcpp::wrap(find_start_codon_cpp(nt_sequence));
    return rcpp_result_gen;
END_RCPP
}
// find_stop_codon_cpp
int find_stop_codon_cpp(StringVector nt_sequence);
RcppExport SEXP _evolution_find_stop_codon_cpp(SEXP nt_sequenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type nt_sequence(nt_sequenceSEXP);
    rcpp_result_gen = Rcpp::wrap(find_stop_codon_cpp(nt_sequence));
    return rcpp_result_gen;
END_RCPP
}
// insertion_mutator_cpp
StringVector insertion_mutator_cpp(StringVector nt_sequence, double insertion_rate_A, double insertion_rate_T, double insertion_rate_C, double insertion_rate_G);
RcppExport SEXP _evolution_insertion_mutator_cpp(SEXP nt_sequenceSEXP, SEXP insertion_rate_ASEXP, SEXP insertion_rate_TSEXP, SEXP insertion_rate_CSEXP, SEXP insertion_rate_GSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type nt_sequence(nt_sequenceSEXP);
    Rcpp::traits::input_parameter< double >::type insertion_rate_A(insertion_rate_ASEXP);
    Rcpp::traits::input_parameter< double >::type insertion_rate_T(insertion_rate_TSEXP);
    Rcpp::traits::input_parameter< double >::type insertion_rate_C(insertion_rate_CSEXP);
    Rcpp::traits::input_parameter< double >::type insertion_rate_G(insertion_rate_GSEXP);
    rcpp_result_gen = Rcpp::wrap(insertion_mutator_cpp(nt_sequence, insertion_rate_A, insertion_rate_T, insertion_rate_C, insertion_rate_G));
    return rcpp_result_gen;
END_RCPP
}
// substitution_mutator_cpp
StringVector substitution_mutator_cpp(StringVector nt_sequence, float sub_rate_A_to_T, float sub_rate_A_to_C, float sub_rate_A_to_G, float sub_rate_T_to_A, float sub_rate_T_to_C, float sub_rate_T_to_G, float sub_rate_C_to_A, float sub_rate_C_to_T, float sub_rate_C_to_G, float sub_rate_G_to_A, float sub_rate_G_to_T, float sub_rate_G_to_C);
RcppExport SEXP _evolution_substitution_mutator_cpp(SEXP nt_sequenceSEXP, SEXP sub_rate_A_to_TSEXP, SEXP sub_rate_A_to_CSEXP, SEXP sub_rate_A_to_GSEXP, SEXP sub_rate_T_to_ASEXP, SEXP sub_rate_T_to_CSEXP, SEXP sub_rate_T_to_GSEXP, SEXP sub_rate_C_to_ASEXP, SEXP sub_rate_C_to_TSEXP, SEXP sub_rate_C_to_GSEXP, SEXP sub_rate_G_to_ASEXP, SEXP sub_rate_G_to_TSEXP, SEXP sub_rate_G_to_CSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type nt_sequence(nt_sequenceSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_A_to_T(sub_rate_A_to_TSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_A_to_C(sub_rate_A_to_CSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_A_to_G(sub_rate_A_to_GSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_T_to_A(sub_rate_T_to_ASEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_T_to_C(sub_rate_T_to_CSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_T_to_G(sub_rate_T_to_GSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_C_to_A(sub_rate_C_to_ASEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_C_to_T(sub_rate_C_to_TSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_C_to_G(sub_rate_C_to_GSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_G_to_A(sub_rate_G_to_ASEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_G_to_T(sub_rate_G_to_TSEXP);
    Rcpp::traits::input_parameter< float >::type sub_rate_G_to_C(sub_rate_G_to_CSEXP);
    rcpp_result_gen = Rcpp::wrap(substitution_mutator_cpp(nt_sequence, sub_rate_A_to_T, sub_rate_A_to_C, sub_rate_A_to_G, sub_rate_T_to_A, sub_rate_T_to_C, sub_rate_T_to_G, sub_rate_C_to_A, sub_rate_C_to_T, sub_rate_C_to_G, sub_rate_G_to_A, sub_rate_G_to_T, sub_rate_G_to_C));
    return rcpp_result_gen;
END_RCPP
}
// translate_cpp
StringVector translate_cpp(StringVector nt_sequence, bool check_start_and_stop);
RcppExport SEXP _evolution_translate_cpp(SEXP nt_sequenceSEXP, SEXP check_start_and_stopSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< StringVector >::type nt_sequence(nt_sequenceSEXP);
    Rcpp::traits::input_parameter< bool >::type check_start_and_stop(check_start_and_stopSEXP);
    rcpp_result_gen = Rcpp::wrap(translate_cpp(nt_sequence, check_start_and_stop));
    return rcpp_result_gen;
END_RCPP
}
