#' Perform substitution mutations on a nucelotide sequence.
#'
#' @details Pass in a vector of nucleotides and the probability of each type of
#'  substitution mutation. Get back a mutated vector.
#'
#' @param nt_sequence A vector of nucleotides, i.e. c("C","A","T","G","A").
#' @param sub_rate_A_to_T The probability of an "A" becoming a "T".
#' @param sub_rate_A_to_C The probability of an "A" becoming a "C".
#' @param sub_rate_A_to_G The probability of an "A" becoming a "G".
#' @param sub_rate_T_to_A The probability of an "T" becoming a "A".
#' @param sub_rate_T_to_C The probability of an "T" becoming a "C".
#' @param sub_rate_T_to_G The probability of an "T" becoming a "G".
#' @param sub_rate_C_to_A The probability of an "C" becoming a "A".
#' @param sub_rate_C_to_T The probability of an "C" becoming a "T".
#' @param sub_rate_C_to_G The probability of an "C" becoming a "G".
#' @param sub_rate_G_to_A The probability of an "G" becoming a "A".
#' @param sub_rate_G_to_T The probability of an "G" becoming a "T".
#' @param sub_rate_G_to_C The probability of an "G" becoming a "C".
#'
#' @importFrom assertthat assert_that noNA is.number
#'
#' @return A vector of nucleotides, i.e. c("C","A","A","G","A"), with
#' subsitution mutations included.
#'
#' @examples
#' set.seed(1)
#' substitution_mutator(
#' c("C","A","T","G","A")
#' )
#'
#' @export
substitution_mutator <- function(
  nt_sequence,
  sub_rate_A_to_T = 0.05,
  sub_rate_A_to_C = 0.05,
  sub_rate_A_to_G = 0.05,
  sub_rate_T_to_A = 0.05,
  sub_rate_T_to_C = 0.05,
  sub_rate_T_to_G = 0.05,
  sub_rate_C_to_A = 0.05,
  sub_rate_C_to_T = 0.05,
  sub_rate_C_to_G = 0.05,
  sub_rate_G_to_A = 0.05,
  sub_rate_G_to_T = 0.05,
  sub_rate_G_to_C = 0.05
){
  # Perform type checking.
  assert_that(
    is.vector(nt_sequence) &&
      (class(nt_sequence) == "character") &&
      noNA(nt_sequence) &&
      all(nt_sequence %in% c("A","T","C","G")) &&
      length(nt_sequence) > 0
  )
  # A to ...
  assert_that(
    is.number(sub_rate_A_to_T) &&
      (class(sub_rate_A_to_T) == "numeric") &&
      noNA(sub_rate_A_to_T) &&
      (sub_rate_A_to_T >= 0) && (sub_rate_A_to_T <= 1)
  )
  assert_that(
    is.number(sub_rate_A_to_C) &&
      (class(sub_rate_A_to_C) == "numeric") &&
      noNA(sub_rate_A_to_C) &&
      (sub_rate_A_to_C >= 0) && (sub_rate_A_to_C <= 1)
  )
  assert_that(
    is.number(sub_rate_A_to_G) &&
      (class(sub_rate_A_to_G) == "numeric") &&
      noNA(sub_rate_A_to_G) &&
      (sub_rate_A_to_G >= 0) && (sub_rate_A_to_G <= 1)
  )
  # T to ...
  assert_that(
    is.number(sub_rate_T_to_A) &&
      (class(sub_rate_T_to_A) == "numeric") &&
      noNA(sub_rate_T_to_A) &&
      (sub_rate_T_to_A >= 0) && (sub_rate_T_to_A <= 1)
  )
  assert_that(
    is.number(sub_rate_T_to_C) &&
      (class(sub_rate_T_to_C) == "numeric") &&
      noNA(sub_rate_T_to_C) &&
      (sub_rate_T_to_C >= 0) && (sub_rate_T_to_C <= 1)
  )
  assert_that(
    is.number(sub_rate_T_to_G) &&
      (class(sub_rate_T_to_G) == "numeric") &&
      noNA(sub_rate_T_to_G) &&
      (sub_rate_T_to_G >= 0) && (sub_rate_T_to_G <= 1)
  )
  # C to ...
  assert_that(
    is.number(sub_rate_C_to_A) &&
      (class(sub_rate_C_to_A) == "numeric") &&
      noNA(sub_rate_C_to_A) &&
      (sub_rate_C_to_A >= 0) && (sub_rate_C_to_A <= 1)
  )
  assert_that(
    is.number(sub_rate_C_to_T) &&
      (class(sub_rate_C_to_T) == "numeric") &&
      noNA(sub_rate_C_to_T) &&
      (sub_rate_C_to_T >= 0) && (sub_rate_C_to_T <= 1)
  )
  assert_that(
    is.number(sub_rate_C_to_G) &&
      (class(sub_rate_C_to_G) == "numeric") &&
      noNA(sub_rate_C_to_G) &&
      (sub_rate_C_to_G >= 0) && (sub_rate_C_to_G <= 1)
  )
  # G to ...
  assert_that(
    is.number(sub_rate_G_to_A) &&
      (class(sub_rate_G_to_A) == "numeric") &&
      noNA(sub_rate_G_to_A) &&
      (sub_rate_G_to_A >= 0) && (sub_rate_G_to_A <= 1)
  )
  assert_that(
    is.number(sub_rate_G_to_T) &&
      (class(sub_rate_G_to_T) == "numeric") &&
      noNA(sub_rate_G_to_T) &&
      (sub_rate_G_to_T >= 0) && (sub_rate_G_to_T <= 1)
  )
  assert_that(
    is.number(sub_rate_G_to_C) &&
      (class(sub_rate_G_to_C) == "numeric") &&
      noNA(sub_rate_G_to_C) &&
      (sub_rate_G_to_C >= 0) && (sub_rate_G_to_C <= 1)
  )

  # Call .substitution_mutator_cpp
  .substitution_mutator_cpp(
    nt_sequence,
    sub_rate_A_to_T,
    sub_rate_A_to_C,
    sub_rate_A_to_G,
    sub_rate_T_to_A,
    sub_rate_T_to_C,
    sub_rate_T_to_G,
    sub_rate_C_to_A,
    sub_rate_C_to_T,
    sub_rate_C_to_G,
    sub_rate_G_to_A,
    sub_rate_G_to_T,
    sub_rate_G_to_C
  )
}
