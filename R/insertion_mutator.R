#' @title Iterate through the nucleotides and randomly insert nucleotides.
#'
#' @description Pass in a vector of nucleotides, the probability of any given
#' nucleotide being inserted, and get back a vector with insertion mutations.
#'
#' @param nt_sequence A vector of nucleotides, i.e. c("C","A","T","G","A").
#' @param insertion_rate_A The probability of an "A" insertion occurring.
#' @param insertion_rate_T The probability of an "T" insertion occurring.
#' @param insertion_rate_C The probability of an "C" insertion occurring.
#' @param insertion_rate_G The probability of an "G" insertion occurring.
#'
#' @importFrom assertthat assert_that noNA is.number
#'
#' @return A vector of nucleotides, i.e. c("C","A","T","G","A"), with any
#' insertions made included.
#'
#' @examples
#' set.seed(1)
#' insertion_mutator(
#' c("C","A","T","G","A"),
#' insertion_rate_A = 0.05,
#' insertion_rate_T = 0.05,
#' insertion_rate_C = 0.05,
#' insertion_rate_G = 0.05
#' )
#'
#' @export
insertion_mutator <- function(
  nt_sequence,
  insertion_rate_A = 0.05,
  insertion_rate_T = 0.05,
  insertion_rate_C = 0.05,
  insertion_rate_G = 0.05
){
  # Perform type checking.
  assert_that(
    is.vector(nt_sequence) &&
      (class(nt_sequence) == "character") &&
      noNA(nt_sequence) &&
      all(nt_sequence %in% c("A","T","C","G")) &&
      length(nt_sequence) > 0
  )
  assert_that(
    is.number(insertion_rate_A) &&
      (class(insertion_rate_A) == "numeric") &&
      noNA(insertion_rate_A) &&
      (insertion_rate_A >= 0) && (insertion_rate_A <= 1)
  )
  assert_that(
    is.number(insertion_rate_T) &&
      (class(insertion_rate_T) == "numeric") &&
      noNA(insertion_rate_T) &&
      (insertion_rate_T >= 0) && (insertion_rate_T <= 1)
  )
  assert_that(
    is.number(insertion_rate_C) &&
      (class(insertion_rate_C) == "numeric") &&
      noNA(insertion_rate_C) &&
      (insertion_rate_C >= 0) && (insertion_rate_C <= 1)
  )
  assert_that(
    is.number(insertion_rate_G) &&
      (class(insertion_rate_G) == "numeric") &&
      noNA(insertion_rate_G) &&
      (insertion_rate_G >= 0) && (insertion_rate_G <= 1)
  )

  # Call .insertion_mutator_cpp
  .insertion_mutator_cpp(
    nt_sequence,
    insertion_rate_A,
    insertion_rate_T,
    insertion_rate_C,
    insertion_rate_G
  )
}
