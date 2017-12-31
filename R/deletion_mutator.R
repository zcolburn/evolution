#' @title Randomly delete nucleotides in a sequence
#'
#' @description Pass in a vector of nucleotides, the probability of single
#' nucleotide deletions for each of the four nucleotides, and return a new
#' vector of nucleotides.
#'
#' @param nt_sequence A vector of nucleotides, i.e. c("C","A","T","G","A").
#' @param deletion_rate_A The probability of an "A" deletion occurring.
#' @param deletion_rate_T The probability of an "T" deletion occurring.
#' @param deletion_rate_C The probability of an "C" deletion occurring.
#' @param deletion_rate_G The probability of an "G" deletion occurring.
#'
#' @importFrom assertthat assert_that is.number noNA
#'
#' @return A vector of nucleotides, i.e. c("C","T","G","A"), with deleted
#' nucleotides omitted.
#'
#' @examples
#' set.seed(1)
#' deletion_mutator(
#' c("C","A","T","G","A"),
#' deletion_rate_A = 0.05,
#' deletion_rate_T = 0.05,
#' deletion_rate_C = 0.05,
#' deletion_rate_G = 0.05
#' )
#'
#' @export
deletion_mutator <- function(
  nt_sequence,
  deletion_rate_A = 0.05,
  deletion_rate_T = 0.05,
  deletion_rate_C = 0.05,
  deletion_rate_G = 0.05
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
    is.number(deletion_rate_A) &&
      (class(deletion_rate_A) == "numeric") &&
      noNA(deletion_rate_A) &&
      (deletion_rate_A >= 0) && (deletion_rate_A <= 1)
  )
  assert_that(
    is.number(deletion_rate_T) &&
      (class(deletion_rate_T) == "numeric") &&
      noNA(deletion_rate_T) &&
      (deletion_rate_T >= 0) && (deletion_rate_T <= 1)
  )
  assert_that(
    is.number(deletion_rate_C) &&
      (class(deletion_rate_C) == "numeric") &&
      noNA(deletion_rate_C) &&
      (deletion_rate_C >= 0) && (deletion_rate_C <= 1)
  )
  assert_that(
    is.number(deletion_rate_G) &&
      (class(deletion_rate_G) == "numeric") &&
      noNA(deletion_rate_G) &&
      (deletion_rate_G >= 0) && (deletion_rate_G <= 1)
  )

  # Call .deletion_mutator_cpp
  .deletion_mutator_cpp(
    nt_sequence,
    deletion_rate_A,
    deletion_rate_T,
    deletion_rate_C,
    deletion_rate_G
  )
}
