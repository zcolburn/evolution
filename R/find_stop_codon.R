#' @title Find the first in-frame stop codon
#'
#' @description This function will return the index in a vector of nucleotides
#' of the first stop codon. nt_sequence is assumed to be in-frame.
#'
#' @param nt_sequence A vector of nucleotides, i.e. c("A","T","G","A", ...) in
#' which the first codon is the start codon.
#'
#' @importFrom assertthat assert_that noNA
#'
#' @return Index of the first in-frame stop codon (or NA if not found).
#'
#' @examples
#' find_stop_codon(
#' c("A", "A", "T", "G", "C", "A", "A", "T", "C", "A", "T", "A", "G", "A")
#' )
#'
#' @export
find_stop_codon <- function(
  nt_sequence
){
  # Perform type checking.
  assert_that(
    is.vector(nt_sequence) &&
      (class(nt_sequence) == "character") &&
      noNA(nt_sequence) &&
      all(nt_sequence %in% c("A","T","C","G")) &&
      (length(nt_sequence) >= 3)
  )

  # Call .find_stop_codon_cpp
  .find_stop_codon_cpp(nt_sequence)
}
