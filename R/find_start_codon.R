#' @title Find the first start codon in a sequence.
#'
#' @description This function will return the index in a vector of nucleotides
#' of the first start codon.
#'
#' @param nt_sequence A vector of nucleotides, i.e. c("C","A","T","G","A").
#'
#' @importFrom assertthat assert_that noNA
#'
#' @return Start codon site or NA if not found.
#'
#' @examples
#' find_start_codon(
#' c("A", "A", "T", "G", "C", "A", "A", "T", "C", "A", "T", "A", "G", "A")
#' )
#'
#' @export
find_start_codon <- function(
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

  # Call .find_start_codon_cpp
  .find_start_codon_cpp(nt_sequence)
}
