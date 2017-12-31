#' @title Translate a DNA sequence into an amino acid sequence.
#'
#' @description Translate a vector of DNA nucleotides sequence into an amino
#' acid sequence.
#'
#' @param nt_sequence An in-frame vector of nucleotides, i.e. c("A","T","G","T"
#' ,"A","G"). The first and last codons should be start and stop codons.
#' @param check_start_and_stop Logical indicating whether the first and last
#' codons need to be start and stop codons, respectively.
#'
#' @importFrom assertthat assert_that noNA is.flag
#'
#' @return A vector of amino acids.
#'
#' @examples
#' translate(
#' c("A","T","G","T","A","G")
#' )
#'
#' @export
translate <- function(
  nt_sequence,
  check_start_and_stop = TRUE
){
  # Perform type checking.
  assert_that(
    is.vector(nt_sequence) &&
      (class(nt_sequence) == "character") &&
      noNA(nt_sequence) &&
      all(nt_sequence %in% c("A","T","C","G")) &&
      length(nt_sequence) > 0
  )
  assert_that(is.flag(check_start_and_stop))

  # Call .translate_cpp
  .translate_cpp(
    nt_sequence,
    check_start_and_stop
  )
}
