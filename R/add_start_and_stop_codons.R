#' Add start and stop codons
#'
#' @param nt_sequence Vector of nucleotides.
#'
#' @return Vector of nucleotides with start and stop c("T","A","G") codons
#' prepended and appended, respectively. An error will be returned if the
#' number of nucleotides is not a multiple of 3.
#'
#' @export
#'
#' @importFrom assertthat assert_that noNA
#'
#' @examples
#' add_start_and_stop_codons(c("A","T","A"))
add_start_and_stop_codons <- function(
  nt_sequence
){
  assert_that(
    is.vector(nt_sequence) &&
      (class(nt_sequence) == "character") &&
      noNA(nt_sequence) &&
      all(nt_sequence %in% c("A","T","C","G"))
  )
  if(!((length(nt_sequence) %% 3) == 0)){
    stop("Nucleotide sequence is not a multiple of 3")
  }
  c("A","T","G",nt_sequence,"T","A","G")
}
