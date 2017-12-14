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
#' @examples
#' add_start_and_stop_codons(c("A","T","A"))
add_start_and_stop_codons <- function(
  nt_sequence
){
  if(!((length(nt_sequence) %% 3) == 0)){
    stop("Nucleotide sequence is not a multiple of 3")
  }
  c("A","T","G",nt_sequence,"T","A","G")
}
