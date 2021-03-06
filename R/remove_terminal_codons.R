#' Remove terminal codons.
#'
#' @param nt_sequence Vector of nucleotides.
#'
#' @return Vector of nucleotides with the terminal codons removed.
#'
#' @importFrom assertthat assert_that noNA
#'
#' @export
#'
#' @examples
#' remove_terminal_codons(c("A","T","G","A","T","A","T","A","G"))
remove_terminal_codons <- function(
  nt_sequence
){
  assert_that(
    is.vector(nt_sequence) &&
      (class(nt_sequence) == "character") &&
      noNA(nt_sequence)
  )
  if(!(length(nt_sequence) %% 3) == 0){
    stop("Sequence length not a multiple of 3!")
  }
  if(length(nt_sequence) < 9){stop("Sequence too short!")}
  nt_sequence[4:(length(nt_sequence)-3)]
}
