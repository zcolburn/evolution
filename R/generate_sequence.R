#' Generate a DNA sequence.
#'
#' @param n Number of codons in the sequence.
#' @param start_codon TRUE/FALSE, whether a start codon should be included.
#' @param stop_codon TRUE/FALSE, whether a stop codon should be included.
#' @param probabilities Probabilities for A, T, C, and G, respectively.
#' @param allow_internal_stop_codons TRUE/FALSE, whether stop codons should be
#' generated in the sequence (not including the terminus if stop_codon is TRUE).
#'
#' @return A vector of nucleotides
#' @export
#'
#' @examples
#' generate_sequence(100, start_codon = TRUE, stop_codon = TRUE)
generate_sequence <- function(
  n = 100,
  start_codon = TRUE,
  stop_codon = TRUE,
  probabilities = c(0.25, 0.25, 0.25, 0.25),
  allow_internal_stop_codons = FALSE
){
  if(any((probabilities < 0) | (probabilities >= 1))){
    stop("Invalid probability value!")
  }
  if(n <= 0){
    stop("Invalid sequence length!")
  }
  if(start_codon) n <- n - 1
  if(stop_codon) n <- n - 1
  n <- n * 3
  if(n < 0) stop("The sample size is too small!")
  new_sequence <- sample(
    c("A","T","C","G"),
    size = n,
    replace = TRUE,
    prob = probabilities
  )

  # If the sequence was generated with a stop codon
  if(!allow_internal_stop_codons){
    while(!is.na(find_stop_codon(new_sequence))){
      # Randomly choose a nucleotide in the stop codon to mutate
      nt_index <- sample(0:2, 1) + find_stop_codon(new_sequence)

      # Mutate that nucleotide
      new_sequence[nt_index] <- sample(
        c("A","T","C","G"), size = 1, prob = probabilities
      )
    }
  }


  if(start_codon) new_sequence <- c("A","T","G",new_sequence)
  if(stop_codon) new_sequence <- c(new_sequence,"T","A","G")
  return(new_sequence)
}
