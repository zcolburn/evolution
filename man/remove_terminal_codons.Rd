% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_terminal_codons.R
\name{remove_terminal_codons}
\alias{remove_terminal_codons}
\title{Remove terminal codons.}
\usage{
remove_terminal_codons(nt_sequence)
}
\arguments{
\item{nt_sequence}{Vector of nucleotides.}
}
\value{
Vector of nucleotides with the terminal codons removed.
}
\description{
Remove terminal codons.
}
\examples{
remove_terminal_codons(c("A","T","G","A","T","A","T","A","G"))
}
