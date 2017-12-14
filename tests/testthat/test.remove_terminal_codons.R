context("remove_terminal_codons")

test_that(
  "remove_terminal_codons will error if too few nucleotides",
  {
    expect_error(
      {
        remove_terminal_codons(c("A","T","G","A","A","T","A","G"))
      }
    )
  }
)



test_that(
  "remove_terminal_codons will error if the number of nucleotides is not a
  multiple of 3",
  {
    expect_error(
      {
        remove_terminal_codons(c("A","T","G","A","A","A","A","T","A","G"))
      }
    )
  }
)

test_that(
  "remove_terminal_codons can remove terminal codons",
  {
    expect_that(
      {
        set.seed(10)
        remove_terminal_codons(c("A","T","G","A","T","A","T","A","G"))
      },
      equals(c("A","T","A"))
    )
  }
)
