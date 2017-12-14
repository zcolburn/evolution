context("add_start_and_stop_codons")

test_that(
  "add_start_and_stop_codons can add these codons",
  {
    expect_that(
      {
        add_start_and_stop_codons(c("A","T","A"))
      },
      equals(c("A","T","G","A","T","A","T","A","G"))
    )
  }
)

test_that(
  "add_start_and_stop_codons errors if the number of nucleotides is not a
  multiple of 3",
  {
    expect_error(
      {
add_start_and_stop_codons(c("C","A","T","G","A"))
      }
    )
  }
)
