context("Find start codon")

test_that(
  "find_start codon can find a start codon",
  {
    expect_that(
      {
        find_start_codon(
          c("A","A","T","G","C","A","A","T","C","A","T","A","G","A")
        )
      },
      equals(2)
    )
  }
)

test_that(
  "find_start returns NA when there is no start codon",
  {
    expect_true(
      {
        is.na(find_start_codon(
          c("A","A","A","G","C","A","A","T","C","A","T","A","G","A")
        ))
      }
    )
  }
)
