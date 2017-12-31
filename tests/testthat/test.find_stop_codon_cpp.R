context("Find stop codon cpp")

test_that(
  "find_stop codon can find a stop codon",
  {
    expect_that(
      {
        .find_stop_codon_cpp(
          c("A","A","T","G","C","A","A","T","C","T","A","G","A")
        )
      },
      equals(10)
    )
  }
)

test_that(
  "find_stop returns NA when there is no stop codon",
  {
    expect_true(
      {
        is.na(.find_stop_codon_cpp(
          c("A","A","T","G","C","A","A","T","C","A","A","C","G","A")
        ))
      }
    )
  }
)
