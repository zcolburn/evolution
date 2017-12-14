context("generate_sequence")

test_that(
  "generate_sequence can add these codons",
  {
    expect_error(
      {
        generate_sequence(10, TRUE, TRUE, c(0.1, 0.3, 0.4, 1))
      }
    )
  }
)



test_that(
  "generate_sequence errors if the an invalid sequence length is given",
  {
    expect_error(
      {
        generate_sequence(-3)
      }
    )
  }
)

test_that(
  "generate_sequence can add these codons",
  {
    expect_that(
      {
        set.seed(10)
        generate_sequence(3, TRUE, TRUE, c(0.1, 0.3, 0.4, 0.5))
      },
      equals(c("A","T","G","C","G","C","T","A","G"))
    )
  }
)
