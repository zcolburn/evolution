context("Substitution mutator")

test_that(
  "substitution_mutator can substitute nucleotides",
  {
    expect_that(
      {
        set.seed(12345678)
        substitution_mutator(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","G","T"))
    )
  }
)

test_that(
  "substitution_mutator doesn't always substitute nucleotides",
  {
    expect_that(
      {
        set.seed(1)
        value <- substitution_mutator(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","G","A"))
    )
  }
)

test_that(
  "substitution_mutator returns NA with invalid probability inputs",
  {
    expect_error(
      {
        substitution_mutator(
          c("C","A","A"),
          0.05,0.05,-0.1,0.05,0.05,0.05,0.05,0.05
        )
      }
    )
  }
)
