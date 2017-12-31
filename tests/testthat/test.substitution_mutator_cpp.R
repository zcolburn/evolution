context("Substitution mutator cpp")

test_that(
  ".substitution_mutator_cpp can substitute nucleotides",
  {
    expect_that(
      {
        set.seed(12345678)
        .substitution_mutator_cpp(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","G","T"))
    )
  }
)

test_that(
  ".substitution_mutator_cpp doesn't always substitute nucleotides",
  {
    expect_that(
      {
        set.seed(1)
        value <- .substitution_mutator_cpp(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","G","A"))
    )
  }
)
