context("Deletion mutator cpp")

test_that(
  ".deletion_mutator_cpp can delete nucleotides",
  {
    expect_that(
      {
        set.seed(20)
        value <- .deletion_mutator_cpp(c("C","A","T","G","A"))
      },
      equals(c("C","A","G","A"))
    )
  }
)

test_that(
  ".deletion_mutator_cpp doesn't always delete nucleotides",
  {
    expect_that(
      {
        set.seed(10)
        value <- .deletion_mutator_cpp(c("C","A","T","G","A"))
        value
      },
      equals(c("C","A","T","G","A"))
    )
  }
)
