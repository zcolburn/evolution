context("Insertion mutator cpp")

test_that(
  ".insertion_mutator_cpp can insert nucleotides",
  {
    expect_that(
      {
        set.seed(20)
        value <- .insertion_mutator_cpp(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","C","G","A"))
    )
  }
)

test_that(
  ".insertion_mutator_cpp doesn't always insert nucleotides",
  {
    expect_that(
      {
        set.seed(10)
        value <- .insertion_mutator_cpp(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","G","A"))
    )
  }
)
