context("Insertion mutator")

test_that(
  "insertion_mutator can insert nucleotides",
  {
    expect_that(
      {
        set.seed(20)
        value <- insertion_mutator(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","C","G","A"))
    )
  }
)

test_that(
  "insertion_mutator doesn't always insert nucleotides",
  {
    expect_that(
      {
        set.seed(10)
        value <- insertion_mutator(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","G","A"))
    )
  }
)

test_that(
  "insertion_mutator returns NA with invalid probability inputs",
  {
    expect_true(
      {
        value <- insertion_mutator(c("C","A","A"),0.05,0.05,-0.1,0.05)
        is.na(value)
      }
    )
  }
)
