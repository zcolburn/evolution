context("Deletion mutator")

test_that(
  "deletion_mutator can delete nucleotides",
  {
    expect_that(
      {
        set.seed(20)
        value <- deletion_mutator(c("C","A","T","G","A"))
      },
      equals(c("C","A","G","A"))
    )
  }
)

test_that(
  "deletion_mutator doesn't always delete nucleotides",
  {
    expect_that(
      {
        set.seed(10)
        value <- deletion_mutator(c("C","A","T","G","A"))
      },
      equals(c("C","A","T","G","A"))
    )
  }
)


test_that(
  "deletion_mutator returns NA with invalid probability inputs",
  {
    expect_error(
      {
        deletion_mutator(c("C","A","A"),1.2,0.05,0.05,0.05)
      }
    )
  }
)
