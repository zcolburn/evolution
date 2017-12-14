context("translation")

test_that("translate correctly translates codons",{
  # I
  expect_that(translate(c("A","T","T")), equals("I"))
  expect_that(translate(c("A","T","C")), equals("I"))
  expect_that(translate(c("A","T","A")), equals("I"))

  # L
  expect_that(translate(c("C","T","T")), equals("L"))
  expect_that(translate(c("C","T","C")), equals("L"))
  expect_that(translate(c("C","T","A")), equals("L"))
  expect_that(translate(c("C","T","G")), equals("L"))
  expect_that(translate(c("T","T","A")), equals("L"))
  expect_that(translate(c("T","T","G")), equals("L"))

  # V
  expect_that(translate(c("G","T","T")), equals("V"))
  expect_that(translate(c("G","T","C")), equals("V"))
  expect_that(translate(c("G","T","A")), equals("V"))
  expect_that(translate(c("G","T","G")), equals("V"))

  # F
  expect_that(translate(c("T","T","T")), equals("F"))
  expect_that(translate(c("T","T","C")), equals("F"))

  # M
  expect_that(translate(c("A","T","G")), equals("M"))

  # C
  expect_that(translate(c("T","G","T")), equals("C"))
  expect_that(translate(c("T","G","C")), equals("C"))

  # A
  expect_that(translate(c("G","C","T")), equals("A"))
  expect_that(translate(c("G","C","C")), equals("A"))
  expect_that(translate(c("G","C","A")), equals("A"))
  expect_that(translate(c("G","C","G")), equals("A"))

  # G
  expect_that(translate(c("G","G","T")), equals("G"))
  expect_that(translate(c("G","G","C")), equals("G"))
  expect_that(translate(c("G","G","A")), equals("G"))
  expect_that(translate(c("G","G","G")), equals("G"))

  # P
  expect_that(translate(c("C","C","T")), equals("P"))
  expect_that(translate(c("C","C","C")), equals("P"))
  expect_that(translate(c("C","C","A")), equals("P"))
  expect_that(translate(c("C","C","G")), equals("P"))

  # T
  expect_that(translate(c("A","C","T")), equals("T"))
  expect_that(translate(c("A","C","C")), equals("T"))
  expect_that(translate(c("A","C","A")), equals("T"))
  expect_that(translate(c("A","C","G")), equals("T"))

  # S
  expect_that(translate(c("T","C","T")), equals("S"))
  expect_that(translate(c("T","C","C")), equals("S"))
  expect_that(translate(c("T","C","A")), equals("S"))
  expect_that(translate(c("T","C","G")), equals("S"))
  expect_that(translate(c("A","G","T")), equals("S"))
  expect_that(translate(c("A","G","C")), equals("S"))

  # Y
  expect_that(translate(c("T","A","T")), equals("Y"))
  expect_that(translate(c("T","A","C")), equals("Y"))

  # W
  expect_that(translate(c("T","G","G")), equals("W"))

  # Q
  expect_that(translate(c("C","A","A")), equals("Q"))
  expect_that(translate(c("C","A","G")), equals("Q"))

  # N
  expect_that(translate(c("A","A","T")), equals("N"))
  expect_that(translate(c("A","A","C")), equals("N"))

  # H
  expect_that(translate(c("C","A","T")), equals("H"))
  expect_that(translate(c("C","A","C")), equals("H"))

  # E
  expect_that(translate(c("G","A","A")), equals("E"))
  expect_that(translate(c("G","A","G")), equals("E"))

  # D
  expect_that(translate(c("G","A","T")), equals("D"))
  expect_that(translate(c("G","A","C")), equals("D"))

  # K
  expect_that(translate(c("A","A","A")), equals("K"))
  expect_that(translate(c("A","A","G")), equals("K"))

  # R
  expect_that(translate(c("C","G","T")), equals("R"))
  expect_that(translate(c("C","G","C")), equals("R"))
  expect_that(translate(c("C","G","A")), equals("R"))
  expect_that(translate(c("C","G","G")), equals("R"))
  expect_that(translate(c("A","G","A")), equals("R"))
  expect_that(translate(c("A","G","G")), equals("R"))

  expect_that({
    set.seed(10)
    nt_seq <- generate_sequence(3, FALSE, FALSE)
    p_seq <- translate(nt_seq)
  },
  equals(c("A","V","P"))
  )

  expect_true(
    is.na(translate(c("C","G","T"), TRUE))
  )
}
)
