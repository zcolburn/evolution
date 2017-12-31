context("translation")

test_that("translate correctly translates codons",{
  # I
  expect_that(translate(c("A","T","T"), FALSE), equals("I"))
  expect_that(translate(c("A","T","C"), FALSE), equals("I"))
  expect_that(translate(c("A","T","A"), FALSE), equals("I"))

  # L
  expect_that(translate(c("C","T","T"), FALSE), equals("L"))
  expect_that(translate(c("C","T","C"), FALSE), equals("L"))
  expect_that(translate(c("C","T","A"), FALSE), equals("L"))
  expect_that(translate(c("C","T","G"), FALSE), equals("L"))
  expect_that(translate(c("T","T","A"), FALSE), equals("L"))
  expect_that(translate(c("T","T","G"), FALSE), equals("L"))

  # V
  expect_that(translate(c("G","T","T"), FALSE), equals("V"))
  expect_that(translate(c("G","T","C"), FALSE), equals("V"))
  expect_that(translate(c("G","T","A"), FALSE), equals("V"))
  expect_that(translate(c("G","T","G"), FALSE), equals("V"))

  # F
  expect_that(translate(c("T","T","T"), FALSE), equals("F"))
  expect_that(translate(c("T","T","C"), FALSE), equals("F"))

  # M
  expect_that(translate(c("A","T","G"), FALSE), equals("M"))

  # C
  expect_that(translate(c("T","G","T"), FALSE), equals("C"))
  expect_that(translate(c("T","G","C"), FALSE), equals("C"))

  # A
  expect_that(translate(c("G","C","T"), FALSE), equals("A"))
  expect_that(translate(c("G","C","C"), FALSE), equals("A"))
  expect_that(translate(c("G","C","A"), FALSE), equals("A"))
  expect_that(translate(c("G","C","G"), FALSE), equals("A"))

  # G
  expect_that(translate(c("G","G","T"), FALSE), equals("G"))
  expect_that(translate(c("G","G","C"), FALSE), equals("G"))
  expect_that(translate(c("G","G","A"), FALSE), equals("G"))
  expect_that(translate(c("G","G","G"), FALSE), equals("G"))

  # P
  expect_that(translate(c("C","C","T"), FALSE), equals("P"))
  expect_that(translate(c("C","C","C"), FALSE), equals("P"))
  expect_that(translate(c("C","C","A"), FALSE), equals("P"))
  expect_that(translate(c("C","C","G"), FALSE), equals("P"))

  # T
  expect_that(translate(c("A","C","T"), FALSE), equals("T"))
  expect_that(translate(c("A","C","C"), FALSE), equals("T"))
  expect_that(translate(c("A","C","A"), FALSE), equals("T"))
  expect_that(translate(c("A","C","G"), FALSE), equals("T"))

  # S
  expect_that(translate(c("T","C","T"), FALSE), equals("S"))
  expect_that(translate(c("T","C","C"), FALSE), equals("S"))
  expect_that(translate(c("T","C","A"), FALSE), equals("S"))
  expect_that(translate(c("T","C","G"), FALSE), equals("S"))
  expect_that(translate(c("A","G","T"), FALSE), equals("S"))
  expect_that(translate(c("A","G","C"), FALSE), equals("S"))

  # Y
  expect_that(translate(c("T","A","T"), FALSE), equals("Y"))
  expect_that(translate(c("T","A","C"), FALSE), equals("Y"))

  # W
  expect_that(translate(c("T","G","G"), FALSE), equals("W"))

  # Q
  expect_that(translate(c("C","A","A"), FALSE), equals("Q"))
  expect_that(translate(c("C","A","G"), FALSE), equals("Q"))

  # N
  expect_that(translate(c("A","A","T"), FALSE), equals("N"))
  expect_that(translate(c("A","A","C"), FALSE), equals("N"))

  # H
  expect_that(translate(c("C","A","T"), FALSE), equals("H"))
  expect_that(translate(c("C","A","C"), FALSE), equals("H"))

  # E
  expect_that(translate(c("G","A","A"), FALSE), equals("E"))
  expect_that(translate(c("G","A","G"), FALSE), equals("E"))

  # D
  expect_that(translate(c("G","A","T"), FALSE), equals("D"))
  expect_that(translate(c("G","A","C"), FALSE), equals("D"))

  # K
  expect_that(translate(c("A","A","A"), FALSE), equals("K"))
  expect_that(translate(c("A","A","G"), FALSE), equals("K"))

  # R
  expect_that(translate(c("C","G","T"), FALSE), equals("R"))
  expect_that(translate(c("C","G","C"), FALSE), equals("R"))
  expect_that(translate(c("C","G","A"), FALSE), equals("R"))
  expect_that(translate(c("C","G","G"), FALSE), equals("R"))
  expect_that(translate(c("A","G","A"), FALSE), equals("R"))
  expect_that(translate(c("A","G","G"), FALSE), equals("R"))

  expect_that({
    set.seed(10)
    nt_seq <- generate_sequence(3, FALSE, FALSE)
    p_seq <- translate(nt_seq, FALSE)
  },
  equals(c("A","V","P"))
  )
}
)
