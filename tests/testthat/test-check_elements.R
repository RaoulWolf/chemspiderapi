library(chemspiderapi)

context("check_elements")

test_that("check_elements() fails if no input is provided.", {
  expect_error(
    .check_elements()
    )
})

test_that("check_elements() fails if NULL is provided as input.", {
  expect_error(
    .check_elements(include_elements = NULL,
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if NULL is provided as input.", {
  expect_error(
    .check_elements(include_elements = c("C", "H", "O"),
                   exclude_elements = NULL)
  )
})

test_that("check_elements() fails if no input for include_elements is provided.", {
  expect_error(
    .check_elements(exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if no input for exclude_elements is provided.", {
  expect_error(
    .check_elements(include_elements = c("C", "H", "O"))
  )
})

test_that("check_elements() fails if a non-character input for include_elements is provided.", {
  expect_error(
    .check_elements(include_elements = c(1, 2, 3),
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if a non-character input for exclude_elements is provided.", {
  expect_error(
    .check_elements(include_elements = c("C", "H", "O"),
                   exclude_elements = c(1, 2, 3))
  )
})

test_that("check_elements() fails if include_elements contains over 15 entries.", {
  expect_error(
    .check_elements(include_elements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S"),
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if include_elements contains over 15 entries.", {
  expect_error(
    .check_elements(include_elements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S"),
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if exclude_elements contains over 100 entries.", {
  expect_error(
    .check_elements(include_elements = c("C", "H", "O"),
                   exclude_elements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md"))
  )
})

test_that("check_elements() fails if include_elements contains a non-periodic table element symbol.", {
  expect_error(
    .check_elements(include_elements = "J",
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("check_elements() fails if exclude_elements contains a non-periodic table element symbol.", {
  expect_error(
    .check_elements(include_elements = c("C", "H", "O"),
                   exclude_elements = "J")
  )
})

test_that("check_elements() remains silent when correct inputs are provided", {
  expect_silent(
    .check_elements(include_elements = c("C", "H", "O"),
                   exclude_elements = c("Na", "K", "Fe"))
  )
})
