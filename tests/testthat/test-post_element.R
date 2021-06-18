library(chemspiderapi)

context("post_element")

test_that("check_elements() fails if no input is provided.", {
  expect_error(
    post_element()
  )
})

test_that("post_element() fails if NULL is provided as input.", {
  expect_error(
    post_element(include_elements = NULL,
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if NULL is provided as input.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                   exclude_elements = NULL)
  )
})

test_that("post_element() fails if no input for include_elements is provided.", {
  expect_error(
    post_element(exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if no input for exclude_elements is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"))
  )
})

test_that("post_element() fails if a non-character input for include_elements is provided.", {
  expect_error(
    post_element(include_elements = c(1, 2, 3),
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if a non-character input for exclude_elements is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                   exclude_elements = c(1, 2, 3))
  )
})

test_that("post_element() fails if include_elements contains over 15 entries.", {
  expect_error(
    post_element(include_elements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S"),
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if include_elements contains over 15 entries.", {
  expect_error(
    post_element(include_elements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S"),
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if exclude_elements contains over 100 entries.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                   exclude_elements = c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md"))
  )
})

test_that("post_element() fails if include_elements contains a non-periodic table element symbol.", {
  expect_error(
    post_element(include_elements = "J",
                   exclude_elements = c("Na", "K", "Fe"))
  )
})

test_that("post_element() fails if exclude_elements contains a non-periodic table element symbol.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                   exclude_elements = "J")
  )
})

test_that("post_element() fails if more than one complexity is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                     exclude_elements = c("Na", "K", "Fe"),
                     complexity = c("any", "single", "multiple"))
  )
})

test_that("post_element() fails if a wrong character string is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                     exclude_elements = c("Na", "K", "Fe"),
                     complexity = "something")
  )
})

test_that("post_element() fails if a numeric value is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                     exclude_elements = c("Na", "K", "Fe"),
                     complexity = 123)
  )
})


test_that("post_element() fails if a logical is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                     exclude_elements = c("Na", "K", "Fe"),
                     complexity = TRUE)
  )
})

test_that("post_element() fails if NULL is provided as isotopic.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                   exclude_elements = c("Na", "K", "Fe"),
                   complexity = "any", 
                   isotopic = NULL)
  )
})

test_that("post_element() fails if multiple isotopic are provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                   exclude_elements = c("Na", "K", "Fe"),
                   complexity = "any", 
                   isotopic = c("all", "labeled"))
  )
})

test_that("post_element() fails if a non-character isotopic is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                   exclude_elements = c("Na", "K", "Fe"),
                   complexity = "any", 
                   isotopic = 123)
  )
})

test_that("post_element() fails if a wrong isotopic is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                   exclude_elements = c("Na", "K", "Fe"),
                   complexity = "any", 
                   isotopic = "something")
  )
})

test_that("post_element() fails if more than one order_by is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                exclude_elements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                order_by = c("recordid", "massdefect"), order_direction = NULL)
  )
})

test_that("post_element() fails if a false order_by is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                exclude_elements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                order_by = "thewrongthing", order_direction = NULL)
  )
})

test_that("post_element() fails if a non-character order_by is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                exclude_elements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                order_by = 123, order_direction = NULL)
  )
})

test_that("post_element() fails if more than one order_direction is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                exclude_elements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                order_by = NULL, order_direction = c("ascending", "descending"))
  )
})

test_that("post_element() fails if a non-character order_direction is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                exclude_elements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                order_by = NULL, order_direction = 123)
  )
})

test_that("post_element() fails if a false order_direction is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                exclude_elements = c("Na", "K", "Fe"),
                complexity = "any", 
                isotopic = "any",
                order_by = NULL, order_direction = "thewrongthing")
  )
})

test_that("post_element() fails if no API key is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                 exclude_elements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 order_by = "recordId", order_direction = "ascending")
  )
})

test_that("post_element() fails if NULL is provided as API key.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                 exclude_elements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 order_by = "recordId", order_direction = "ascending",
                 apikey = NULL)
  )
})

test_that("post_element() fails if more than one API key is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                 exclude_elements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 order_by = "recordId", order_direction = "ascending",
                 apikey = c("API key one", "API key two"))
  )
})

test_that("post_element() fails if a numeric API key is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                 exclude_elements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 order_by = "recordId", order_direction = "ascending",
                 apikey = 1234567890)
  )
})

test_that("post_element() fails if a logical API key is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                 exclude_elements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 order_by = "recordId", order_direction = "ascending",
                 apikey = TRUE)
  )
})

test_that("post_element() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_element(include_elements = c("C", "H", "O"),
                 exclude_elements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 order_by = "recordId", order_direction = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"fe7fe60b-0b67-4b24-9d9b-1cf01b75f844\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_ELEMENT_URL" = web$url())

test_that("post_element() returns a proper response.", {
  expect_type(
    post_element(include_elements = c("C", "H", "O"),
                 exclude_elements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 order_by = "recordId", order_direction = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz123456",
                 coerce = TRUE),
    "list"
  )
})

test_that("post_element() returns a proper response.", {
  expect_type(
    post_element(include_elements = c("C", "H", "O"),
                 exclude_elements = c("Na", "K", "Fe"),
                 complexity = "any", 
                 isotopic = "any",
                 order_by = "recordId", order_direction = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz123456",
                 simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("POST_ELEMENT_URL")

web$stop()
