context("enkey")

test_that("default call adds a row-indexed column to use as key", {
  d <- data.frame(
    a = seq(10),
    b = sample(letters, 10),
    c = rnorm(10), stringsAsFactors = FALSE)
  dd <- enkey(d)
  keycol <- attr(dd, "key_column")
  expect_integer(dd[[keycol]])
  expect_equal(dd[[keycol]], seq(nrow(d)))
})

test_that("enkey works on single columns", {
  d <- data.frame(
    a = seq(10),
    b = sample(letters, 10),
    c = rnorm(10), stringsAsFactors = FALSE)

  da <- enkey(d, "a")
  expect_equal(da, d, check.attributes = FALSE)
  expect_equal(attr(da, "key_column"), "a")

  # 'real' columns can't be used as a key
  expect_error(enkey(d, "c"), "discrete")
})

test_that("enkey handles multi-column keys", {
  d <- data.frame(
    a = seq(10),
    b = sample(letters, 10),
    c = rnorm(10), stringsAsFactors = FALSE)

  dab <- enkey(d, c("a", "b"), "mykey")
  keycol <- attr(dab, "key_column")
  expect_equal(keycol, "mykey")
  expect_equal(dab[[keycol]], paste(d$a, d$b, sep = "_"))
})

test_that("unique key column added when an already existing column exists", {
  d <- data.frame(
    a = seq(10),
    b = sample(letters, 10),
    c = rnorm(10),
    fakekey = rnorm(10),
    stringsAsFactors = FALSE)

  dab <- expect_warning(enkey(d, c("a", "b"), "fakekey"), "already exists")
  keycol <- attr(dab, "key_column")
  expect_equal(make.names(keycol), keycol, info = "valid R variable name")
  expect_false(keycol %in% colnames(d))
  expect_equal(dab[[keycol]], paste(d$a, d$b, sep = "_"))
})

test_that("enkey fails on missing columns", {
  d <- data.frame(
    a = seq(10),
    b = sample(letters, 10),
    c = rnorm(10), stringsAsFactors = FALSE)

  expect_error(enkey(d, "bad"), "subset")
})
