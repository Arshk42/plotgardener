test_that("plotGenomeLabel errors when chromstart/chromend are not integer basepairs", {
  skip_if_not_installed("plotgardener")
  
# Need a page
  plotgardener::pageCreate(width = 6, height = 10, showGuides = FALSE)
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 1.5,
      chromend = 2000,
      scale = "bp",
      x = 1, y = 1, length = 3,
      sequence = FALSE
    ),
    "chromstart.*integer|integer.*chromstart|basepairs",
    fixed = FALSE
  )
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 1000,
      chromend = 2000.25,
      scale = "bp",
      x = 1, y = 1.5, length = 3,
      sequence = FALSE
    ),
    "chromend.*integer|integer.*chromend|basepairs",
    fixed = FALSE
  )
})

test_that("plotGenomeLabel does not error for valid integer basepairs", {
  skip_if_not_installed("plotgardener")
  
  plotgardener::pageCreate(width = 6, height = 10, showGuides = FALSE)
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 123456789,
      chromend = 223456789,
      scale = "Mb",
      x = 1, y = 2, length = 3,
      sequence = FALSE
    ),
    NA
  )
})

test_that("plotGenomeLabel digits validation (only when digits is provided)", {
  skip_if_not_installed("plotgardener")
  
  plotgardener::pageCreate(width = 6, height = 10, showGuides = FALSE)
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 1000,
      chromend = 2000,
      scale = "Mb",
      x = 1, y = 3, length = 3,
      digits = -1,
      sequence = FALSE
    ),
    "digits",
    fixed = FALSE
  )
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 1000,
      chromend = 2000,
      scale = "Mb",
      x = 1, y = 3.5, length = 3,
      digits = 1.5,
      sequence = FALSE
    ),
    "digits",
    fixed = FALSE
  )
})

test_that("plotGenomeLabel scale validation errors on invalid scale", {
  skip_if_not_installed("plotgardener")
  
  plotgardener::pageCreate(width = 6, height = 10, showGuides = FALSE)
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 1000,
      chromend = 2000,
      scale = "Gb",   # invalid
      x = 1, y = 4, length = 3,
      sequence = FALSE
    ),
    "Invalid.*scale|scale",
    fixed = FALSE
  )
})

test_that("plotGenomeLabel requires x, y, chrom, and length", {
  skip_if_not_installed("plotgardener")
  plotgardener::pageCreate(width = 6, height = 10, showGuides = FALSE)
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 1000,
      chromend = 2000,
      scale = "bp",
      y = 1, length = 3,
      sequence = FALSE
    ),
    "argument \"x\" is missing",
    fixed = TRUE
  )
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 1000,
      chromend = 2000,
      scale = "bp",
      x = 1, length = 3,
      sequence = FALSE
    ),
    "argument \"y\" is missing",
    fixed = TRUE
  )
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chromstart = 1000,
      chromend = 2000,
      scale = "bp",
      x = 1, y = 1, length = 3,
      sequence = FALSE
    ),
    "argument \"chrom\" is missing",
    fixed = TRUE
  )
  
  expect_error(
    plotgardener::plotGenomeLabel(
      chrom = "chr1",
      chromstart = 1000,
      chromend = 2000,
      scale = "bp",
      x = 1, y = 1,
      sequence = FALSE
    ),
    "argument \"length\" is missing",
    fixed = TRUE
  )
})