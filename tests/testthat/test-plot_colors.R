library(testthat)

# ── Helpers ───────────────────────────────────────────────────────────────────

is_hex_color <- function(x) {
  all(grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x))
}

# ── Input validation ──────────────────────────────────────────────────────────

test_that("n must be a single positive integer", {
  expect_error(generate_colors(0),       "`n` must be a single positive integer")
  expect_error(generate_colors(-1),      "`n` must be a single positive integer")
  expect_error(generate_colors(1.5),     "`n` must be a single positive integer")
  expect_error(generate_colors(c(2, 3)), "`n` must be a single positive integer")
  expect_error(generate_colors("5"),     "`n` must be a single positive integer")
})

test_that("palette must be a single character string or function", {
  expect_error(generate_colors(5, 123),         "`palette` must be a single character string")
  expect_error(generate_colors(5, c("a", "b")), "`palette` must be a single character string")
})

test_that("unknown palette falls back to hcl.colors with a message", {
  expect_message(
    result <- generate_colors(5, "notapalette"),
    "Unknown palette: 'notapalette'"
  )
  expect_equal(length(result), 5)
  expect_true(is_hex_color(result))
})

# ── Return type and length ────────────────────────────────────────────────────

test_that("output is a character vector of correct length for each palette family", {
  palettes <- c("viridis", "plasma", "rainbow", "heat", "terrain", "topo", "Set1", "Blues")
  for (pal in palettes) {
    result <- generate_colors(5, pal)
    expect_true(is.character(result), label = paste0("is.character [", pal, "]"))
    expect_equal(length(result), 5,   label = paste0("length == 5  [", pal, "]"))
  }
})

test_that("output colors are valid hex codes", {
  palettes <- c("viridis", "magma", "rainbow", "hcl", "Set1", "Blues")
  for (pal in palettes) {
    result <- generate_colors(5, pal)
    expect_true(is_hex_color(result), label = paste0("hex check [", pal, "]"))
  }
})

test_that("n = 1 works for all palette families", {
  expect_equal(length(generate_colors(1, "viridis")), 1)
  expect_equal(length(generate_colors(1, "rainbow")), 1)
  expect_equal(length(generate_colors(1, "Set1")),    1)
})

# ── viridisLite ───────────────────────────────────────────────────────────────

test_that("all viridisLite palettes return correct length", {
  viridis_palettes <- c("viridis", "magma", "plasma", "inferno",
                        "cividis", "mako", "rocket", "turbo")
  for (pal in viridis_palettes) {
    expect_equal(length(generate_colors(6, pal)), 6, label = paste0("length [", pal, "]"))
  }
})

test_that("viridisLite palette names are case-insensitive", {
  expect_equal(generate_colors(5, "VIRIDIS"), generate_colors(5, "viridis"))
  expect_equal(generate_colors(5, "Plasma"),  generate_colors(5, "plasma"))
})

test_that("extra args are forwarded to viridisLite (direction)", {
  fwd <- generate_colors(5, "viridis", direction =  1)
  rev <- generate_colors(5, "viridis", direction = -1)
  expect_false(identical(fwd, rev))
})

# ── grDevices ─────────────────────────────────────────────────────────────────

test_that("grDevices palettes return correct length", {
  for (pal in c("hcl", "rainbow", "heat", "terrain", "topo")) {
    expect_equal(length(generate_colors(7, pal)), 7, label = paste0("length [", pal, "]"))
  }
})

test_that("grDevices palette names are case-insensitive", {
  expect_equal(generate_colors(5, "Rainbow"), generate_colors(5, "rainbow"))
  expect_equal(generate_colors(5, "HEAT"),    generate_colors(5, "heat"))
})

# ── RColorBrewer ──────────────────────────────────────────────────────────────

test_that("RColorBrewer returns exactly n colors for any n >= 1", {
  expect_equal(length(generate_colors(1,  "Set1")), 1)   # below brewer min, slices
  expect_equal(length(generate_colors(2,  "Set1")), 2)   # below brewer min, slices
  expect_equal(length(generate_colors(3,  "Set1")), 3)   # at brewer min
  expect_equal(length(generate_colors(9,  "Set1")), 9)   # at brewer max
  expect_equal(length(generate_colors(15, "Set1")), 15)  # above brewer max, interpolates
})

test_that("RColorBrewer n < 3 does not warn or error", {
  expect_no_warning(generate_colors(1, "Set1"))
  expect_no_warning(generate_colors(2, "Blues"))
})

test_that("RColorBrewer output is valid hex for all n", {
  expect_true(is_hex_color(generate_colors(1,  "Blues")))
  expect_true(is_hex_color(generate_colors(9,  "Blues")))
  expect_true(is_hex_color(generate_colors(20, "Blues")))
})

test_that("RColorBrewer sequential and diverging palettes work", {
  expect_equal(length(generate_colors(5, "Blues")), 5)
  expect_equal(length(generate_colors(5, "RdBu")),  5)
})

# ── Function passthrough ──────────────────────────────────────────────────────

test_that("palette accepts a function directly", {
  result <- generate_colors(5, viridisLite::viridis)
  expect_equal(length(result), 5)
  expect_true(is_hex_color(result))
})

test_that("palette accepts an anonymous function", {
  result <- generate_colors(5, \(n) rep("#FF0000FF", n))
  expect_equal(result, rep("#FF0000FF", 5))
})

test_that("error message mentions function as valid input type", {
  expect_error(generate_colors(5, 123), "single character string or a function")
})

# ── Fallback ──────────────────────────────────────────────────────────────────

test_that("fallback message includes available options", {
  expect_message(generate_colors(5, "notapalette"), "viridisLite")
  expect_message(generate_colors(5, "notapalette"), "RColorBrewer")
})

test_that("fallback returns correct length and valid hex colors", {
  result <- suppressMessages(generate_colors(8, "notapalette"))
  expect_equal(length(result), 8)
  expect_true(is_hex_color(result))
})
