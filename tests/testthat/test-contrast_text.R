test_that("Contrasting works", {
  colors <- c("#F2F2F2", "blue","red","black","white","gray35")

  expect_snapshot(
    contrast_text(colors)
  )

  expect_snapshot(
    contrast_text(colors,light_text = "blue",dark_text = "grey10", method = "relative", threshold = .1)
  )

  expect_snapshot(
    contrast_text(colors,light_text = "blue",dark_text = "grey10", method = "perceived", threshold = .7)
  )
})
