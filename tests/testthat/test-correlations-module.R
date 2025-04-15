test_that("correlations module works", {
  testServer(data_correlations_server, args=list(data = mtcars,cutoff = shiny::reactive(.8)), {
    expect_equal(nchar(output$suggest), 281)
    expect_equal(class(output$correlation_plot),"list")
    expect_equal(length(output$correlation_plot),5)
  })

  expect_snapshot(
    correlation_pairs(data = gtsummary::trial,threshold = .2)
    )

  expect_snapshot(
  sentence_paste(letters[1:8])
  )

})

