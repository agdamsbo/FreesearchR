test_that("Create columnSelectInput", {
  library(shiny)
  ui <- shiny::fluidPage(
    shiny::uiOutput("x"),
    shiny::uiOutput("out")
  )
  server <- function(input, output, session) {
    library(FreesearchR)
    output$x <-
      shiny::renderUI({
      columnSelectInput(inputId = "x",selected = "mpg",label = "X",data = mtcars)
      })

    output$out <- renderText({
      # req(input$x)
      input$x
      })
  }

  # shinyApp(ui,server)

  testServer(server, {
    session$setInputs(x = "cyl")
    expect_equal(output$out, "cyl")

    session$setInputs(x = "mpg")
    expect_equal(output$out, "mpg")
  })

  server <- function(input, output, session) {
    library(FreesearchR)
    output$x <-
      shiny::renderUI({
        columnSelectInput(inputId = "x",label = "X",data = gtsummary::trial)
      })

    output$out <- renderText({
      # req(input$x)
      input$x
    })
  }

  # shinyApp(ui,server)

  testServer(server, {
    session$setInputs(x = "trt")
    expect_equal(output$out, "trt")

    session$setInputs(x = "stage")
    expect_equal(output$out, "stage")
  })

})

test_that("Create columnSelectInput", {
  library(shiny)
  ui <- shiny::fluidPage(
    shiny::uiOutput("x"),
    shiny::uiOutput("out")
  )
  server <- function(input, output, session) {
    library(FreesearchR)
    output$x <-
      shiny::renderUI({
        vectorSelectInput(inputId = "x",choices = setNames(names(mtcars),seq_len(ncol(mtcars))),label = "X")
      })

    output$out <- renderText({
      # req(input$x)
      input$x
    })
  }

  # shinyApp(ui,server)

  testServer(server, {
    session$setInputs(x = "cyl")
    expect_equal(output$out, "cyl")

    session$setInputs(x = "mpg")
    expect_equal(output$out, "mpg")
  })
})
