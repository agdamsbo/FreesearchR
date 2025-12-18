footer_ui <- function(i18n) {
  tagList(
    shiny::tags$footer(
      style = "background-color: #14131326; padding: 4px; text-align: center; bottom: 0; width: 100%;",
      shiny::p(
        style = "margin: 1",
        i18n$t("Data is only stored for analyses and deleted when the app is closed.")
      ),
      shiny::p(
        style = "margin: 1",
        i18n$t("Run the FreesearchR app locally when working with sensitive data."), shiny::tags$a(i18n$t("(Read more)"), href = "https://agdamsbo.github.io/FreesearchR/#run-locally-on-your-own-machine", target = "_blank", rel = "noopener noreferrer")
      ),
      # shiny::p(
      #   style = "margin: 1; color: #888;",
      div(
        style = "display: inline-flex; align-items: center; gap: 1px;",
        shiny::tags$a(i18n$t("Documentation"), href = "https://agdamsbo.github.io/FreesearchR/", target = "_blank", rel = "noopener noreferrer"), " | ", div(
          style = "display: inline-block;",
          class = c("smart-dropdown", "text-select"),
          shiny::uiOutput(outputId = "language_select")
        ), " | ", shiny::tags$a(i18n$t("Feedback"), href = "https://redcap.au.dk/surveys/?s=JPCLPTXYDKFA8DA8", target = "_blank", rel = "noopener noreferrer")
      ),
      br(),
      p(
        style = "display: inline-flex; align-items: center; gap: 1px;",
        hosted_version(), " | ", shiny::tags$a(i18n$t("License: AGPLv3"), href = "https://github.com/agdamsbo/FreesearchR/blob/main/LICENSE.md", target = "_blank", rel = "noopener noreferrer"), " | ", shiny::tags$a(i18n$t("Source"), href = "https://github.com/agdamsbo/FreesearchR/", target = "_blank", rel = "noopener noreferrer"),
      )
    )
  )
}
