#' Tag list of elements for the FreesearchR landing page
#'
#' @param i18n i18n function loaded in the UI
#'
#' @returns tag list
#'
landing_page_ui <- function(i18n) {
  tagList(
    # Header section
    div(
      class = "container-fluid py-4",
      # style = "background: linear-gradient(135deg, #1E4A8F 0%, #8A4FFF 100%); color: white;",
      div(
        class = "row align-items-center",
        div(
          class = "col-md-8",
          h1(i18n$t("Welcome to FreesearchR"), style = "font-weight: 700; margin-bottom: 10px;"),
          h4(i18n$t("A free data analysis tool for clinicians, students, and learners"),
             style = "font-weight: 300; opacity: 0.95;")
        ),
        div(
          class = "col-md-4 text-end",
          img(src = "FreesearchR-logo.png", style = "max-width: 200px; height: auto;")
        )
      )
    ),

    # Main content
    div(
      class = "container my-5",

      # Introduction text
      div(
        class = "row mb-5",
        div(
          class = "col-12 text-center",
          p(
            class = "lead",
            i18n$t("Start with FreesearchR for basic data evaluation and analysis."),
            i18n$t("When you need more advanced tools, you'll be better prepared to use R directly."),
            style = "font-size: 1.2rem; color: #555;"
          )
        )
      ),

      # Core Features Section
      h2(i18n$t("Core Features"), class = "text-center mb-4",
         style = "color: #1E4A8F; font-weight: 600;"),

      div(
        class = "row g-4 mb-5",

        # Import Data
        div(
          class = "col-md-4",
          div(
            class = "card h-100 shadow-sm hover-card",
            style = "border: none; transition: transform 0.2s;",
            div(
              class = "card-body text-center p-4",
              div(
                style = "font-size: 3rem; color: #1E4A8F; margin-bottom: 15px;",
                fa("file-import")
              ),
              h4(i18n$t("Import Data"), class = "card-title", style = "color: #2D2D42; font-weight: 600;"),
              p(
                class = "card-text",
                i18n$t("Load data from spreadsheets, REDCap servers, or try with sample data. Multiple sources supported for maximum flexibility.")
              )
            )
          )
        ),

        # Data Management
        div(
          class = "col-md-4",
          div(
            class = "card h-100 shadow-sm hover-card",
            style = "border: none;",
            div(
              class = "card-body text-center p-4",
              div(
                style = "font-size: 3rem; color: #1E4A8F; margin-bottom: 15px;",
                fa("pen-to-square")
              ),
              h4(i18n$t("Data Management"), class = "card-title", style = "color: #2D2D42; font-weight: 600;"),
              p(
                class = "card-text",
                i18n$t("Filter, modify, and create new variables. Prepare your data efficiently for analysis.")
              )
            )
          )
        ),

        # Descriptive Statistics
        div(
          class = "col-md-4",
          div(
            class = "card h-100 shadow-sm hover-card",
            style = "border: none;",
            div(
              class = "card-body text-center p-4",
              div(
                style = "font-size: 3rem; color: #1E4A8F; margin-bottom: 15px;",
                fa("magnifying-glass-chart")
              ),
              h4(i18n$t("Descriptive Statistics"), class = "card-title", style = "color: #2D2D42; font-weight: 600;"),
              p(
                class = "card-text",
                i18n$t("Evaluate data with descriptive analyses, inspect correlations, and review missing observations effectively.")
              )
            )
          )
        )
      ),

      # Advanced Features Section
      h3(i18n$t("Additional Capabilities"), class = "text-center mb-4 mt-5",
         style = "color: #2D2D42; font-weight: 500; font-size: 1.5rem;"),

      div(
        class = "row g-3 mb-5",
        div(
          class = "col-md-6",
          div(
            class = "card shadow-sm",
            style = "border-left: 4px solid #8A4FFF;",
            div(
              class = "card-body",
              h5(fa("chart-line"), " ", i18n$t("Data Visualization"), class = "card-title", style = "color: #2D2D42;"),
              p(class = "card-text small", i18n$t("Create simple, clean plots for quick insights and overview"))
            )
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "card shadow-sm",
            style = "border-left: 4px solid #8A4FFF;",
            div(
              class = "card-body",
              h5(fa("calculator"), " ", i18n$t("Regression Models"), class = "card-title", style = "color: #2D2D42;"),
              p(class = "card-text small", i18n$t("Build simple regression models for advanced analysis"))
            )
          )
        )
      ),

      # Export & Learning Section
      div(
        class = "row mb-5",
        div(
          class = "col-12",
          div(
            class = "card shadow",
            style = "background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); border: none;",
            div(
              class = "card-body p-4",
              h4(fa("download"), " ", i18n$t("Export & Learn"), class = "text-center mb-3", style = "color: #1E4A8F;"),
              div(
                class = "row text-center",
                div(
                  class = "col-md-4",
                  p(strong(i18n$t("Download Results")), br(), i18n$t("Export results directly to your text editor"))
                ),
                div(
                  class = "col-md-4",
                  p(strong(i18n$t("Get Modified Data")), br(), i18n$t("Save your processed datasets for later"))
                ),
                div(
                  class = "col-md-4",
                  p(strong(i18n$t("Reproducible Code")), br(), i18n$t("Learn and reproduce results in R"))
                )
              )
            )
          )
        )
      )#,

      # Footer links
    #   div(
    #     class = "row mt-5 pt-4 border-top",
    #     div(
    #       class = "col-md-4 text-center mb-3",
    #       a(
    #         href = "https://agdamsbo.github.io/FreesearchR/",
    #         target = "_blank",
    #         class = "btn btn-outline-primary",
    #         fa("book"), " ", i18n$t("Full Documentation")
    #       )
    #     ),
    #     div(
    #       class = "col-md-4 text-center mb-3",
    #       a(
    #         href = "https://redcap.au.dk/surveys/?s=JPCLPTXYDKFA8DA8",
    #         target = "_blank",
    #         class = "btn btn-outline-success",
    #         fa("comments"), " ", i18n$t("Share Feedback")
    #       )
    #     ),
    #     div(
    #       class = "col-md-4 text-center mb-3",
    #       a(
    #         href = "mailto:info@freesearchr.org",
    #         class = "btn btn-outline-info",
    #         fa("envelope"), " ", i18n$t("Contact Us")
    #       )
    #     )
    #   ),
    #
    #   # Translation notice
    #   div(
    #     class = "row mt-4",
    #     div(
    #       class = "col-12 text-center",
    #       p(
    #         class = "text-muted small",
    #         em(i18n$t("FreesearchR is available in multiple languages. To help with translations, please contact us at info@freesearchr.org"))
    #       )
    #     )
    #   )
    ),

    # Custom CSS for hover effects
    tags$style(HTML("
      .hover-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 .5rem 1rem rgba(0,0,0,.15)!important;
      }
    "))
  )
}

# Example usage in a Shiny app with nav_panel:
#
# ui <- page_navbar(
#   title = "FreesearchR",
#   theme = bs_theme(
#     version = 5,
#     bootswatch = "flatly",
#     primary = "#2C3E50",
#     base_font = font_google("Roboto")
#   ),
#   nav_panel(
#     title = i18n$t("Home"),
#     landing_page_ui(i18n)
#   ),
#   nav_panel(
#     title = i18n$t("Import Data"),
#     # Import data UI
#   ),
#   # ... other nav_panels
# )
