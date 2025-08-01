#' FreesearchR UI elements list
#'
#' @param selection specify element to output
#'
#' @returns Shinu UI elements
#' @export
#'
ui_elements <- function(selection) {
  out <- list(
    ##############################################################################
    #########
    #########  Home panel
    #########
    ##############################################################################
    "home" = bslib::nav_panel(
      title = "FreesearchR",
      # title = shiny::div(htmltools::img(src="FreesearchR-logo-white-nobg-h80.png")),
      icon = shiny::icon("house"),
      shiny::fluidRow(
        ## On building the dev-version for shinyapps.io, the dev_banner() is redefined
        ## Default just output "NULL"
        ## This could probably be achieved more legantly, but this works.
        dev_banner(),
        shiny::column(width = 2),
        shiny::column(
          width = 8,
          shiny::markdown(readLines("www/intro.md")),
          shiny::column(width = 2)
        )
      )
    ),
    ##############################################################################
    #########
    #########  Import panel
    #########
    ##############################################################################
    "import" = bslib::nav_panel(
      title = "Get started",
      icon = shiny::icon("play"),
      value = "nav_import",
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(
          width = 8,
          shiny::h4("Choose your data source"),
          shiny::br(),
          # shiny::uiOutput(outputId = "source"),
          shinyWidgets::radioGroupButtons(
            inputId = "source",
            selected = "file",
            choices = c(
              "File upload" = "file",
              "REDCap server export" = "redcap",
              "Local or sample data" = "env"
            ),
            size = "lg"
          ),
          shiny::tags$script('document.querySelector("#source div").style.width = "100%"'),
          shiny::helpText("Upload a file from your device, get data directly from REDCap or select a sample data set for testing from the app."),
          shiny::br(),
          shiny::br(),
          shiny::conditionalPanel(
            condition = "input.source=='file'",
            import_file_ui(
              id = "file_import",
              layout_params = "dropdown",
              # title = "Choose a datafile to upload",
              file_extensions = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".rds", ".ods", ".dta")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.source=='redcap'",
            shinyWidgets::alert(
              id = "redcap-warning",
              status = "info",
              shiny::tags$h2(shiny::markdown("Careful with sensitive data")),
              shiny::tags$p("The", shiny::tags$i(shiny::tags$b("FreesearchR")), "app only stores data for analyses, but please only use with sensitive data when running locally.", "", shiny::tags$a("Read more here", href = "https://agdamsbo.github.io/FreesearchR/#run-locally-on-your-own-machine"), "."),
              dismissible = TRUE
            ),
            m_redcap_readUI(
              id = "redcap_import",
              title = ""
            )
          ),
          shiny::conditionalPanel(
            condition = "input.source=='env'",
            import_globalenv_ui(id = "env", title = NULL)
          ),
          # shiny::conditionalPanel(
          #   condition = "input.source=='redcap'",
          #   DT::DTOutput(outputId = "redcap_prev")
          # ),
          shiny::conditionalPanel(
            condition = "output.data_loaded == true",
            shiny::br(),
            shiny::actionButton(
              inputId = "modal_initial_view",
              label = "Quick overview",
              width = "100%",
              icon = shiny::icon("binoculars"),
              disabled = FALSE
            ),
            shiny::br(),
            shiny::br(),
            shiny::h5("Select variables for final import"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::p("Exclude incomplete variables:"),
                shiny::br(),
                shinyWidgets::noUiSliderInput(
                  inputId = "complete_cutoff",
                  label = NULL,
                  update_on = "end",
                  min = 0,
                  max = 100,
                  step = 5,
                  value = 30,
                  format = shinyWidgets::wNumbFormat(decimals = 0),
                  color = datamods:::get_primary_color()
                ),
                shiny::helpText("Only include variables missing less observations than the specified percentage."),
                shiny::br()
              ),
              shiny::column(
                width = 6,
                shiny::p("Manual selection:"),
                shiny::br(),
                shiny::uiOutput(outputId = "import_var"),
                shiny::br()
              )
            ),
            shiny::uiOutput(outputId = "data_info_import", inline = TRUE),
            shiny::br(),
            shiny::br(),
            shiny::actionButton(
              inputId = "act_start",
              label = "Start",
              width = "100%",
              icon = shiny::icon("play"),
              disabled = TRUE
            ),
            shiny::helpText('After importing, hit "Start" or navigate to the desired tab.'),
            shiny::br(),
            shiny::br()
          ),
          shiny::column(width = 2)
        ),
        shiny::br(),
        shiny::br()
      )
    ),
    ##############################################################################
    #########
    #########  Data overview panel
    #########
    ##############################################################################
    "prepare" = bslib::nav_menu(
      title = "Prepare",
      icon = shiny::icon("pen-to-square"),
      value = "nav_prepare",
      bslib::nav_panel(
        title = "Overview",
        icon = shiny::icon("eye"),
        value = "nav_prepare_overview",
        tags$h3("Overview and filtering"),
        fluidRow(
          shiny::column(
            width = 9,
            shiny::uiOutput(outputId = "data_info", inline = TRUE),
            shiny::tags$p(
              "Below is a short summary table, on the right you can click to visualise data classes or browse data and create different data filters."
            )
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              inputId = "modal_visual_overview",
              label = "Visual overview",
              width = "100%",
              disabled = TRUE
            ),
            shiny::br(),
            shiny::br(),
            shiny::actionButton(
              inputId = "modal_browse",
              label = "Browse data",
              width = "100%",
              disabled = TRUE
            ),
            shiny::br(),
            shiny::br()
          )
        ),
        fluidRow(
          shiny::column(
            width = 9,
            data_summary_ui(id = "data_summary"),
            shiny::br(),
            shiny::br(),
            shiny::br(),
            shiny::br(),
            shiny::br()
          ),
          shiny::column(
            width = 3,
            shiny::tags$h6("Filter data types"),
            shiny::uiOutput(
              outputId = "column_filter"
            ),
            shiny::helpText("Read more on how ", tags$a(
              "data types",
              href = "https://agdamsbo.github.io/FreesearchR/articles/data-types.html",
              target = "_blank",
              rel = "noopener noreferrer"
            ), " are defined."),
            shiny::br(),
            shiny::br(),
            shiny::tags$h6("Filter observations"),
            shiny::tags$p("Filter on observation level"),
            IDEAFilter::IDEAFilter_ui("data_filter"),
            shiny::br(),
            shiny::br()
          )
        ),
        shiny::br(),
        shiny::br(),
        shiny::br()
      ),
      bslib::nav_panel(
        title = "Modify",
        icon = shiny::icon("file-pen"),
        tags$h3("Subset, rename and convert variables"),
        fluidRow(
          shiny::column(
            width = 9,
            shiny::tags$p(
              shiny::markdown("Below, are several options for simple data manipulation like update variables by renaming, creating new labels (for nicer tables in the report) and changing variable classes (numeric, factor/categorical etc.)."),
              shiny::markdown("There are more advanced options to modify factor/categorical variables as well as create new factor from a continous variable or new variables with *R* code. At the bottom you can restore the original data."),
              shiny::markdown("Please note that data modifications are applied before any filtering.")
            )
          )
        ),
        update_variables_ui("modal_variables"),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::tags$h4("Advanced data manipulation"),
        shiny::tags$p("Below options allow more advanced varaible manipulations."),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = "modal_update",
              label = "Reorder factor levels",
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText("Reorder the levels of factor/categorical variables."),
            shiny::tags$br(),
            shiny::tags$br()
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = "modal_cut",
              label = "New factor",
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText("Create factor/categorical variable from a continous variable (number/date/time)."),
            shiny::tags$br(),
            shiny::tags$br()
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = "modal_column",
              label = "New variable",
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText(shiny::markdown("Create a new variable/column based on an *R*-expression.")),
            shiny::tags$br(),
            shiny::tags$br()
          )
        ),
        tags$h4("Compare modified data to original"),
        shiny::tags$br(),
        shiny::tags$p(
          "Raw print of the original vs the modified data."
        ),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::tags$b("Original data:"),
            shiny::verbatimTextOutput("original_str")
          ),
          shiny::column(
            width = 6,
            shiny::tags$b("Modified data:"),
            shiny::verbatimTextOutput("modified_str")
          )
        ),
        shiny::tags$br(),
        shiny::actionButton(
          inputId = "data_reset",
          label = "Restore original data",
          width = "100%"
        ),
        shiny::tags$br(),
        shiny::helpText("Reset to original imported dataset. Careful! There is no un-doing."),
        shiny::tags$br()
      )
      # )
    ),
    ##############################################################################
    #########
    #########  Descriptive analyses panel
    #########
    ##############################################################################
    "describe" =
      bslib::nav_menu(
        title = "Evaluate",
        icon = shiny::icon("magnifying-glass-chart"),
        value = "nav_describe",
        # id = "navdescribe",
        # bslib::navset_bar(
        #   title = "",
        bslib::nav_panel(
          title = "Characteristics",
          icon = bsicons::bs_icon("table"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              shiny::uiOutput(outputId = "data_info_nochar", inline = TRUE),
              bslib::accordion(
                open = "acc_chars",
                multiple = FALSE,
                bslib::accordion_panel(
                  open = TRUE,
                  value = "acc_chars",
                  title = "Settings",
                  icon = bsicons::bs_icon("table"),
                  shiny::uiOutput("strat_var"),
                  shiny::helpText("Only factor/categorical variables are available for stratification. Go back to the 'Prepare' tab to reclass a variable if it's not on the list."),
                  shiny::conditionalPanel(
                    condition = "input.strat_var!='none'",
                    shiny::radioButtons(
                      inputId = "add_p",
                      label = "Compare strata?",
                      selected = "no",
                      inline = TRUE,
                      choices = list(
                        "No" = "no",
                        "Yes" = "yes"
                      )
                    ),
                    shiny::helpText("Option to perform statistical comparisons between strata in baseline table.")
                  ),
                  shiny::br(),
                  shiny::br(),
                  shiny::actionButton(
                    inputId = "act_eval",
                    label = "Evaluate",
                    width = "100%",
                    icon = shiny::icon("calculator"),
                    disabled = TRUE
                  )
                )
              )
            ),
            gt::gt_output(outputId = "table1")
          )
        ),
        bslib::nav_panel(
          title = "Correlations",
          icon = bsicons::bs_icon("bounding-box"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              # shiny::uiOutput(outputId = "data_info_nochar", inline = TRUE),
              bslib::accordion(
                open = "acc_chars",
                multiple = FALSE,
                bslib::accordion_panel(
                  value = "acc_cor",
                  title = "Correlations",
                  icon = bsicons::bs_icon("bounding-box"),
                  shiny::uiOutput("outcome_var_cor"),
                  shiny::helpText("To avoid evaluating the correlation of the outcome variable, this can be excluded from the plot or select 'none'."),
                  shiny::br(),
                  shinyWidgets::noUiSliderInput(
                    inputId = "cor_cutoff",
                    label = "Correlation cut-off",
                    min = 0,
                    max = 1,
                    step = .01,
                    value = .8,
                    format = shinyWidgets::wNumbFormat(decimals = 2),
                    color = datamods:::get_primary_color()
                  ),
                  shiny::helpText("Set the cut-off for considered 'highly correlated'.")
                )
              )
            ),
            data_correlations_ui(id = "correlations", height = 600)
          )
        ),
        bslib::nav_panel(
          title = "Missings",
          icon = bsicons::bs_icon("x-circle"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                open = "acc_chars",
                multiple = FALSE,
                bslib::accordion_panel(
                  vlaue = "acc_mis",
                  title = "Missings",
                  icon = bsicons::bs_icon("x-circle"),
                  shiny::uiOutput("missings_var"),
                  shiny::helpText("To consider if data is missing by random, choose the outcome/dependent variable, if it has any missings to evaluate if there is a significant difference across other variables depending on missing data or not.")
                )
              )
            ),
            data_missings_ui(id = "missingness")
          )
        )
      ),
    ##############################################################################
    #########
    #########  Download panel
    #########
    ##############################################################################
    "visuals" = do.call(
      bslib::nav_panel,
      c(
        list(
          title = "Visuals",
          icon = shiny::icon("chart-line"),
          value = "nav_visuals"
        ),
        data_visuals_ui("visuals")
      )
      # do.call(
      #   bslib::navset_bar,
      #     data_visuals_ui("visuals")#,
      # c(

      # )
      # )
    ),
    ##############################################################################
    #########
    #########  Regression analyses panel
    #########
    ##############################################################################
    "analyze" =
      bslib::nav_panel(
        title = "Regression",
        icon = shiny::icon("calculator"),
        value = "nav_analyses",
        do.call(
          bslib::navset_card_tab,
          regression_ui("regression")
        )
      ),
    ##############################################################################
    #########
    #########  Download panel
    #########
    ##############################################################################
    "download" =
      bslib::nav_panel(
        title = "Download",
        icon = shiny::icon("download"),
        value = "nav_download",
        shiny::fluidRow(
          shiny::column(width = 2),
          shiny::column(
            width = 8,
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h4("Report"),
                shiny::helpText("Choose your favourite output file format for further work, and download, when the analyses are done."),
                shiny::br(),
                shiny::br(),
                shiny::selectInput(
                  inputId = "output_type",
                  label = "Output format",
                  selected = NULL,
                  choices = list(
                    "MS Word" = "docx",
                    "LibreOffice" = "odt"
                    # ,
                    # "PDF" = "pdf",
                    # "All the above" = "all"
                  )
                ),
                shiny::br(),
                # Button
                shiny::downloadButton(
                  outputId = "report",
                  label = "Download report",
                  icon = shiny::icon("download")
                )
                # shiny::helpText("If choosing to output to MS Word, please note, that when opening the document, two errors will pop-up. Choose to repair and choose not to update references. The issue is being worked on. You can always choose LibreOffice instead."),
              ),
              shiny::column(
                width = 6,
                shiny::h4("Data"),
                shiny::helpText("Choose your favourite output data format to download the modified data."),
                shiny::br(),
                shiny::br(),
                shiny::selectInput(
                  inputId = "data_type",
                  label = "Data format",
                  selected = NULL,
                  choices = list(
                    "R" = "rds",
                    "stata" = "dta",
                    "CSV" = "csv"
                  )
                ),
                shiny::helpText("No metadata is saved when exporting to csv."),
                shiny::br(),
                shiny::br(),
                # Button
                shiny::downloadButton(
                  outputId = "data_modified",
                  label = "Download data",
                  icon = shiny::icon("download")
                )
              )
            ),
            shiny::br(),
            shiny::br(),
            shiny::h4("Code snippets"),
            shiny::tags$p("Below are the code bits used to create the final data set and the main analyses."),
            shiny::tags$p("This can be used as a starting point for learning to code and for reproducibility."),
            shiny::tagList(
              lapply(
                paste0("code_", c(
                  "import", "format", "data", "variables", "filter", "table1", "univariable", "multivariable"
                )),
                \(.x)shiny::htmlOutput(outputId = .x)
              )
            ),
            shiny::tags$br(),
            shiny::br()
          ),
          shiny::column(width = 2)
        )
      ),
    ##############################################################################
    #########
    #########  Feedback link
    #########
    ##############################################################################
    "feedback" = bslib::nav_item(
      # shiny::img(shiny::icon("book")),
      shiny::tags$a(
        href = "https://redcap.au.dk/surveys/?s=JPCLPTXYDKFA8DA8",
        "Feedback", shiny::icon("arrow-up-right-from-square"),
        target = "_blank",
        rel = "noopener noreferrer"
      )
    ),
    ##############################################################################
    #########
    #########  Documentation link
    #########
    ##############################################################################
    "docs" = bslib::nav_item(
      # shiny::img(shiny::icon("book")),
      shiny::tags$a(
        href = "https://agdamsbo.github.io/FreesearchR/",
        "Docs", shiny::icon("arrow-up-right-from-square"),
        target = "_blank",
        rel = "noopener noreferrer"
      )
    )
    #   bslib::nav_panel(
    #   title = "Documentation",
    #   # shiny::tags$iframe("www/docs.html", height=600, width=535),
    #   shiny::htmlOutput("docs_file"),
    #   shiny::br()
    # )
  )
if (!is.null(selection)){
  out[[selection]]
} else {
  out
}

  }


# ls <- list("home"=1:4,
#            "test"=1:4)
#
