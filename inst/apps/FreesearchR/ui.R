# ns <- NS(id)



ui_elements <- list(
  ##############################################################################
  #########
  #########  Home panel
  #########
  ##############################################################################
  "home" = bslib::nav_panel(
    title = "FreesearchR",
    shiny::fluidRow(
      shiny::column(width = 2),
      shiny::column(
        width = 8,
        shiny::markdown(readLines("www/intro.md")),
        shiny::column(width = 2)
      )
    ),
    icon = shiny::icon("home")
  ),
  ##############################################################################
  #########
  #########  Import panel
  #########
  ##############################################################################
  "import" = bslib::nav_panel(
    title = "Import",
    shiny::fluidRow(
      shiny::column(width = 2),
      shiny::column(
        width = 8,
        shiny::h4("Choose your data source"),
        shiny::br(),
        shinyWidgets::radioGroupButtons(
          inputId = "source",
          selected = "env",
          choices = c(
            "File upload" = "file",
            "REDCap server export" = "redcap",
            "Local or sample data" = "env"
          ),
          width = "100%"
        ),
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
          m_redcap_readUI(
            id = "redcap_import",
            title = ""
          )
        ),
        shiny::conditionalPanel(
          condition = "input.source=='env'",
          import_globalenv_ui(id = "env", title = NULL)
        ),
        shiny::conditionalPanel(
          condition = "input.source=='redcap'",
          DT::DTOutput(outputId = "redcap_prev")
        ),
        shiny::conditionalPanel(
          condition = "output.data_loaded == true",
          shiny::br(),
          shiny::br(),
          shiny::h5("Specify variables to include"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::br(),
              shiny::p("Filter by completeness threshold and manual selection:"),
              shiny::br(),
              shiny::br()
            ),
            shiny::column(
              width = 6,
              shinyWidgets::noUiSliderInput(
                inputId = "complete_cutoff",
                label = NULL,
                update_on = "end",
                min = 0,
                max = 100,
                step = 5,
                value = 70,
                format = shinyWidgets::wNumbFormat(decimals = 0),
                color = datamods:::get_primary_color()
              ),
              shiny::helpText("Exclude variables with completeness below the specified percentage."),
              shiny::br(),
              shiny::br(),
              shiny::uiOutput(outputId = "import_var"),
              shiny::uiOutput(outputId = "data_info_import", inline = TRUE)
            )
          )
        ),
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
        shiny::br(),
        shiny::column(width = 2)
      )
    )
  ),
  ##############################################################################
  #########
  #########  Data overview panel
  #########
  ##############################################################################
  "overview" =
  # bslib::nav_panel_hidden(
    bslib::nav_panel(
      # value = "overview",
      title = "Data",
      bslib::navset_bar(
        fillable = TRUE,
        bslib::nav_panel(
          title = "Overview",
          tags$h3("Overview and filtering"),
          fluidRow(
            shiny::column(
              width = 9,
              shiny::uiOutput(outputId = "data_info", inline = TRUE),
              shiny::tags$p(
                "Below is a short summary table, on the right you can click to browse data and create data filters."
              )
            )
          ),
          fluidRow(
            shiny::column(
              width = 9,
              data_summary_ui(id = "data_summary")
            ),
            shiny::column(
              width = 3,
              shiny::actionButton(
                inputId = "modal_browse",
                label = "Browse data",
                width = "100%",
                disabled = TRUE
              ),
              shiny::tags$br(),
              shiny::tags$br(),
              shiny::uiOutput(outputId = "column_filter"),
              shiny::helpText("Variable ", tags$a(
                "data type",
                href = "https://agdamsbo.github.io/FreesearchR/articles/data-types.html",
                target = "_blank",
                rel = "noopener noreferrer"
              ), " filtering."),
              shiny::tags$br(),
              shiny::tags$br(),
              IDEAFilter::IDEAFilter_ui("data_filter"),
              shiny::helpText("Observations level filtering."),
              shiny::tags$br(),
              shiny::tags$br()
            )
          ),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$br()
        ),
        bslib::nav_panel(
          title = "Modify",
          tags$h3("Subset, rename and convert variables"),
          fluidRow(
            shiny::column(
              width = 9,
              shiny::tags$p(
                shiny::markdown("Below, are several options for simple data manipulation like update variables by renaming, creating new labels (for nicer tables in the report) and changing variable classes (numeric, factor/categorical etc.)."),
                shiny::markdown("There are also more advanced options to modify factor/categorical variables as well as create new factor from a continous variable or new variables with *R* code. At the bottom you can restore the original data."),
                shiny::markdown("Please note that data modifications are applied before any data or variable filtering is applied.")
              )
            )
          ),
          # shiny::tags$br(),
          update_variables_ui("modal_variables"),
          shiny::tags$br(),
          shiny::tags$br(),
          tags$h4("Advanced data manipulation"),
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
              shiny::tags$br()
            )
          ),
          shiny::tags$br(),
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
              # verbatimTextOutput("original"),
              shiny::verbatimTextOutput("original_str")
            ),
            shiny::column(
              width = 6,
              shiny::tags$b("Modified data:"),
              # verbatimTextOutput("modified"),
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
      )
    ),
  ##############################################################################
  #########
  #########  Descriptive analyses panel
  #########
  ##############################################################################
  "describe" =
    bslib::nav_panel(
      title = "Evaluate",
      id = "navdescribe",
      bslib::navset_bar(
        title = "",
        sidebar = bslib::sidebar(
          shiny::uiOutput(outputId = "data_info_nochar", inline = TRUE),
          bslib::accordion(
            open = "acc_chars",
            multiple = FALSE,
            bslib::accordion_panel(
              value = "acc_chars",
              title = "Characteristics",
              icon = bsicons::bs_icon("table"),
              shiny::uiOutput("strat_var"),
              shiny::helpText("Only factor/categorical variables are available for stratification. Go back to the 'Data' tab to reclass a variable if it's not on the list."),
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
            ),
            bslib::accordion_panel(
              vlaue = "acc_cor",
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
        bslib::nav_panel(
          title = "Characteristics",
          gt::gt_output(outputId = "table1")
        ),
        bslib::nav_panel(
          title = "Correlations",
          data_correlations_ui(id = "correlations", height = 600)
        )
      )
    ),
  ##############################################################################
  #########
  #########  Download panel
  #########
  ##############################################################################
  "visuals" = bslib::nav_panel(
    title = "Visuals",
    id = "navvisuals",
    do.call(
      bslib::navset_bar,
      c(
        data_visuals_ui("visuals"),
        shiny::tagList(
          bslib::nav_spacer(),
          bslib::nav_panel(
            title = "Notes",
            shiny::fluidRow(
              shiny::column(width = 2),
              shiny::column(
                width = 8,
                shiny::markdown(readLines("www/notes_visuals.md")),
                shiny::column(width = 2)
              )
            )
          )
        )
      )
    )
  ),
  ##############################################################################
  #########
  #########  Regression analyses panel
  #########
  ##############################################################################
  "analyze" =
    bslib::nav_panel(
      title = "Regression",
      id = "navanalyses",
      do.call(
        bslib::navset_bar,
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
      id = "navdownload",
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
                "import", "data", "variables", "filter", "table1", "univariable", "multivariable"
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
  #########  Documentation panel
  #########
  ##############################################################################
  "docs" = bslib::nav_item(
    # shiny::img(shiny::icon("book")),
    shiny::tags$a(
      href = "https://agdamsbo.github.io/FreesearchR/",
      "Docs (external)",
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
# Initial attempt at creating light and dark versions
light <- custom_theme()
dark <- custom_theme(
  bg = "#000",
  fg = "#fff"
)

# Fonts to consider:
# https://webdesignerdepot.com/17-open-source-fonts-youll-actually-love/

ui <- bslib::page_fixed(
  prismDependencies,
  prismRDependency,
  shiny::tags$head(includeHTML(("www/umami-app.html"))),
  shiny::tags$style(
    type = "text/css",
    # add the name of the tab you want to use as title in data-value
    shiny::HTML(
      ".container-fluid > .nav > li >
                        a[data-value='FreesearchR'] {font-size: 28px}"
    )
  ),
  title = "FreesearchR",
  theme = light,
  shiny::useBusyIndicators(),
  bslib::page_navbar(
    id = "main_panel",
    ui_elements$home,
    ui_elements$import,
    ui_elements$overview,
    ui_elements$describe,
    ui_elements$visuals,
    ui_elements$analyze,
    ui_elements$download,
    bslib::nav_spacer(),
    ui_elements$docs,
    fillable = FALSE,
    footer = shiny::tags$footer(
      style = "background-color: #14131326; padding: 4px; text-align: center; bottom: 0; width: 100%;",
      shiny::p(
        style = "margin: 1",
        "Data is only stored for analyses and deleted immediately afterwards."
      ),
      shiny::p(
        style = "margin: 1; color: #888;",
        "AG Damsbo | v", app_version(), " | ", shiny::tags$a("AGPLv3 license", href = "https://github.com/agdamsbo/FreesearchR/blob/main/LICENSE.md", target = "_blank", rel = "noopener noreferrer"), " | ", shiny::tags$a("Source on Github", href = "https://github.com/agdamsbo/FreesearchR/", target = "_blank", rel = "noopener noreferrer")
      ),
    )
  )
)
