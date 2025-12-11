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
        # "The browser language is",
        # textOutput("your_lang"),
        # p(i18n$t("Hello")),
        # shiny::uiOutput(outputId = "language_select"),
        ## On building the dev-version for shinyapps.io, the dev_banner() is redefined
        ## Default just output "NULL"
        ## This could probably be achieved more legantly, but this works.
        dev_banner(),
        shiny::column(width = 2),
        shiny::column(
          width = 8,
          # shiny::uiOutput(outputId = "language_select"),
          htmlOutput("intro_text")
          # shiny::includeHTML(i18n$t("www/intro.html"))
          # shiny::markdown(readLines(i18n$t("www/intro.md")))
        ),
        shiny::column(width = 2)
      )
    ),
    ##############################################################################
    #########
    #########  Import panel
    #########
    ##############################################################################
    "import" = bslib::nav_panel(
      title = i18n$t("Get started"),
      icon = shiny::icon("play"),
      value = "nav_import",
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(
          width = 8,
          shiny::h4(i18n$t("Choose your data")),
          # shiny::br(),
          # shiny::uiOutput(outputId = "source"),
          # radioGroupButtons(
          #   inputId = "source",
          #   selected = "file",
          #   choices = c("File" = "file"),
          #   size = "lg"
          # ),
          shiny::selectInput(
            inputId = "source",
            label = "",
            selected = "file",
            choices = "file",
            width = "100%"
          ),
          # shiny::tags$script('document.querySelector("#source div").style.width = "100%"'),
          ## Update this to change depending on run locally or hosted
          shiny::helpText(i18n$t("Upload a file, get data directly from REDCap or use local or sample data.")),
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
            shiny::uiOutput(outputId = "redcap_warning"),
            # shinyWidgets::alert(
            #   id = "redcap-warning",
            #   status = "warning",
            #   shiny::tags$h2(i18n$t("Please be mindfull handling sensitive data")),
            #   shiny::HTML(i18n$t("<p>The <em><strong>FreesearchR</strong></em> app only stores data for analyses, but please only use with sensitive data when running locally. <a href='https://agdamsbo.github.io/FreesearchR/#run-locally-on-your-own-machine'>Read more here</a></p>")),
            #   dismissible = TRUE
            # ),
            m_redcap_readUI(
              id = "redcap_import",
              title = ""
            )
          ),
          shiny::conditionalPanel(
            condition = "input.source=='env'",
            import_globalenv_ui(
              id = "env",
              title = NULL,
              packages = c("NHANES", "stRoke", "datasets", "MASS")
            )
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
              label = i18n$t("Quick overview"),
              width = "100%",
              icon = shiny::icon("binoculars"),
              disabled = FALSE
            ),
            shiny::br(),
            shiny::br(),
            shiny::h5(i18n$t("Select variables for final import")),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::p(i18n$t("Exclude incomplete variables:")),
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
                shiny::helpText(i18n$t("At 0, only complete variables are included; at 100, all variables are included.")),
                shiny::br()
              ),
              shiny::column(
                width = 6,
                shiny::p(i18n$t("Manual selection:")),
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
              label = i18n$t("Let's begin!"),
              width = "100%",
              icon = shiny::icon("play"),
              disabled = TRUE
            ),
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
      title = i18n$t("Prepare"),
      icon = shiny::icon("pen-to-square"),
      value = "nav_prepare",
      bslib::nav_panel(
        title = i18n$t("Overview and filter"),
        icon = shiny::icon("eye"),
        value = "nav_prepare_overview",
        tags$h3(i18n$t("Overview and filtering")),
        fluidRow(
          shiny::column(
            width = 9,
            shiny::uiOutput(outputId = "data_info", inline = TRUE),
            shiny::tags$p(
              i18n$t("Below you find a summary table for quick insigths, and on the right you can visualise data classes, browse observations and apply different data filters.")
            )
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              inputId = "modal_visual_overview",
              label = i18n$t("Visual overview"),
              width = "100%",
              disabled = TRUE
            ),
            shiny::br(),
            shiny::br(),
            shiny::actionButton(
              inputId = "modal_browse",
              label = i18n$t("Browse observations"),
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
            shiny::tags$h6(i18n$t("Filter data types")),
            shiny::uiOutput(
              outputId = "column_filter"
            ),
            ## This needs to run in server for translation
            shiny::helpText("Read more on how ", tags$a(
              "data types",
              href = "https://agdamsbo.github.io/FreesearchR/articles/data-types.html",
              target = "_blank",
              rel = "noopener noreferrer"
            ), " are defined."),
            validation_ui("validation_var"),
            shiny::br(),
            shiny::br(),
            shiny::tags$h6(i18n$t("Filter observations")),
            shiny::tags$p(i18n$t("Apply filter on observation")),
            IDEAFilter::IDEAFilter_ui("data_filter"),
            validation_ui("validation_obs"),
            shiny::br(),
            shiny::br()
          )
        ),
        shiny::br(),
        shiny::br(),
        shiny::br()
      ),
      bslib::nav_panel(
        title = i18n$t("Edit and create data"),
        icon = shiny::icon("file-pen"),
        tags$h3(i18n$t("Subset, rename and convert variables")),
        fluidRow(
          shiny::column(
            width = 9,
            shiny::tags$p(
              i18n$t("Below, are several options for simple data manipulation like update variables by renaming, creating new labels (for nicer tables in the report) and changing variable classes (numeric, factor/categorical etc.)."),
              i18n$t("There are more advanced options to modify factor/categorical variables as well as create new factor from an existing variable or new variables with R code. At the bottom you can restore the original data."),
              i18n$t("Please note that data modifications are applied before any filtering.")
            )
          )
        ),
        update_variables_ui("modal_variables"),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::tags$h4(i18n$t("Advanced data manipulation")),
        shiny::tags$p(i18n$t("Below options allow more advanced varaible manipulations.")),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::actionButton(
              inputId = "modal_update",
              label = i18n$t("Reorder factor levels"),
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText(i18n$t("Reorder the levels of factor/categorical variables.")),
            shiny::tags$br(),
            shiny::tags$br()
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              inputId = "modal_cut",
              label = i18n$t("New factor"),
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText(i18n$t("Create factor/categorical variable from a continous variable (number/date/time).")),
            shiny::tags$br(),
            shiny::tags$br()
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              inputId = "modal_string",
              label = i18n$t("Split text"),
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText(i18n$t("Split a text column by a recognised delimiter.")),
            shiny::tags$br(),
            shiny::tags$br()
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              inputId = "modal_column",
              label = i18n$t("New variable"),
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText(i18n$t("Create a new variable based on an R-expression.")),
            shiny::tags$br(),
            shiny::tags$br()
          )
        ),
        tags$h4(i18n$t("Compare modified data to original")),
        shiny::tags$br(),
        shiny::tags$p(
          i18n$t("Raw print of the original vs the modified data.")
        ),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::tags$b(i18n$t("Original data:")),
            shiny::verbatimTextOutput("original_str")
          ),
          shiny::column(
            width = 6,
            shiny::tags$b(i18n$t("Modified data:")),
            shiny::verbatimTextOutput("modified_str")
          )
        ),
        shiny::tags$br(),
        shiny::actionButton(
          inputId = "data_reset",
          label = i18n$t("Restore original data"),
          width = "100%"
        ),
        shiny::tags$br(),
        shiny::helpText(i18n$t("Reset to original imported dataset. Careful! There is no un-doing.")),
        shiny::tags$br(),
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
        title = i18n$t("Evaluate"),
        icon = shiny::icon("magnifying-glass-chart"),
        value = "nav_describe",
        # id = "navdescribe",
        # bslib::navset_bar(
        #   title = "",
        bslib::nav_panel(
          title = i18n$t("Characteristics"),
          icon = bsicons::bs_icon("table"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              shiny::uiOutput(outputId = "data_info_nochar", inline = TRUE),
              bslib::accordion(
                id = "acc_chars",
                open = "acc_chars",
                multiple = FALSE,
                bslib::accordion_panel(
                  open = TRUE,
                  value = "acc_pan_chars",
                  title = "Settings",
                  icon = bsicons::bs_icon("table"),
                  # vectorSelectInput(
                  #   inputId = "baseline_theme",
                  #   selected = "none",
                  #   label = i18n$t("Select table theme"),
                  #   choices = c(
                  #     "The Journal of the American Medical Association" = "jama",
                  #     "The Lancet"="lancet",
                  #     "The New England Journal of Medicine" = "nejm",
                  #     "The Quarterly Journal of Economics" = "qjecon")
                  # ),
                  shiny::uiOutput("detail_level"),
                  shiny::uiOutput("strat_var"),
                  shiny::helpText(i18n$t("Only factor/categorical variables are available for stratification. Go back to the 'Prepare' tab to reclass a variable if it's not on the list.")),
                  shiny::conditionalPanel(
                    condition = "input.strat_var!='none'",
                    shiny::radioButtons(
                      inputId = "add_p",
                      label = i18n$t("Compare strata?"),
                      selected = "no",
                      inline = TRUE,
                      choices = list(
                        "No" = "no",
                        "Yes" = "yes"
                      )
                    ),
                    # shiny::helpText(i18n$t("Option to perform statistical comparisons between strata in baseline table.")),
                    shiny::br(),
                    shiny::radioButtons(
                      inputId = "add_diff",
                      label = i18n$t("Include group differences"),
                      selected = "no",
                      inline = TRUE,
                      choices = list(
                        "No" = "no",
                        "Yes" = "yes"
                      )
                    )
                  ),
                  shiny::br(),
                  shiny::actionButton(
                    inputId = "act_eval",
                    label = i18n$t("Evaluate"),
                    width = "100%",
                    icon = shiny::icon("calculator"),
                    disabled = TRUE
                  ),
                  shiny::helpText(i18n$t("Press 'Evaluate' to create the comparison table."))
                )
              )
            ),
            gt::gt_output(outputId = "table1")
          )
        ),
        bslib::nav_panel(
          title = i18n$t("Correlations"),
          icon = bsicons::bs_icon("bounding-box"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              # shiny::uiOutput(outputId = "data_info_nochar", inline = TRUE),
              bslib::accordion(
                id = "acc_cor",
                open = "acc_chars",
                multiple = FALSE,
                bslib::accordion_panel(
                  value = "acc_pan_cor",
                  title = "Settings",
                  icon = bsicons::bs_icon("bounding-box"),
                  shiny::uiOutput("outcome_var_cor"),
                  shiny::helpText(i18n$t("To avoid evaluating the correlation of the outcome variable, this can be excluded from the plot or select 'none'.")),
                  shiny::br(),
                  shinyWidgets::noUiSliderInput(
                    inputId = "cor_cutoff",
                    label = i18n$t("Correlation cut-off"),
                    min = 0,
                    max = 1,
                    step = .01,
                    value = .8,
                    format = shinyWidgets::wNumbFormat(decimals = 2),
                    color = datamods:::get_primary_color()
                  ),
                  shiny::helpText(i18n$t("Set the cut-off for considered 'highly correlated'."))
                )
              )
            ),
            data_correlations_ui(id = "correlations", height = 600)
          )
        ),
        do.call(
          bslib::nav_panel,
          c(
            list(
              title = i18n$t("Missings"),
              icon = bsicons::bs_icon("x-circle")
            ),
            data_missings_ui(id = "missingness",
                             validation_ui("validation_mcar"))
          )
        )
      ),
    ##############################################################################
    #########
    #########  Visuals panel
    #########
    ##############################################################################
    "visuals" = do.call(
      bslib::nav_panel,
      c(
        list(
          title = i18n$t("Visuals"),
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
        title = i18n$t("Regression"),
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
        title = i18n$t("Download"),
        icon = shiny::icon("download"),
        value = "nav_download",
        shiny::fluidRow(
          shiny::column(width = 2),
          shiny::column(
            width = 8,
            shiny::h4(i18n$t("Analysis validation")),
            validation_ui("validation_all"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h4(i18n$t("Report")),
                shiny::helpText(i18n$t("Choose your favourite output file format for further work, and download, when the analyses are done.")),
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
                ),
                shiny::br()
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
  if (!is.null(selection)) {
    out[[selection]]
  } else {
    out
  }
}


# ls <- list("home"=1:4,
#            "test"=1:4)
#
