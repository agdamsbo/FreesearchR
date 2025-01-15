# ns <- NS(id)

ui_elements <- list(
  ##############################################################################
  #########
  #########  Home panel
  #########
  ##############################################################################
  "home" = bslib::nav_panel(
    title = "freesearcheR",
    shiny::markdown(readLines("www/intro.md")),
    icon = shiny::icon("home")
  ),
  ##############################################################################
  #########
  #########  Import panel
  #########
  ##############################################################################
  "import" = bslib::nav_panel(
    title = "Import",
    shiny::tagList(
      shiny::h4("Choose your data source"),
      # shiny::conditionalPanel(
      #   condition = "output.has_input=='yes'",
      #   # Input: Select a file ----
      #   shiny::helpText("Analyses are performed on provided data")
      # ),
      # shiny::conditionalPanel(
      #   condition = "output.has_input=='no'",
      # Input: Select a file ----
      shinyWidgets::radioGroupButtons(
        inputId = "source",
        selected = "env",
        # label = "Choice: ",
        choices = c(
          "File upload" = "file",
          "REDCap server" = "redcap",
          "Local data" = "env"
        ),
        # checkIcon = list(
        #   yes = icon("square-check"),
        #   no = icon("square")
        # ),
        width = "100%"
      ),
      shiny::conditionalPanel(
        condition = "input.source=='file'",
        datamods::import_file_ui("file_import",
          title = "Choose a datafile to upload",
          file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav", ".ods", ".dta")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.source=='redcap'",
        m_redcap_readUI("redcap_import")
      ),
      shiny::conditionalPanel(
        condition = "input.source=='env'",
        import_globalenv_ui(id = "env", title = NULL)
      ),
      shiny::conditionalPanel(
        condition = "input.source=='redcap'",
        DT::DTOutput(outputId = "redcap_prev")
      ),
      shiny::br(),
      shiny::actionButton(
        inputId = "act_start",
        label = "Start",
        width = "100%",
        icon = shiny::icon("play")
      ),
      shiny::helpText('After importing, hit "Start" or navigate to the desired tab.'),
      shiny::br(),
      shiny::br()
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
          title = "Summary & filter",
          tags$h3("Data summary and filtering"),
          fluidRow(
            shiny::column(
              width = 9,
              shiny::tags$p(
              "Below is a short summary table of the provided data.
              On the right hand side you have the option to create filters.
              At the bottom you'll find a raw overview of the original vs the modified data.")
            )
          ),
          fluidRow(
            # column(
            #   width = 3,
            #   shiny::uiOutput("filter_vars"),
            #   shiny::conditionalPanel(
            #     condition = "(typeof input.filter_vars !== 'undefined' && input.filter_vars.length > 0)",
            #     datamods::filter_data_ui("filtering", max_height = "500px")
            #   )
            # ),
            # column(
            #   width = 9,
            #   DT::DTOutput(outputId = "filtered_table"),
            #   tags$b("Code dplyr:"),
            #   verbatimTextOutput(outputId = "filtered_code")
            # ),
            shiny::column(
              width = 9,
              toastui::datagridOutput(outputId = "tbl_overview"),
              # data_summary_ui(id = "data_summary"),
              shiny::tags$b("Reproducible code:"),
              shiny::verbatimTextOutput(outputId = "filtered_code")
            ),
            shiny::column(
              width = 3,
              IDEAFilter::IDEAFilter_ui("data_filter") # ,
              # shiny::actionButton("save_filter", "Apply the filter")
            )
          ),
          fluidRow(
            column(
              width = 6,
              tags$b("Original data:"),
              # verbatimTextOutput("original"),
              verbatimTextOutput("original_str")
            ),
            column(
              width = 6,
              tags$b("Modified data:"),
              # verbatimTextOutput("modified"),
              verbatimTextOutput("modified_str")
            )
          )
        ),
        # bslib::nav_panel(
        #   title = "Overview",
        #   DT::DTOutput(outputId = "table")
        # ),
        bslib::nav_panel(
          title = "Modify",
          tags$h3("Subset, rename and convert variables"),
          fluidRow(
            shiny::column(
              width = 9,
              shiny::tags$p("Below, you can subset the data (by not selecting the variables to exclude on applying changes), rename variables, set new labels (for nicer tables in the analysis report) and change variable classes.
                            Italic text can be edited/changed.
                            On the right, you can create and modify factor/categorical variables as well as resetting the data to the originally imported data.")
            )
          ),
          fluidRow(
            shiny::column(
              width = 9,
              update_variables_ui("vars_update")
            ),
            shiny::column(
              width = 3,
              shiny::actionButton("modal_cut", "Create factor variable"),
              shiny::tags$br(),
              shiny::helpText("Create factor/categorical variable from an other value."),
              shiny::tags$br(),
              shiny::tags$br(),
              shiny::actionButton("modal_update", "Reorder factor levels"),
              shiny::tags$br(),
              shiny::helpText("Reorder the levels of factor/categorical variables."),
              shiny::tags$br(),
              shiny::tags$br(),
              shiny::actionButton("data_reset", "Restore original data"),
              shiny::tags$br(),
              shiny::helpText("Reset to original imported dataset. Careful! There is no un-doing."),
              shiny::tags$br() # ,
              # shiny::tags$br(),
              # shiny::tags$br(),
              # IDEAFilter::IDEAFilter_ui("data_filter") # ,
              # shiny::actionButton("save_filter", "Apply the filter")
            )
            # datamods::update_variables_ui("vars_update")
          )
        ),
        bslib::nav_panel(
          title = "Browser",
          tags$h3("Browse the provided data"),
          shiny::tags$p(
                "Below is a data table with all the modified data provided to browse and understand data."
                ),
          shinyWidgets::html_dependency_winbox(),
          # fluidRow(
          # column(
          #   width = 3,
          #   shiny::uiOutput("filter_vars"),
          #   shiny::conditionalPanel(
          #     condition = "(typeof input.filter_vars !== 'undefined' && input.filter_vars.length > 0)",
          #     datamods::filter_data_ui("filtering", max_height = "500px")
          #   )
          # ),
          # column(
          #   width = 9,
          #   DT::DTOutput(outputId = "filtered_table"),
          #   tags$b("Code dplyr:"),
          #   verbatimTextOutput(outputId = "filtered_code")
          # ),
          # shiny::column(
          #   width = 8,
          toastui::datagridOutput(outputId = "table_mod") # ,
          # shiny::tags$b("Reproducible code:"),
          # shiny::verbatimTextOutput(outputId = "filtered_code")
          #   ),
          #   shiny::column(
          #     width = 4,
          #     shiny::actionButton("modal_cut", "Create factor from a variable"),
          #     shiny::tags$br(),
          #     shiny::tags$br(),
          #     shiny::actionButton("modal_update", "Reorder factor levels")#,
          #     # shiny::tags$br(),
          #     # shiny::tags$br(),
          #     # IDEAFilter::IDEAFilter_ui("data_filter") # ,
          #     # shiny::actionButton("save_filter", "Apply the filter")
          #   )
          # )
        )


        # column(
        #   8,
        #   shiny::verbatimTextOutput("filtered_code"),
        #   DT::DTOutput("filtered_table")
        # ),
        # column(4, IDEAFilter::IDEAFilter_ui("data_filter"))
      )
    ),
  ##############################################################################
  #########
  #########  Data analyses panel
  #########
  ##############################################################################
  "analyze" =
  # bslib::nav_panel_hidden(
    bslib::nav_panel(
      # value = "analyze",
      title = "Analyses",
      bslib::navset_bar(
        title = "",
        # bslib::layout_sidebar(
        #   fillable = TRUE,
        sidebar = bslib::sidebar(
          shiny::helpText(em("Please specify relevant settings for your data, and press 'Analyse'")),
          shiny::uiOutput("outcome_var"),
          shiny::radioButtons(
            inputId = "all",
            label = "Specify covariables",
            inline = TRUE, selected = 2,
            choiceNames = c(
              "Yes",
              "No"
            ),
            choiceValues = c(1, 2)
          ),
          shiny::conditionalPanel(
            condition = "input.all==1",
            shiny::uiOutput("include_vars")
          ),
          shiny::uiOutput("strat_var"),
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
          shiny::radioButtons(
            inputId = "specify_factors",
            label = "Specify categorical variables?",
            selected = "no",
            inline = TRUE,
            choices = list(
              "Yes" = "yes",
              "No" = "no"
            )
          ),
          shiny::conditionalPanel(
            condition = "input.specify_factors=='yes'",
            shiny::uiOutput("factor_vars")
          ),
          bslib::input_task_button(
            id = "load",
            label = "Analyse",
            icon = shiny::icon("pencil", lib = "glyphicon"),
            label_busy = "Working...",
            icon_busy = fontawesome::fa_i("arrows-rotate",
              class = "fa-spin",
              "aria-hidden" = "true"
            ),
            type = "secondary",
            auto_reset = TRUE
          ),
          shiny::helpText("If you change the parameters, press 'Analyse' again to update the tables"),
          # shiny::conditionalPanel(
          #   condition = "output.ready=='yes'",
          shiny::tags$hr(),
          shiny::h4("Download results"),
          shiny::helpText("Choose your favourite output file format for further work, and download, when the analyses are done."),
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
          shiny::helpText("If choosing to output to MS Word, please note, that when opening the document, two errors will pop-up. Choose to repair and choose not to update references. The issue is being worked on. You can always choose LibreOffice instead."),
          shiny::tags$hr(),
          shiny::h4("Download data"),
          shiny::helpText("Choose your favourite output data format to download the modified data."),
          shiny::selectInput(
            inputId = "data_type",
            label = "Data format",
            selected = NULL,
            choices = list(
              "R" = "rds",
              "stata" = "dta"
            )
          ),
          shiny::br(),
          # Button
          shiny::downloadButton(
            outputId = "data_modified",
            label = "Download data",
            icon = shiny::icon("download")
          )
        ),
        bslib::nav_panel(
          title = "Baseline characteristics",
          gt::gt_output(outputId = "table1")
        ),
        bslib::nav_panel(
          title = "Regression table",
          gt::gt_output(outputId = "table2")
        ),
        bslib::nav_panel(
          title = "Regression checks",
          shiny::plotOutput(outputId = "check")
        )
      )
    ),
  ##############################################################################
  #########
  #########  Documentation panel
  #########
  ##############################################################################
  "docs" = bslib::nav_panel(
    title = "Documentation",
    # shiny::tags$iframe("www/docs.html", height=600, width=535),
    shiny::htmlOutput("docs_file"),
    shiny::br()
  )
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
  shiny::tags$style(
    type = "text/css",
    # add the name of the tab you want to use as title in data-value
    shiny::HTML(
      ".container-fluid > .nav > li >
                        a[data-value='freesearcheR'] {font-size: 28px}"
    )
  ),
  title = "freesearcheR",
  theme = light,
  shiny::useBusyIndicators(),
  bslib::page_navbar(
    # title = "freesearcheR",
    id = "main_panel",
    # header = shiny::tags$header(shiny::p("Data is only stored temporarily for analysis and deleted immediately afterwards.")),
    ui_elements$home,
    ui_elements$import,
    ui_elements$overview,
    ui_elements$analyze,
    ui_elements$docs,
    # bslib::nav_spacer(),
    # bslib::nav_item(shinyWidgets::circleButton(inputId = "mode", icon = icon("moon"),status = "primary")),
    fillable = FALSE,
    footer = shiny::tags$footer(
      style = "background-color: #14131326; padding: 4px; text-align: center; bottom: 0; width: 100%;",
      shiny::p(
        style = "margin: 1",
        "Data is only stored for analyses and deleted immediately afterwards."
      ),
      shiny::p(
        style = "margin: 1; color: #888;",
        "Andreas G Damsbo | AGPLv3 license | ", shiny::tags$a("Source on Github", href = "https://github.com/agdamsbo/freesearcheR/", target = "_blank", rel = "noopener noreferrer")
      ),
    )
  )
)
