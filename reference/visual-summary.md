# Data correlations evaluation module

Data correlations evaluation module

Visual summary server

Visual summary modal

Plot missings and class with apexcharter. Not in use with FreesearchR.

Ggplot2 data summary visualisation based on visdat::vis_dat.

## Usage

``` r
visual_summary_ui(id)

visual_summary_server(id, data_r = shiny::reactive(NULL), ...)

modal_visual_summary(
  id,
  title = "Visual overview of data classes and missing observations",
  easyClose = TRUE,
  size = "xl",
  footer = NULL,
  ...
)

missings_apex_plot(data, animation = FALSE, ...)

visual_summary(data, legend.title = NULL, ylab = "Observations", ...)
```

## Arguments

- id:

  id

- data_r:

  reactive data

- ...:

  optional arguments passed to data_summary_gather()

- title:

  title

- easyClose:

  easyClose

- size:

  modal size

- footer:

  modal footer

- data:

  data

- legend.title:

  Legend title

- ylab:

  Y axis label

## Value

Shiny ui module

shiny server

shiny modal

An
[`apexcharter::apexchart()`](https://dreamrs.github.io/apexcharter/reference/apexchart.html)
`htmlwidget` object.

ggplot2 object
