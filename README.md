# FreesearchR <a href="https://agdamsbo.github.io/FreesearchR/"><img src="man/figures/logo.png" align="right" height="70" alt="FreesearchR website" /></a>

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14527429.svg)](https://doi.org/10.5281/zenodo.14527429) 
[![rhub](https://github.com/agdamsbo/FreesearchR/actions/workflows/rhub.yaml/badge.svg)](https://github.com/agdamsbo/FreesearchR/actions/workflows/rhub.yaml)
[![FreesearchR](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://agdamsbo.shinyapps.io/FreesearchR/)
<!-- badges: end -->

The [***FreesearchR***](https://agdamsbo.shinyapps.io/FreesearchR/) is a simple, clinical health data exploration and analysis tool to democratise clinical research by assisting any researcher to easily evaluate and analyse data and export publication ready results.

[***FreesearchR***](https://agdamsbo.shinyapps.io/FreesearchR/) is free and open-source, and is directly accessible here: [link to the app freely hosted on shinyapps.io](https://agdamsbo.shinyapps.io/FreesearchR/). The app can also run locally, please see below.

All feedback is welcome and can be shared as a GitHub issue. Any suggestions on collaboration is much welcomed. Please reach out!

![FreesearchR demo](main/figures/demo.gif)

## Motivation

This app has the following simple goals:

1.   help the health clinician getting an overview of data in quality improvement projects and clinical research

1.   help learners get a good start analysing data and coding in *R*

1.   ease quick data overview and basic visualisations for any clinical researcher

## Run locally on your own machine

The ***FreesearchR*** app can also run on your own machine with no data transmitted anywhere. Any data.frame available in the global environment will be accessible from the interface. Just follow the below steps:

1.   **Requirement:** You need to have [*R* installed](https://www.r-project.org/) and possibly an editor like [RStudio](https://posit.co/download/rstudio-desktop/). 

1.   Then open the *R* console and copy/paste the following code, that will install the `{devtools}` package and then the `{FreesearchR}` *R*-package with its dependencies:

     ```
     require("devtools")
     devtools::install_github("agdamsbo/FreesearchR")
     library(FreesearchR)
     # By loading mtcars to the environment, it will be available 
     # in the interface like any other data.frame
     data(mtcars) 
     launch_FreesearchR()
     ```

## Code of Conduct

Please note that the ***FreesearchR*** project is published with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Acknowledgements

Like any other project, this project was never possible without the great work of others. These are some of the sources and packages I have used:

-   The ***FreesearchR*** app is build with [Shiny](https://shiny.posit.co/) and based on [*R*](https://www.r-project.org/).

-   [gtsummary](https://www.danieldsjoberg.com/gtsummary/): superb and flexible way to create publication-ready analytical and summary tables.

-   [dreamRs](https://github.com/dreamRs): maintainers of a broad selection of great extensions and tools for [Shiny](https://shiny.posit.co/).

-   [easystats](https://easystats.github.io/easystats/): the [`performance::check_model()`](https://easystats.github.io/performance/articles/check_model.html) function was central in sparking the idea to create a data analysis tool.

-   [IDEAfilter](https://biogen-inc.github.io/IDEAFilter/): a visually appealing data filter function based on the [{shinyDataFilter}](https://github.com/dgkf/shinyDataFilter).

This project was all written by a human and not by any AI-based tools.

The online ***FreesearchR*** app contains a tracking script, transmitting minimal data on usage. No uploaded data is transmitted anywhere. Have a look at the [tracking data here](https://analytics.gdamsbo.dk/share/2i4BNpMcDMB9lJvF/agdamsbo.shinyapps.io). No tracking data is sent running the app locally (see above).
