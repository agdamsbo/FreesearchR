# FreesearchR <img style="float: right;" src="logo-text-white-250.png">

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14527429.svg)](https://doi.org/10.5281/zenodo.14527429) 
[![rhub](https://github.com/agdamsbo/FreesearchR/actions/workflows/rhub.yaml/badge.svg)](https://github.com/agdamsbo/FreesearchR/actions/workflows/rhub.yaml)
[![FreesearchR](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://agdamsbo.shinyapps.io/freesearcheR/)
<!-- badges: end -->

This package is the backbone of the ***FreesearchR***, a free and open-source browser based data exploration and analysis tool intended to democratise clinical research by assisting any researcher to easily evaluate and analyse data and export publication ready results.

The ***FreesearchR***-tool is online and accessible here: [link to the app freely hosted on shinyapps.io](https://agdamsbo.shinyapps.io/FreesearcheR/). All feedback is welcome and can be shared as a GitHub issue. Any suggestions on collaboration is much welcomed. Please reach out!

## Motivation

This app has the following simple goals:

1.   help the health clinician getting an overview of data in quality improvement projects and clinical research

1.   help learners get a good start analysing data and coding in *R*

1.   ease quick data overview and basic visualisations for any clinical researcher

## Install locally

The ***FreesearchR***-tool can also be launched locally. Any data.frame available in the global environment will be accessible from the interface.

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

Please note that the ***FreesearchR*** project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Acknowledgements

Like any other project, this project was never possible without the great work of others. These are some of the sources and packages I have used:

-   The ***FreesearchR*** app is build with [Shiny](https://shiny.posit.co/) and based on (*R*)[https://www.r-project.org/].

-   [gtsummary](https://www.danieldsjoberg.com/gtsummary/): superb and flexible way to create publication-ready analytical and summary tables.

-   [dreamRs](https://github.com/dreamRs): maintainers of a broad selection of great extensions and tools for [Shiny](https://shiny.posit.co/).

This project was all written by a human and not by any AI-based tools.
