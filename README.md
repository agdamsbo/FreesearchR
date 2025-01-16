# freesearcheR

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![rhub](https://github.com/agdamsbo/freesearcheR/actions/workflows/rhub.yaml/badge.svg)](https://github.com/agdamsbo/freesearcheR/actions/workflows/rhub.yaml)
[![freesearcheR](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://agdamsbo.shinyapps.io/freesearcheR/)
<!-- badges: end -->

This package is the backbone of the free and open browser based data exploration and analysis tool with publication ready output.

This package and the ***freesearcheR***-tool is part of a large project to democratize health data analysis and removing barriers for clinicians to engage in health research.

the ***freesearcheR***-tool is online and accessible here: [link to the app freely hosted on shinyapps.io](https://agdamsbo.shinyapps.io/freesearcheR/). All feedback is welcome and can be shared as a GitHub issue.

Initiatives for funding continued development of the tool and surrounding initiatives is ongoing.

## Roadmap

-   [ ] Stratified analyses

-   Additional study designs:

    -   [x] Cross-sectional data analyses

    -   [ ] Longitudinal data analyses

    -   [ ] Survival analysis

-   More detailed variable browser

    -   [x] Add histograms for data distribution. 2025-01-16

    -   [x] Option to edit labels. 2025-01-16

-   [ ] Plot regression analyses results

-   [x] Export modified data. 2025-01-16

-   [ ] Include reproducible code for all steps

-   [x] ~~Modify factor levels~~ Factor level modifications is possible through converting factors to numeric > cutting numeric with desired fixed values. 2024-12-12

-   [x] More options for date/datetime/time grouping/factoring. Included weekday and month-only options. 2024-12-12


## Install locally

The ***freesearcheR***-tool can also be launched locally. Any data.frame available in the global environment will be accessible from the interface.

```
require("devtools")
devtools::install_github("agdamsbo/freesearcheR")
library(freesearcheR)
# By loading mtcars to the environment, it will be available 
# in the interface like any other data.frame
data(mtcars) 
shiny_freesearcheR()
```

## Code of Conduct

Please note that the freesearcheR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
