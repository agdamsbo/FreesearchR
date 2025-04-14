# FreesearchR

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14527429.svg)](https://doi.org/10.5281/zenodo.14527429) 
[![rhub](https://github.com/agdamsbo/FreesearchR/actions/workflows/rhub.yaml/badge.svg)](https://github.com/agdamsbo/FreesearchR/actions/workflows/rhub.yaml)
[![FreesearchR](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://agdamsbo.shinyapps.io/freesearcheR/)
<!-- badges: end -->

This package is the backbone of the ***FreesearchR***, a free and open-source browser based data exploration and analysis tool for clinicians and researchers with publication ready output.

This package and the ***FreesearchR***-tool is part of a larger initiative to democratize health data analysis and remove barriers for clinicians to engage in health research.

the ***FreesearchR***-tool is online and accessible here: [link to the app freely hosted on shinyapps.io](https://agdamsbo.shinyapps.io/FreesearcheR/). All feedback is welcome and can be shared as a GitHub issue.

Initiatives for funding continued development of the tool and surrounding initiatives is ongoing.

## Motivation




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
