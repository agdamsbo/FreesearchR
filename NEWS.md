# FreesearchR 25.9.2 - DEV

*NEW* Improvements to translations with more strings having been translated.

# FreesearchR 25.9.1

*NEW* Language has been revised to make the app more accessible and easier to understand.

*NEW* Foundations for introducing an internationalised UI has been introduced. Initial and very rudimentary translation for Danish and Swahili is included. Other languages can be added as well.

*NEW* Alerts as to guide on select important steps and aspects are introduced. This is expected to expand.

# FreesearchR 25.8.2

- *NEW* Including the [NHANES](https://cran.r-project.org/web/packages/NHANES/refman/NHANES.html#NHANES) dataset for experimentation.

- *BUG* Improved JS to correctly handle menu drop downs on mobile devices

- *DOCS* Updated "Prepare > Overview" to "Prepare > Overview and filter" to better reflect options.

# FreesearchR 25.8.1

- *NEW* improved the use of `wrap_plot_list()` to pass on additional arguments to `patchwork::wrap_plots()` and allowed to specify axes to align in `align_axes()`.
- *FIX* fixed axis text printed in Euler diagrams

# FreesearchR 25.7.2

- *FIX* refining hiding drop downs. All JavaScript is now in separate file. Coded with GAI help from claude.ai.

- *FIX* refined iconography and navigation

- *FIX* updated intro and docs.

# FreesearchR 25.7.1

- *NEW* UI overhaul and navigation update. The interface is simplified to clearly show the relationship between panels and sub-items by abandoning multiple levels on panel to instead show a drop-down menu. This also results in simplified sidebar menus with room to add more controls in the future.

# FreesearchR 25.6.4

The app is now also published as a docker container. See the README for instructions. It is mainly to use for hosting the app. Work is ongoing to publish a true standalone app, preferably for both Windows and MacOS.

- *FIX* improved plot labels.

# FreesearchR 25.6.3

- *NEW* Introducing more options to evaluate missing observations. Inspired by the [visdat()] function from the {visdat} package, a specialised function has been introduced to easily visualise data classes and missing observations in the data set. This highly increases the options to visually get an overview of the data and to assess the pattern of missing data. Also under Evaluate, a comparison module has been introduced to compare the distribution of observations across variables depending on the missing vs non-missing in a specified variable.

- *FIX* The REDCap import module has been updated visually and the PAI token is now hidden as a password. This module should still only be used when running locally if you are accessing sensitive data.

- minor rewordings and updated UI.

# FreesearchR 25.6.2

- *FIX* Added warning about only using REDCap with sensitive data running locally. THis applies to all data actually. Considering taking REDCap out in hosted version. Standalone app is in the works.

- *FIX* Reworded the completeness filter to be on missingness, as this is a more commonly used concept.

- *FIX* Improved layout around data filters to improve usage.

- *FIX* Regression table in report respects inclusion of p-values or not.

# FreesearchR 25.6.1

- *FIX* big not allowing to browse data

- *FIX* caught the last bugs when initiating the creation of new variables

# FreesearchR 25.5.6

- *FIX* note on max file size of 5 mb

- *FIX* added a banner to the dev version on shinyapps.io

- *FIX* updated intro text

# FreesearchR 25.5.5

- *FIX* several minor bugs and polish

- *FIX* include/exclude p-values in regression table.

# FreesearchR 25.5.4

- *FIX* correctly omit NAs in `data_type()` call

- *FIX* omit NAs when plotting Euler diagrams.

- *FIX* print correct labels in horizontal stacked bars.

- *FIX* initial app load should feel faster.

# FreesearchR 25.5.3

- *FIX* a little polish on the data import

- *FIX* polished REDCap import and new code to reference the `REDCapCAST::easy_redcap()` function.

- *FIX* updated documentation to reflect new private hosting on a Hetzner server in Germany.

# FreesearchR 25.5.2

- *FIX*: correct export of plots. The solution in the last version broke more than it solved.

- *NEW*: added simple loading animation.

A privately hosted version is now live on app.freesearchr.org. For now, it is hosted on Hetzner with Yunohost.

# FreesearchR 25.5.1

- *FIX*: correct export of single variable plot.

- *NEW*: Include app version in report for reference.

- *NEW*: Show progress on connecting to a REDCap database.

- *FIX*: Data import code export.

# FreesearchR 25.4.5

- *BUG*: Regression results and code not returned correctly

- *IMPROVED*: analyses results are reset on data change

- *NEW*: app usage tracking only in hosted app. README updated to reflect.
 
# FreesearchR 25.4.4

Minor updates in docs and easier citation.

# FreesearchR 25.4.3

- *NEW*: Added a variables type filter to easily exclude unwanted types. This also includes having data type rather than data class in the summary table. Will evaluate. Types are a simpler, more practical version of the *R* data class to easy interpretation.

- *NEW*: A logo is here! It should emphasize the underlying reliance on *R* while also inspire to explore.

- *IMPROVED*: docs are updated and much more comprehensive. They will be continuously updated.

Polishing and moved hosted app to new address to fully reflect name change: [https://agdamsbo.shinyapps.io/FreesearchR/](https://agdamsbo.shinyapps.io/FreesearchR/)

# FreesearchR 25.4.2

Polished and simplified data import module including a much improved REDCap import module.

- *CHANGE* `default_parsing()` now ensure unique variable names.

- *NEW* Working code output for all major modules including import, modifications, filter, evaluation, plotting and regression. And it is nicely formatted!

- *NEW* The basics of a "Getting started"-vignette is done, and can be expanded on later.

# FreesearchR 25.4.1

Focus is on polish and improved ui/ux.

Updating project name to FreesearchR, with renamed repository. Graphics are coming. This may introduce some breaking chances for others calling or installing the package. No additional future changes are planned. A complete transition is planned before attending and presenting a poster at the European Stroke Organisation Conference 2025 in May.

Testing file upload conducted and improved.

Working on improving code export. This is very difficult to get perfect. Initial focus is on extracting enough to be able to learn from it.

Regression calculations, plots, and checks have been improved and moved to standalone module.

Data overview/modifications has been simplified slightly.

# freesearcheR 25.3.1

First steps towards a more focused and simplified interface.

Focus will be on ease of use, handling basic functionality for data inspection and descriptive analyses.

Inspired by the Stroke Center implementation guidelines of the WSO, we will apply a similar approach to this project in order to keep the interface simple and robust. Basic functions for descriptive analyses and data browsing are the basics. More advanced features like regression analyses are added for learning purposes, but really should be done by one self in software like *R* to ensure learning and reproducibility.

Teal dependencies removed. The teal framework really seems very powerful and promising, but it will also mean less control and more clutter. May come up again later.

All main components have been implemented:

-   Data import from different sources

-   Data management (variable creation, re-classing, naming, labelling and more)

-   Basic data comparisons and descriptive analyses

-   Basic data visualisations with a select set of plot types great for publication purposes

-   Regression analysis of basic clinical cross-sectional data (mixed models of repeated measures and survival analyses is on the table)

-   Export of outputs (descriptive analyses and regression) as well as modified dataset (code is also showed, but not working as it should)

Next steps are:

-   Polished code export

-   Improved workflow and descriptive text as well as thorough step-wise guide/documentation (possibly with small videos)

-   Implement in clinical projects

-   Implement in data analysis courses

-   Extensive testing and bug squashing


# freesearcheR 25.1.1

* UI tweaks.

* NEW: Option to set class as `hms` using the `{hms}` package.

* NEW: summary grid with sparklines.

* Speed improvements and better regression analysis handling. Preparations for extending analysis options and study designs.


# freesearcheR 24.12.1

* Initial release for Zenodo.
