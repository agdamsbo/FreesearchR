# Changelog

## FreesearchR 25.12.6

*NEW* Export missingness table directly.

*NEW* Updated and slightly extended list of allowed operators for new
variable creation.

*FIX* The exported MS Word documents prompted a warning. I believe that
has been fixed, but required several new dependencies.

## FreesearchR 25.12.5

*NEW* Added option to add pairwise differences in discriptive table.

## FreesearchR 25.12.4

Republish 25.12.3

## FreesearchR 25.12.3

*NEW* Extended missingness evaluation to include two different
approaches. Docs will catch up and video tutorials are coming.

## FreesearchR 25.12.2

*FIX* Fixed hanging interface when splitting strings.

*NEW* New option to shorten character variables to the first N words or
characters. Shortening by characters could be useful working with eg.
ICD-10 diagnostic codes.

## FreesearchR 25.12.1

*NEW* Option to edit factor label names in the “New factor” pop-up. This
allows for easier naming for tables, but also to combine levels. A new
variable is appended to the dataset if label names are changed. Code is
now also exported.

*FIX* Fixes a bug, where white space in code exported was removed. Now a
little too many spaces are included. Fine tuning continues.

*NEW* Easily copy code by just clicking “copy” in code blocks.

## FreesearchR 25.11.2

*NEW* Vignettes were moved to the [FreesearchR project knowledge
base](https://freesearchr.github.io/FreesearchR-knowledge/). This was
mainly to ease rendering and allow quick and easy updates as well as
future translations.

## FreesearchR 25.11.1

*NEW* Added option to select extensive baseline table selecting between
“Minimal” (current) or “Extensive” which adds mean/sd and min/max as
well as plots all levels also for dichotomous variables.

## FreesearchR 25.10.5

*NEW* New character/text split function available. A selection of
delimiters are recognised and selectable. Function only available if
splittable variables are present.

*NEW* Distribution plotting for factors have been much improved
including two new bar plot styles and removing options better suited for
continuous data.

These were the last major functions to be implemented after workshops at
Jitimai in Zanzibar City, Zanzibar during October 2025.

## FreesearchR 25.10.4

*NEW* Two new options to create new simplified factors from factors. The
“top” option will keep only the top N levels, while the “bottom” option
will combine all levels occurring below set percentage.

*NEW* The “New factor” function received some updates to include
detailed information on the methods for creating new factors.

*LANGUAGE* Updated Swahili strings. All Danish strings are translated.
New languages are on track.

## FreesearchR 25.10.3

*NEW* Improvements to translations with more strings having been
translated. Nearing completion of marking strings for translation, which
means (almost) the complete interface is now translatable.

## FreesearchR 25.10.2

*NEW* Improvements to translations with more strings having been
translated.

*NEW* More detailed label for the stacked horizontal bar plot.

*NEW* Better .rds import that will import the first element as
data.frame if a list-type element is supplied.

*NEW* A limit to the imported dataset size was added to ensure
performance on hosted version. The data is limited to 100.000 cells by
dropping rows to fit. The vast majority of users will never experience
this capping, but adds a layer of security and stability to the hosting
framework.

## FreesearchR 25.10.1

*NEW* Improvements to translations with more strings having been
translated.

*NEW* Sample data has been filtered to only include a few select
packages (NHANES and stRoke).

*NEW* Missings evaluations slightly tweaked to include bold significant
p-values for easier overview.

## FreesearchR 25.9.2

*NEW* Improvements to translations with more strings having been
translated.

The Euler visualisation option has been limited to only plot dichotomous
variables. This is also what makes the most sense.

## FreesearchR 25.9.1

*NEW* Language has been revised to make the app more accessible and
easier to understand.

*NEW* Foundations for introducing an internationalised UI has been
introduced. Initial and very rudimentary translation for Danish and
Swahili is included. Other languages can be added as well.

*NEW* Alerts as to guide on select important steps and aspects are
introduced. This is expected to expand.

## FreesearchR 25.8.2

- *NEW* Including the
  [NHANES](https://cran.r-project.org/web/packages/NHANES/refman/NHANES.html#NHANES)
  dataset for experimentation.

- *BUG* Improved JS to correctly handle menu drop downs on mobile
  devices

- *DOCS* Updated “Prepare \> Overview” to “Prepare \> Overview and
  filter” to better reflect options.

## FreesearchR 25.8.1

- *NEW* improved the use of
  [`wrap_plot_list()`](https://agdamsbo.github.io/FreesearchR/reference/wrap_plot_list.md)
  to pass on additional arguments to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  and allowed to specify axes to align in
  [`align_axes()`](https://agdamsbo.github.io/FreesearchR/reference/align_axes.md).
- *FIX* fixed axis text printed in Euler diagrams

## FreesearchR 25.7.2

- *FIX* refining hiding drop downs. All JavaScript is now in separate
  file. Coded with GAI help from claude.ai.

- *FIX* refined iconography and navigation

- *FIX* updated intro and docs.

## FreesearchR 25.7.1

- *NEW* UI overhaul and navigation update. The interface is simplified
  to clearly show the relationship between panels and sub-items by
  abandoning multiple levels on panel to instead show a drop-down menu.
  This also results in simplified sidebar menus with room to add more
  controls in the future.

## FreesearchR 25.6.4

The app is now also published as a docker container. See the README for
instructions. It is mainly to use for hosting the app. Work is ongoing
to publish a true standalone app, preferably for both Windows and MacOS.

- *FIX* improved plot labels.

## FreesearchR 25.6.3

- *NEW* Introducing more options to evaluate missing observations.
  Inspired by the \[visdat()\] function from the {visdat} package, a
  specialised function has been introduced to easily visualise data
  classes and missing observations in the data set. This highly
  increases the options to visually get an overview of the data and to
  assess the pattern of missing data. Also under Evaluate, a comparison
  module has been introduced to compare the distribution of observations
  across variables depending on the missing vs non-missing in a
  specified variable.

- *FIX* The REDCap import module has been updated visually and the PAI
  token is now hidden as a password. This module should still only be
  used when running locally if you are accessing sensitive data.

- minor rewordings and updated UI.

## FreesearchR 25.6.2

- *FIX* Added warning about only using REDCap with sensitive data
  running locally. THis applies to all data actually. Considering taking
  REDCap out in hosted version. Standalone app is in the works.

- *FIX* Reworded the completeness filter to be on missingness, as this
  is a more commonly used concept.

- *FIX* Improved layout around data filters to improve usage.

- *FIX* Regression table in report respects inclusion of p-values or
  not.

## FreesearchR 25.6.1

- *FIX* big not allowing to browse data

- *FIX* caught the last bugs when initiating the creation of new
  variables

## FreesearchR 25.5.6

- *FIX* note on max file size of 5 mb

- *FIX* added a banner to the dev version on shinyapps.io

- *FIX* updated intro text

## FreesearchR 25.5.5

- *FIX* several minor bugs and polish

- *FIX* include/exclude p-values in regression table.

## FreesearchR 25.5.4

- *FIX* correctly omit NAs in
  [`data_type()`](https://agdamsbo.github.io/FreesearchR/reference/data_type.md)
  call

- *FIX* omit NAs when plotting Euler diagrams.

- *FIX* print correct labels in horizontal stacked bars.

- *FIX* initial app load should feel faster.

## FreesearchR 25.5.3

- *FIX* a little polish on the data import

- *FIX* polished REDCap import and new code to reference the
  [`REDCapCAST::easy_redcap()`](https://agdamsbo.github.io/REDCapCAST/reference/easy_redcap.html)
  function.

- *FIX* updated documentation to reflect new private hosting on a
  Hetzner server in Germany.

## FreesearchR 25.5.2

- *FIX*: correct export of plots. The solution in the last version broke
  more than it solved.

- *NEW*: added simple loading animation.

A privately hosted version is now live on app.freesearchr.org. For now,
it is hosted on Hetzner with Yunohost.

## FreesearchR 25.5.1

- *FIX*: correct export of single variable plot.

- *NEW*: Include app version in report for reference.

- *NEW*: Show progress on connecting to a REDCap database.

- *FIX*: Data import code export.

## FreesearchR 25.4.5

- *BUG*: Regression results and code not returned correctly

- *IMPROVED*: analyses results are reset on data change

- *NEW*: app usage tracking only in hosted app. README updated to
  reflect.

## FreesearchR 25.4.4

Minor updates in docs and easier citation.

## FreesearchR 25.4.3

- *NEW*: Added a variables type filter to easily exclude unwanted types.
  This also includes having data type rather than data class in the
  summary table. Will evaluate. Types are a simpler, more practical
  version of the *R* data class to easy interpretation.

- *NEW*: A logo is here! It should emphasize the underlying reliance on
  *R* while also inspire to explore.

- *IMPROVED*: docs are updated and much more comprehensive. They will be
  continuously updated.

Polishing and moved hosted app to new address to fully reflect name
change: <https://agdamsbo.shinyapps.io/FreesearchR/>

## FreesearchR 25.4.2

Polished and simplified data import module including a much improved
REDCap import module.

- *CHANGE*
  [`default_parsing()`](https://agdamsbo.github.io/FreesearchR/reference/default_parsing.md)
  now ensure unique variable names.

- *NEW* Working code output for all major modules including import,
  modifications, filter, evaluation, plotting and regression. And it is
  nicely formatted!

- *NEW* The basics of a “Getting started”-vignette is done, and can be
  expanded on later.

## FreesearchR 25.4.1

Focus is on polish and improved ui/ux.

Updating project name to FreesearchR, with renamed repository. Graphics
are coming. This may introduce some breaking chances for others calling
or installing the package. No additional future changes are planned. A
complete transition is planned before attending and presenting a poster
at the European Stroke Organisation Conference 2025 in May.

Testing file upload conducted and improved.

Working on improving code export. This is very difficult to get perfect.
Initial focus is on extracting enough to be able to learn from it.

Regression calculations, plots, and checks have been improved and moved
to standalone module.

Data overview/modifications has been simplified slightly.
