# freesearcheR 25.3.2

Focus is on polish and improved ui/ux.

First steps towards an updated name (will be FreesearchR), with renamed repository. This may introduce some breaking chances for others calling or installing the package. No future changes are planned. A complete transition is planned before attending and presenting a poster at the European Stroke Organisation COnference 2025 in May.

Testing file upload conducted and improved.

Working on improving code export.

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
