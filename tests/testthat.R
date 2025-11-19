# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(FreesearchR)
library(shiny)


i18n <- shiny.i18n::Translator$new(translation_csvs_path = here::here("inst/translations/"))
i18n$set_translation_language("en")


test_check("FreesearchR")
