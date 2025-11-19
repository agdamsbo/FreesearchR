# Wrapper to ease data file import

Wrapper to ease data file import

## Usage

``` r
import_delim(file, skip, encoding, na.strings)

import_xls(file, sheet, skip, na.strings)

import_ods(file, sheet, skip, na.strings)

import_dta(file)

import_rds(file)
```

## Arguments

- file:

  path to the file

- skip:

  number of row to skip

- encoding:

  file encoding

- na.strings:

  character(s) to interpret as missing values.

- sheet:

  for Excel files, sheet to read

## Value

data.frame

data.frame

data.frame

data.frame

data.frame
