[![ACTUS](https://github.com/actusfrf/actus-resources/blob/master/logos/actus_logo.jpg "ACTUS Financial Research Foundation")](https://www.actusfrf.org)

ACTUS Dictionary
=======

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)

The ACTUS Dictionary provides the definition of terms of the ACTUS standard for the algorithmic representation of financial contracts.

It comes in two forms; Excel for human readability and JSON for programmatic integration.

# Generate JSON dictionary

The JSON dictionary (actus-dictionary.json) is derived from the Excel dictionary (actus-dictionary.xlsx) through the R script in the scripts folder. Compute the JSON dictionary as described below.

## Requirements
* `java`: v8.0
* `R`: v3.4.4
* `jsonlite`: from CRAN
* `magrittr`: from CRAN
* `readr`: from CRAN
* `xlsx`: from CRAN

## Execute
```sh
# scripts/
Rscript create-dictionary.R
```
