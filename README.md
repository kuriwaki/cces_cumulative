CCES Cumulative File
================
Shiro Kuriwaki

[![](https://img.shields.io/badge/Dataverse%20DOI-10.7910/DVN/II2DB6-orange)](https://www.doi.org/10.7910/DVN/II2DB6)

This repository is R code to build the Cooperative Congressional
Election Study (CCES) cumulative file (2006 - 2022).

- [*Current Dataverse
  Version*](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/II2DB6)
- [*Current
  Guide*](https://github.com/kuriwaki/cces_cumulative/blob/main/guide/guide_cumulative_2006-2022.pdf)

Please feel free to file any questions or requests about the cumulative
file as [Github
issues](https://github.com/kuriwaki/cces_cumulative/issues).

# Getting Started

Start by downloading either the `.dta`, `.Rds`, or `.feather` file on
the [dataverse
page](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/II2DB6)
to your computer. This repository does not track the data due to size
constraints, but feel free to contact me if you need the newest version
not on Dataverse. The `.Rds` format can be read into R.

``` r
dat <- readRDS("cumulative_2006-2022.Rds")
```

Make sure to load the `tidyverse` package first. The Rds file can be
dealt with as a base-R data.frame, but it was built completely in the
`tidyverse` environment so using it as a `tibble` gives full features.

``` r
library(tidyverse)
dat
```

    ## # A tibble: 617,455 × 103
    ##     year case_id weight weight_cumulative state            st      cong  cong_up
    ##  * <int> <chr>    <dbl>             <dbl> <int+lbl>        <int+l> <fct> <fct>  
    ##  1  2006 439219   1.85              1.67  37 [North Carol… 37 [NC] 109   110    
    ##  2  2006 439224   0.968             0.872 39 [Ohio]        39 [OH] 109   110    
    ##  3  2006 439228   1.59              1.44  34 [New Jersey]  34 [NJ] 109   110    
    ##  4  2006 439237   1.40              1.26  17 [Illinois]    17 [IL] 109   110    
    ##  5  2006 439238   0.903             0.813 36 [New York]    36 [NY] 109   110    
    ##  6  2006 439242   0.839             0.756 48 [Texas]       48 [TX] 109   110    
    ##  7  2006 439251   0.777             0.700 27 [Minnesota]   27 [MN] 109   110    
    ##  8  2006 439254   0.839             0.756 32 [Nevada]      32 [NV] 109   110    
    ##  9  2006 439255   0.331             0.299 48 [Texas]       48 [TX] 109   110    
    ## 10  2006 439263   1.10              0.993 24 [Maryland]    24 [MD] 109   110    
    ## # ℹ 617,445 more rows
    ## # ℹ 95 more variables: state_post <int+lbl>, st_post <int+lbl>, dist <int>,
    ## #   dist_up <int>, cd <chr>, cd_up <chr>, dist_post <int>, dist_up_post <int>,
    ## #   cd_post <chr>, cd_up_post <chr>, zipcode <chr>, county_fips <chr>,
    ## #   tookpost <int+lbl>, weight_post <dbl>, rvweight <dbl>, rvweight_post <dbl>,
    ## #   starttime <dttm>, pid3 <int+lbl>, pid3_leaner <int+lbl>, pid7 <int+lbl>,
    ## #   ideo5 <fct>, gender <int+lbl>, sex <int+lbl>, gender4 <int+lbl>, …

A Stata `.dta` can also be read in by Stata, or in R through
`haven::read_dta()`. You will need the `haven` package loaded.

The arrow files can be loaded with `arrow::read_feather()`. They are
currently modeled so that it would give the same output as reading the
dta file.

Each row is a respondent, and each variable is information associated
with that respondent. Note that this cumulative dataset extracts only a
couple of key variables from each year’s CCES, which has hundreds of
columns.

# What’s New

## Unified Variable Names

Most variables in this dataset come straight from each year’s CCES.
However, it renames and standardizes variable names, making them
accessible in one place. Please see the guide or the Crunch dataset for
a full list and description of these variables.

## Candidate Names and Identifiers

The cumulative file has added candidate name and identifiers that a
respondent chose. In the original year-specific datasets, the response
values for a vote choice question is usually a generic label,
e.g. `Candidate1` and `Candidate2` (with separate look-up variables
appended). The cumulative dataset shows both the generic label *and* the
chosen candidate’s name, party, and identifier, which will vary across
individuals.

``` r
select(dat, year, case_id, matches("voted_sen"))
```

    ## # A tibble: 617,455 × 5
    ##     year case_id voted_sen                   voted_sen_party voted_sen_chosen   
    ##    <int> <chr>   <fct>                       <fct>           <chr>              
    ##  1  2006 439219  <NA>                        <NA>            <NA>               
    ##  2  2006 439224  [Democrat / Candidate 1]    Democratic      Sherrod C. Brown (…
    ##  3  2006 439228  [Democrat / Candidate 1]    Democratic      Robert Menendez (D)
    ##  4  2006 439237  <NA>                        <NA>            <NA>               
    ##  5  2006 439238  [Democrat / Candidate 1]    Democratic      Hillary Rodham Cli…
    ##  6  2006 439242  I Did Not Vote In This Race <NA>            <NA>               
    ##  7  2006 439251  [Republican / Candidate 2]  Republican      Mark Kennedy (R)   
    ##  8  2006 439254  [Democrat / Candidate 1]    Democratic      Jack Carter (D)    
    ##  9  2006 439255  [Democrat / Candidate 1]    Democratic      Barbara Ann Radnof…
    ## 10  2006 439263  I Did Not Vote In This Race <NA>            <NA>               
    ## # ℹ 617,445 more rows

## Crunch

A version of the dataset is also included in Crunch, a database platform
that makes it easy to view and analyze survey data either with our
without any programming experience. For access to View the dataset
(free), please sign up here:
<https://harvard.az1.qualtrics.com/jfe/form/SV_066hQi4Eeco3Kap>. Some
features include:

### A web GUI for quickly browsing variables

![Browse Variables with Crunch](guide/01_crunch_browse.gif)

### Quickly check cross-tabs and bar graphs, with customizable formatting

![Cross-tabulate Variables with Crunch](guide/02_crunch_tab.gif)

### Sharable widgets.

For questions and more access, please contact the CCES Team.

Crunch datasets can also be manipulated from a R package, `crunch`:
<https://github.com/Crunch-io/rcrunch>.

``` r
install.packages("crunch")
```

For a bit more on using the R crunch package for your own purposes, see
the crunch package vignettes, pkgdown website, or a [short vignette in
this
repo](https://github.com/kuriwaki/cces_cumulative/blob/master/guide/vignette_crunch.md).

# Organization of Scripts

R scripts `01` - `07` reproduce the cumulative dataset starting from
each year’s CCES on dataverse.

- `01_define-names-labels.R` constructs two variable name tables – one
  that names and describes each variable to be in the final dataset, and
  another that indicates which variables corresponds to the candidate
  columns in each year’s CCES.
- `02_download-cces-dataverse.R` indicates a (partial) way to download
  the component CCES data from dataverse so that the rest of the code
  can be run.
- `03_read-common.R` pulls out the common contents with minimal
  formatting (e.g. state, case identifier variable names)
- `04_prepare-fixes.R` makes some fixes to variables in each year’s
  datasets.
- `05_stack-cumulative.R` pulls out the variables of interest from
  annual CCES files, we stack this into a long dataset where each row is
  a respondent from CCES.
- `06_extract_politicians.R` pulls out the “contextual variables” at the
  respondent-level. information on candidates and representatives. It
  uses some long format voting tables from `05`.
- `07_merge-contextual_upload.R` combines all the variables together,
  essentially combining the output of `04` on `05`. Saves a `.Rds` and
  `sav` version.
- `08_format-crunch.R` logs into Crunch, and adds variable names,
  descriptions, groupings, and other Crunch attributes to the Crunch
  dataset. It also adds variables and exports a `.dta` version

More scripts are in `00_prepare`, they format other datasets like
NOMINATE, CQ, and DIME.
