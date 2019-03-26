CCES Cumulative File
================
Shiro Kuriwaki

This repository is R code to build the Cooperative Congressional
Election Study (CCES) cumulative file (2006 - 2017).

2019-03-26 Note: I am actively finalizing the **2006 - 2018** cumulative
dataset on this repo. Please contact me for any new additions you might
want to see in this next release.

  - [*Current Dataverse
    Version*](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/II2DB6)
  - [*Current
    Guide*](https://github.com/kuriwaki/cces_cumulative/blob/master/guide/guide_cumulative_2006_2017.pdf)

Please feel free to file any questions or requests about the cumulative
file as [Github
issues](https://github.com/kuriwaki/cces_cumulative/issues).

# Getting Started

Start by downloading either the `.Rds` or `.dta` file on the [dataverse
page](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/II2DB6)
to your computer. This repository does not track the data due to size
constraints, but please contact me for the newest version. The `.Rds`
format can be read into R.

``` r
df <- readRDS("cumulative_2006_2018.Rds")
```

Make sure to load the `tidyverse` package first. The Rds file can be
dealt with as a base-R data.frame, but it was built completely in the
`tidyverse` environment so using it as a `tibble` gives full features.

``` r
library(tidyverse)
df
```

    ## # A tibble: 452,755 x 78
    ##     year case_id weight weight_cumulati… state st    cd     dist dist_up
    ##    <int>   <int>  <dbl>            <dbl> <chr> <chr> <chr> <int>   <int>
    ##  1  2006  439219  1.85             1.35  Nort… NC    NC-10    10      10
    ##  2  2006  439224  0.968            0.704 Ohio  OH    OH-3      3       3
    ##  3  2006  439228  1.59             1.16  New … NJ    NJ-1      1       1
    ##  4  2006  439237  1.40             1.02  Illi… IL    IL-9      9       9
    ##  5  2006  439238  0.903            0.656 New … NY    NY-22    22      22
    ##  6  2006  439242  0.839            0.610 Texas TX    TX-11    11      11
    ##  7  2006  439251  0.777            0.565 Minn… MN    MN-3      3       3
    ##  8  2006  439254  0.839            0.610 Neva… NV    NV-2      2       2
    ##  9  2006  439255  0.331            0.241 Texas TX    TX-24    24      24
    ## 10  2006  439263  1.10             0.802 Mary… MD    MD-2      2       2
    ## # … with 452,745 more rows, and 69 more variables: cong <int>,
    ## #   cong_up <int>, zipcode <chr>, county_fips <chr>, tookpost <int+lbl>,
    ## #   weight_post <dbl>, starttime <dttm>, pid3 <int+lbl>,
    ## #   pid3_leaner <int+lbl>, pid7 <int+lbl>, ideo5 <fct>, gender <int+lbl>,
    ## #   birthyr <int>, age <int>, race <int+lbl>, hispanic <int+lbl>,
    ## #   educ <int+lbl>, faminc <fct>, marstat <int+lbl>,
    ## #   economy_retro <int+lbl>, newsint <int+lbl>, approval_pres <int+lbl>,
    ## #   approval_rep <fct>, approval_sen1 <fct>, approval_sen2 <fct>,
    ## #   approval_gov <int+lbl>, intent_pres_08 <fct>, intent_pres_12 <fct>,
    ## #   intent_pres_16 <fct>, voted_pres_08 <fct>, voted_pres_12 <fct>,
    ## #   voted_pres_16 <fct>, vv_regstatus <fct>, vv_party_gen <fct>,
    ## #   vv_party_prm <fct>, vv_turnout_gvm <fct>, vv_turnout_pvm <fct>,
    ## #   intent_rep <fct>, intent_rep_party <fct>, voted_rep <fct>,
    ## #   voted_rep_party <fct>, intent_gov <fct>, intent_gov_party <fct>,
    ## #   voted_gov <fct>, voted_gov_party <fct>, intent_sen <fct>,
    ## #   intent_sen_party <fct>, voted_sen <fct>, voted_sen_party <fct>,
    ## #   intent_rep_chosen <chr>, intent_rep_fec <chr>,
    ## #   intent_sen_chosen <chr>, intent_sen_fec <chr>,
    ## #   intent_gov_chosen <chr>, intent_gov_fec <chr>, voted_rep_chosen <chr>,
    ## #   voted_rep_fec <chr>, voted_sen_chosen <chr>, voted_sen_fec <chr>,
    ## #   voted_gov_chosen <chr>, voted_gov_fec <chr>, rep_current <chr>,
    ## #   rep_icpsr <int>, sen1_current <chr>, sen1_icpsr <int>,
    ## #   sen2_current <chr>, sen2_icpsr <int>, gov_current <chr>, gov_fec <chr>

A Stata `.dta` can also be read in by Stata, or in R through
`haven::read_dta()`.

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
values for a vote choice question is usually a generic label, e.g.
`Candidate1` and `Candidate2` (with separate look-up variables
appended). The cumulative dataset shows both the generic label *and* the
chosen candidate’s name, party, and identifier, which will vary across
individuals.

``` r
select(df, year, case_id, matches("voted_sen"))
```

    ## # A tibble: 452,755 x 6
    ##     year case_id voted_sen   voted_sen_party voted_sen_chosen voted_sen_fec
    ##    <int>   <int> <fct>       <fct>           <chr>            <chr>        
    ##  1  2006  439219 <NA>        <NA>            <NA>             <NA>         
    ##  2  2006  439224 [Democrat … Democratic      Sherrod C. Brow… S6OH00163    
    ##  3  2006  439228 [Democrat … Democratic      Robert Menendez… S6NJ00289    
    ##  4  2006  439237 <NA>        <NA>            <NA>             <NA>         
    ##  5  2006  439238 [Democrat … Democratic      Hillary Rodham … S0NY00188    
    ##  6  2006  439242 I Did Not … <NA>            <NA>             <NA>         
    ##  7  2006  439251 [Republica… Republican      Mark Kennedy (R) S6MN00275    
    ##  8  2006  439254 [Democrat … Democratic      Jack Carter (D)  S6NV00150    
    ##  9  2006  439255 [Democrat … Democratic      Barbara Ann Rad… S6TX00180    
    ## 10  2006  439263 I Did Not … <NA>            <NA>             <NA>         
    ## # … with 452,745 more rows

## Crunch

A version of the dataset is also included in Crunch, a database platform
that makes it easy to view and analyze survey data either with our
without any programming experience. For access to View the dataset
(free), please sign up here:
<https://harvard.az1.qualtrics.com/jfe/form/SV_066hQi4Eeco3Kap>. Some
features include:

### A web GUI for quickly browsing variables

![Browse Variables with
Crunch](guide/01_crunch_browse.gif)

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

R scripts `01` - `06` reproduce the cumulative dataset starting from
each year’s CCES on dataverse.

  - `01_define-names-labels.R` constructs two variable name tables – one
    that names and describes each variable to be in the final dataset,
    and another that indicates which variables corresponds to the
    candidate columns in each year’s CCES.
  - `02_read-common.R` pulls out the common contents with minimal
    formatting (e.g. state, case identifier variable names)
  - `03_prepare-fixes.R` makes some fixes to variables in each year’s
    datasets.
  - `04_stack-cumulative.R` pulls out the variables of interest from
    annual CCES files, we stack this into a long dataset where each row
    is a respondent from CCES.
  - `05_extract_contextual.R` pulls out the “contextual variables” at
    the respondent-level. information on candidates and representatives.
    It uses some long format voting tables from `04`.
  - `06_merge-contextual_upload.R` combines all the variables together,
    essentially combining the output of `04` on `05`. Saves a `.Rds` and
    `sav` version.
  - `07_format-crunch.R` logs into Crunch, and adds variable names,
    descriptions, groupings, and other Crunch attributes to the Crunch
    dataset. It also adds variables and exports a `.dta` version

More scripts are in `00_prepare`, they format other datasets like
NOMINATE, CQ, and DIME.
