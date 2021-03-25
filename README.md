CCES Cumulative File
================
Shiro Kuriwaki

[![](https://img.shields.io/badge/Dataverse%20DOI-10.7910/DVN/II2DB6-orange)](https://www.doi.org/10.7910/DVN/II2DB6)

This repository is R code to build the Cooperative Congressional
Election Study (CCES) cumulative file (2006 - 2019).

-   [*Current Dataverse
    Version*](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/II2DB6)
-   [*Current
    Guide*](https://github.com/kuriwaki/cces_cumulative/blob/master/guide/guide_cumulative_2006_2019.pdf)

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
df <- readRDS("cumulative_2006-2019.Rds")
```

Make sure to load the `tidyverse` package first. The Rds file can be
dealt with as a base-R data.frame, but it was built completely in the
`tidyverse` environment so using it as a `tibble` gives full features.

``` r
library(tidyverse)
df
```

    ## # A tibble: 470,755 x 91
    ##     year case_id weight weight_cumulative state   st     cong cong_up state_post
    ##    <int>   <int>  <dbl>             <dbl> <chr>   <chr> <int>   <int> <chr>     
    ##  1  2006  439219  1.85              1.35  North … NC      109     110 North Car…
    ##  2  2006  439224  0.968             0.704 Ohio    OH      109     110 Ohio      
    ##  3  2006  439228  1.59              1.16  New Je… NJ      109     110 New Jersey
    ##  4  2006  439237  1.40              1.02  Illino… IL      109     110 Illinois  
    ##  5  2006  439238  0.903             0.656 New Yo… NY      109     110 New York  
    ##  6  2006  439242  0.839             0.610 Texas   TX      109     110 Texas     
    ##  7  2006  439251  0.777             0.565 Minnes… MN      109     110 Minnesota 
    ##  8  2006  439254  0.839             0.610 Nevada  NV      109     110 Nevada    
    ##  9  2006  439255  0.331             0.241 Texas   TX      109     110 Texas     
    ## 10  2006  439263  1.10              0.802 Maryla… MD      109     110 Maryland  
    ## # … with 470,745 more rows, and 82 more variables: st_post <chr>, dist <int>,
    ## #   dist_up <int>, cd <chr>, cd_up <chr>, dist_post <int>, dist_up_post <int>,
    ## #   cd_post <chr>, cd_up_post <chr>, zipcode <chr>, county_fips <chr>,
    ## #   tookpost <int+lbl>, weight_post <dbl>, rvweight <dbl>, rvweight_post <dbl>,
    ## #   starttime <dttm>, pid3 <int+lbl>, pid3_leaner <int+lbl>, pid7 <int+lbl>,
    ## #   ideo5 <fct>, gender <int+lbl>, birthyr <int>, age <int>, race <int+lbl>,
    ## #   hispanic <int+lbl>, citizen <dbl+lbl>, educ <int+lbl>, marstat <int+lbl>,
    ## #   faminc <fct>, union <int+lbl>, union_hh <int+lbl>, employ <fct>,
    ## #   no_healthins <fct>, has_child <fct>, ownhome <fct>, no_milstat <fct>,
    ## #   religion <int+lbl>, economy_retro <int+lbl>, newsint <int+lbl>,
    ## #   approval_pres <int+lbl>, approval_rep <fct>, approval_sen1 <fct>,
    ## #   approval_sen2 <fct>, approval_gov <int+lbl>, intent_pres_08 <fct>,
    ## #   intent_pres_12 <fct>, intent_pres_16 <fct>, voted_pres_08 <fct>,
    ## #   voted_pres_12 <fct>, voted_pres_16 <fct>, intent_pres_party <fct>,
    ## #   voted_pres_party <fct>, vv_regstatus <fct>, vv_party_gen <fct>,
    ## #   vv_party_prm <fct>, vv_turnout_gvm <fct>, vv_turnout_pvm <fct>,
    ## #   intent_rep <fct>, intent_rep_party <fct>, voted_rep <fct>,
    ## #   voted_rep_party <fct>, intent_gov <fct>, intent_gov_party <fct>,
    ## #   voted_gov <fct>, voted_gov_party <fct>, intent_sen <fct>,
    ## #   intent_sen_party <fct>, voted_sen <fct>, voted_sen_party <fct>,
    ## #   intent_rep_chosen <chr>, intent_sen_chosen <chr>, intent_gov_chosen <chr>,
    ## #   voted_rep_chosen <chr>, voted_sen_chosen <chr>, voted_gov_chosen <chr>,
    ## #   rep_current <chr>, rep_icpsr <dbl>, sen1_current <chr>, sen1_icpsr <dbl>,
    ## #   sen2_current <chr>, sen2_icpsr <dbl>, gov_current <chr>

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
values for a vote choice question is usually a generic label,
e.g. `Candidate1` and `Candidate2` (with separate look-up variables
appended). The cumulative dataset shows both the generic label *and* the
chosen candidate’s name, party, and identifier, which will vary across
individuals.

``` r
select(df, year, case_id, matches("voted_sen"))
```

    ## # A tibble: 470,755 x 5
    ##     year case_id voted_sen               voted_sen_party voted_sen_chosen       
    ##    <int>   <int> <fct>                   <fct>           <chr>                  
    ##  1  2006  439219 <NA>                    <NA>            <NA>                   
    ##  2  2006  439224 [Democrat / Candidate … Democratic      Sherrod C. Brown (D)   
    ##  3  2006  439228 [Democrat / Candidate … Democratic      Robert Menendez (D)    
    ##  4  2006  439237 <NA>                    <NA>            <NA>                   
    ##  5  2006  439238 [Democrat / Candidate … Democratic      Hillary Rodham Clinton…
    ##  6  2006  439242 I Did Not Vote In This… <NA>            <NA>                   
    ##  7  2006  439251 [Republican / Candidat… Republican      Mark Kennedy (R)       
    ##  8  2006  439254 [Democrat / Candidate … Democratic      Jack Carter (D)        
    ##  9  2006  439255 [Democrat / Candidate … Democratic      Barbara Ann Radnofsky …
    ## 10  2006  439263 I Did Not Vote In This… <NA>            <NA>                   
    ## # … with 470,745 more rows

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

R scripts `01` - `06` reproduce the cumulative dataset starting from
each year’s CCES on dataverse.

-   `01_define-names-labels.R` constructs two variable name tables – one
    that names and describes each variable to be in the final dataset,
    and another that indicates which variables corresponds to the
    candidate columns in each year’s CCES.
-   `02_download-cces-dataverse.R` indicates a (partial) way to download
    the component CCES data from dataverse so that the rest of the code
    can be run.
-   `03_read-common.R` pulls out the common contents with minimal
    formatting (e.g. state, case identifier variable names)
-   `04_prepare-fixes.R` makes some fixes to variables in each year’s
    datasets.
-   `05_stack-cumulative.R` pulls out the variables of interest from
    annual CCES files, we stack this into a long dataset where each row
    is a respondent from CCES.
-   `06_extract_politicians.R` pulls out the “contextual variables” at
    the respondent-level. information on candidates and representatives.
    It uses some long format voting tables from `05`.
-   `07_merge-contextual_upload.R` combines all the variables together,
    essentially combining the output of `04` on `05`. Saves a `.Rds` and
    `sav` version.
-   `08_format-crunch.R` logs into Crunch, and adds variable names,
    descriptions, groupings, and other Crunch attributes to the Crunch
    dataset. It also adds variables and exports a `.dta` version

More scripts are in `00_prepare`, they format other datasets like
NOMINATE, CQ, and DIME.
