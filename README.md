CCES cumulative
================

Directory for building a CCES cumulative file (2006 - 2016) collaboratively. Follow the development of dataset building at [this Github page](https://github.com/kuriwaki/cces_cumulative). Project page of tasks [here](https://github.com/kuriwaki/cces_cumulative/projects/1).

A 2006-2012 cumulative file, as well as datasets from individual years, can be found at the CCES homepage: <https://cces.gov.harvard.edu/>

Please feel free to file any questions or requests about the cumulative file as [Github issues](https://github.com/kuriwaki/cces_cumulative/issues).

Data is currently not tracked, but releases will be made as flat files and a [Crunch dataset](crunch.io). For an intermediate dataset, please sign up here: <https://harvard.az1.qualtrics.com/jfe/form/SV_066hQi4Eeco3Kap>.

Getting Started
===============

The `.Rds` format can be read into R. This format preserves dataset properties such as the distinction between integers and doubles, and labelled variables.

``` r
df <- readRDS("cumulative_2006_2016.Rds")
```

``` r
library(tidyverse)
df
```

    ## # A tibble: 374,556 x 67
    ##     year case_id weight weight_cumulative state  st    CD     dist dist_up
    ##    <int>   <int>  <dbl>             <dbl> <chr>  <chr> <chr> <int>   <int>
    ##  1  2006  439219  1.85              1.67  North… NC    NC-10    10      10
    ##  2  2006  439224  0.968             0.872 Ohio   OH    OH-3      3       3
    ##  3  2006  439228  1.59              1.44  New J… NJ    NJ-1      1       1
    ##  4  2006  439237  1.40              1.26  Illin… IL    IL-9      9       9
    ##  5  2006  439238  0.903             0.813 New Y… NY    NY-22    22      22
    ##  6  2006  439242  0.839             0.756 Texas  TX    TX-11    11      11
    ##  7  2006  439251  0.777             0.700 Minne… MN    MN-3      3       3
    ##  8  2006  439254  0.839             0.756 Nevada NV    NV-2      2       2
    ##  9  2006  439255  0.331             0.299 Texas  TX    TX-24    24      24
    ## 10  2006  439263  1.10              0.993 Maryl… MD    MD-2      2       2
    ## # ... with 374,546 more rows, and 58 more variables: cong <int>, cong_up
    ## #   <int>, zipcode <chr>, countyFIPS <chr>, tookpost <int+lbl>, weight_vv
    ## #   <dbl>, weight_vv_post <dbl>, starttime <dttm>, pid3 <int+lbl>, pid7
    ## #   <int+lbl>, gender <int+lbl>, birthyr <int>, age <int>, race <int+lbl>,
    ## #   educ <int+lbl>, economy_retro <int+lbl>, approval_pres <int+lbl>,
    ## #   approval_rep <fct>, approval_sen1 <fct>, approval_sen2 <fct>,
    ## #   approval_gov <int+lbl>, intent_pres_08 <fct>, intent_pres_12 <fct>,
    ## #   intent_pres_16 <fct>, voted_pres_08 <fct>, voted_pres_12 <fct>,
    ## #   voted_pres_16 <fct>, vv_regstatus <fct>, vv_party_gen <fct>,
    ## #   vv_party_prm <fct>, vv_turnout_gvm <fct>, vv_turnout_pvm <fct>,
    ## #   intent_rep <fct>, intent_sen <fct>, intent_gov <fct>, voted_rep <fct>,
    ## #   voted_sen <fct>, voted_gov <fct>, intent_rep_chosen <chr>,
    ## #   intent_rep_fec <chr>, intent_sen_chosen <chr>, intent_sen_fec <chr>,
    ## #   intent_gov_chosen <chr>, intent_gov_fec <chr>, voted_rep_chosen <chr>,
    ## #   voted_rep_fec <chr>, voted_sen_chosen <chr>, voted_sen_fec <chr>,
    ## #   voted_gov_chosen <chr>, voted_gov_fec <chr>, rep_current <chr>,
    ## #   rep_icpsr <int>, sen1_current <chr>, sen1_icpsr <int>, sen2_current
    ## #   <chr>, sen2_icpsr <int>, gov_current <chr>, gov_fec <chr>

What's New
==========

Unified Variable Names
----------------------

Candidate Names and Identifiers
-------------------------------

One addition to this cumulative dataset above the individual years that comprise it is the addition of candidate name and identifiers that a respondent chose. In the individual year's CCES datasets, typically the response values for a vote choice question is a generic label, e.g. `Candidate1` and `Candidate2`. Then, separate variables with those names are appended, requiring users to look-up the relevant candidate for each respondent. The cumulative dataset shows both the generic label *and* the chosen candidate's name, party, and identifier, which will vary across individuals.

``` r
select(df, year, case_id, st, matches("voted_sen"))
```

    ## # A tibble: 374,556 x 6
    ##     year case_id st    voted_sen         voted_sen_chosen    voted_sen_fec
    ##    <int>   <int> <chr> <fct>             <chr>               <chr>        
    ##  1  2006  439219 NC    <NA>              <NA>                <NA>         
    ##  2  2006  439224 OH    [Democrat / Cand… Sherrod C. Brown (… S6OH00163    
    ##  3  2006  439228 NJ    [Democrat / Cand… Robert Menendez (D) S6NJ00289    
    ##  4  2006  439237 IL    <NA>              <NA>                <NA>         
    ##  5  2006  439238 NY    [Democrat / Cand… Hillary Rodham Cli… S0NY00188    
    ##  6  2006  439242 TX    I Did Not Vote I… <NA>                <NA>         
    ##  7  2006  439251 MN    [Republican / Ca… Mark Kennedy (R)    S6MN00275    
    ##  8  2006  439254 NV    [Democrat / Cand… Jack Carter (D)     S6NV00150    
    ##  9  2006  439255 TX    [Democrat / Cand… Barbara Ann Radnof… S6TX00180    
    ## 10  2006  439263 MD    I Did Not Vote I… <NA>                <NA>         
    ## # ... with 374,546 more rows

Crunch
------

Code Organization
=================

R scripsts `01` - `06` reproduce the cumulative dataset starting from each year's CCES on dataverse.

-   `01_define-names-labels.R` constructs two variable name tables -- one that names andibes each variable to be in the final dataset, and another that indicates which varalias corresponds to the candidate columns in each year's CCES.
-   `02_read-common.R` pulls out the common contents with minimal foramtting (e.g. staze record ID variable names)
-   `03_prepare-fixes.R` makes some fixes to variables in each year's datasets.
-   `04_stack-cumulative.R` pulls out the variabales of interest from annual CCES filen, we stack this into a long dataset where each row is a respondent from CCES.
-   `05_extract_contextual.R` pulls out the "contextual variables" at the respondent leve. information on candidates and representatives. It uses some long format voting restables from `04`.
-   `06_merge-contextual_upload.R` combines all the variables together, essentially colnding the output of `04` on `05`. Saves a `.Rds` and `sav` version.
-   `07_format-crunch.R` logs into Crunch, and adds variable names, descriptions, groupings, and other Crunch attributes to the Crunch dataset. It also adds variables and exports a `.dta` version

More scripts in `00_prepare` format other datasets like NOMINATE, CQ, and DIME.
