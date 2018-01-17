CCES cumulative
================
Shiro Kuriwaki

Source code for building a CCES cumulative file (2006 - 2016). Please feel free to file any questions or requests about the cumulative file as [Github issues](https://github.com/kuriwaki/cces_cumulative/issues).

A 2006-2012 cumulative file, as well as datasets from individual years, can be found at the CCES homepage: <https://cces.gov.harvard.edu/>

Data is currently not tracked, but releases will be made as flat files in the CCES [Dataverse](https://dataverse.harvard.edu/dataverse/cces) and a [Crunch dataset](crunch.io).

Getting Started
===============

The `.Rds` format can be read into R[1]

``` r
df <- readRDS("cumulative_2006_2016.Rds")
```

``` r
library(tidyverse)
df
```

    ## # A tibble: 374,556 x 67
    ##     year case_id weight weight_cumulative state  st    cd     dist dist_up
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
    ## #   <int>, zipcode <chr>, county_fips <chr>, tookpost <int+lbl>, weight_vv
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

Each row is a respondent, and each variable is information associated with that respondent. Note that this cumulative dataset extracts only a couple of key variables from each year's CCES, which has hundreds of columns.

What's New
==========

Unified Variable Names
----------------------

Most variables in this dataset come straight from each year's CCES. However, it renames and standardizes variable names, making them accessible in one place.

Please see the guide or the Crunch dataset for a full list and description of variables.

Candidate Names and Identifiers
-------------------------------

One addition to this cumulative dataset above the individual years that comprise it is the addition of candidate name and identifiers that a respondent chose. In the individual year's CCES datasets, typically the response values for a vote choice question is a generic label, e.g. `Candidate1` and `Candidate2`. Then, separate variables with those names are appended, requiring users to look-up the relevant candidate for each respondent. The cumulative dataset shows both the generic label *and* the chosen candidate's name, party, and identifier, which will vary across individuals.

``` r
select(df, year, case_id, matches("voted_sen"))
```

    ## # A tibble: 374,556 x 5
    ##     year case_id voted_sen            voted_sen_chosen       voted_sen_fec
    ##    <int>   <int> <fct>                <chr>                  <chr>        
    ##  1  2006  439219 <NA>                 <NA>                   <NA>         
    ##  2  2006  439224 [Democrat / Candida… Sherrod C. Brown (D)   S6OH00163    
    ##  3  2006  439228 [Democrat / Candida… Robert Menendez (D)    S6NJ00289    
    ##  4  2006  439237 <NA>                 <NA>                   <NA>         
    ##  5  2006  439238 [Democrat / Candida… Hillary Rodham Clinto… S0NY00188    
    ##  6  2006  439242 I Did Not Vote In T… <NA>                   <NA>         
    ##  7  2006  439251 [Republican / Candi… Mark Kennedy (R)       S6MN00275    
    ##  8  2006  439254 [Democrat / Candida… Jack Carter (D)        S6NV00150    
    ##  9  2006  439255 [Democrat / Candida… Barbara Ann Radnofsky… S6TX00180    
    ## 10  2006  439263 I Did Not Vote In T… <NA>                   <NA>         
    ## # ... with 374,546 more rows

Crunch
------

A version of the dataset is also included in Crunch, a database platform that makes it easy to view and analyze survey data either with our without any programming experience. For access to View the dataset (free), please sign up here: <https://harvard.az1.qualtrics.com/jfe/form/SV_066hQi4Eeco3Kap>. Some features include:

### A web GUI that is accessible anywhere and can quickly browse variables

![Browse Variables with Crunch](guide/01_crunch_browse.gif)

### Quick interfaces to show cross-tabs and bar graphs with the data, and customize formatting

![Cross-tabulate Variables with Crunch](guide/02_crunch_tab.gif)

### Sharable widgets.

For questions and more access, please contact the CCES Team.

Code Organization
=================

R scripts `01` - `06` reproduce the cumulative dataset starting from each year's CCES on dataverse.

-   `01_define-names-labels.R` constructs two variable name tables -- one that names and describes each variable to be in the final dataset, and another that indicates which variables corresponds to the candidate columns in each year's CCES.
-   `02_read-common.R` pulls out the common contents with minimal formatting (e.g. state, case identifier variable names)
-   `03_prepare-fixes.R` makes some fixes to variables in each year's datasets.
-   `04_stack-cumulative.R` pulls out the variables of interest from annual CCES files, we stack this into a long dataset where each row is a respondent from CCES.
-   `05_extract_contextual.R` pulls out the "contextual variables" at the respondent-level. information on candidates and representatives. It uses some long format voting tables from `04`.
-   `06_merge-contextual_upload.R` combines all the variables together, essentially combining the output of `04` on `05`. Saves a `.Rds` and `sav` version.
-   `07_format-crunch.R` logs into Crunch, and adds variable names, descriptions, groupings, and other Crunch attributes to the Crunch dataset. It also adds variables and exports a `.dta` version

More scripts in `00_prepare` format other datasets like NOMINATE, CQ, and DIME.

Acknowledgements
================

Thanks to the contributors of this project: Steve Ansolabehere, Stephen DiMauro, Nathan Kaplan, and Joe Williams.

I also relied on compiled datasets to build off of:

-   Ansolabehere, Stephen; Pettigrew, Stephen, 2014, ["Cumulative CCES Common Content (2006-2012)"](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/26451), <doi:10.7910/DVN/26451>, Harvard Dataverse, V5, UNF:5:rXSA73aoDi28uu+IOg7DEg==
-   Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet (2017). ["Voteview: Congressional Roll-Call Votes Database"](https://voteview.com).
-   Bonica, Adam , 2015, ["Database on Ideology, Money in Politics, and Elections (DIME)"](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/O5PX0B), <doi:10.7910/DVN/O5PX0B>, Harvard Dataverse, V2

------------------------------------------------------------------------

[1] In contrast to a csv, this R format preserves dataset properties such as the distinction between integers and doubles, and labelled variables.
