CCES cumulative
================

Directory for building a CCES cumulative file (2006 - 2016) collaboratively. Follow the development of dataset building at [this Github page](https://github.com/kuriwaki/cces_cumulative). Project page of tasks [here](https://github.com/kuriwaki/cces_cumulative/projects/1).

A 2006-2012 cumulative file, as well as datasets from individual years, can be found at the CCES homepage: <https://cces.gov.harvard.edu/>

Please feel free to file any questions or requests about the cumulative file as [Github issues](https://github.com/kuriwaki/cces_cumulative/issues).

Data is currently not tracked, but releases will be made as flat files and a [Crunch dataset](crunch.io). For an intermediate dataset, please [email](mailto:kuriwaki@g.harvard.edu).

Structure of the directory
--------------------------

### code

-   `build` stores code that is used to wrangle data and build datasets
-   `analyze` stores code to visualize and analyze data.
-   `old_code` for code that is out of use

### data

-   `data/source` is where all the input data is. Things in here should not be overwritten. Instead, manipulated output should go in `data/output`.
-   `data/source/cces` stores all the CCES common content material.
-   `data/source/census` stores all the census-related data, and so on.

Note: most *data* files are not tracked on git.

### other references

-   `figures` for generated figures
-   `tables` for generated tables
-   `guides` stores all the codebooks, guides, etc.. to explain the common content.

Main code
---------

### CCES

-   `build/01_read-common.R` pulls out the common contents with minimal foramtting (e.g. standardize record ID variable names)
-   `build/02_pull-08to11.R` pulls out some variables that we need separately from 2008 - 2011 CCES common content, that was masked in the 2006-2012 cumulative file.
-   `build/03_stack-cumulative.R` pulls out the variabales of interest from annual CCES files. Then, we stack this into a long dataset where each row is a respondent from CCES. This is uploaded to Crunch.
-   `build/04_extract_contextual.R` pulls out the "contextual variables" at the respondent level, i.e. information on candidates and representatives
-   `build/09_format-crunch.R` logs into Crunch, and adds variable names, descriptions, groupings, and other Crunch attributes to the Crunch dataset.

### Contextual

-   `11_build-voteview.R` formats datasets of incumbent MCs
-   `12_build_CQ.R` formats data from CQ Alamnac on MCs biographical information
-   `13_build-FEC.R` formats FEC identifier keys for all candidates in the FEC database, using Bonica's DIME databaset
-   `14_join-incumbents.R` combines voteview, CQ, and FEC data for incumbents for congress, then to responses
-   `15_join-challengers.R` combines FEC and district data for challengers for congress
-   `16_join-governors.R` combines FEC and responses for Governor contextual data
-   `17_join-contextual.R` combines all respondent-level contextual info in one long dataset

Crunch
======

Crunch is a interface for viewing and manipulating datasets.

``` r
library(crunch)
```

    ## 
    ## Attaching package: 'crunch'

    ## The following object is masked from 'package:utils':
    ## 
    ##     write.csv

    ## The following object is masked from 'package:base':
    ## 
    ##     table

For example, start with a survey you already have

``` r
library(haven)
library(curl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:crunch':
    ## 
    ##     combine, id

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tibble)

# dataset
cc12_link <- curl("www.shirokuriwaki.com/datasets/cces_sample.dta")
cc12 <- read_dta(cc12_link) %>%
  mutate_if(is.labelled, as_factor) # switch lablled to factor b/c crunch doesn't accept
```

You might have meta-data that accompanies this.

``` r
cc12_meta <- tribble(
  ~alias, ~type, ~name, ~description,
  "state", "categorical", "State", "[State]",
  "age", "numeric", "Age", "How old are you?",
  "gender", "categorical", "Gender", "What is your Gender?",
  "race", "categorical", "Race", "What racial or ethnic group best describes you?",
  "employ", "categorical", "Employment Status", "Which of the following best describes your current employment status?",
  "family_income", "categorical", "Family Income", "Thinking back over the last year, what was your family's annual income?",
  "religion", "categorical", "Religion", "What is your present religion, if any?",
  "obama12", "numeric", "Obama Vote", "[vote for Obama]"
)
```

Now login to crunch

``` r
login()
```

    ## Logged into crunch.io as kuriwaki@g.harvard.edu

Upload the dataset to crunch (do this only once)

``` r
newDataset(cc12, "2018-01-03_sample_cces-2012")
```

You can then load it,

``` r
crunch_ds <- loadDataset("2018-01-03_sample_cces-2012")
```

Apply meta-data for each variable

``` r
lapply(crunch_ds, function(v){
  description(v) <- cc12_meta$description[cc12_meta$alias == name(v)]
})
```

    ## [[1]]
    ## [1] "[State]"
    ## 
    ## [[2]]
    ## [1] "How old are you?"
    ## 
    ## [[3]]
    ## [1] "What is your Gender?"
    ## 
    ## [[4]]
    ## [1] "What racial or ethnic group best describes you?"
    ## 
    ## [[5]]
    ## [1] "Which of the following best describes your current employment status?"
    ## 
    ## [[6]]
    ## [1] "Thinking back over the last year, what was your family's annual income?"
    ## 
    ## [[7]]
    ## [1] "What is your present religion, if any?"
    ## 
    ## [[8]]
    ## [1] "[vote for Obama]"

and make cross-tabs, for example.

``` r
xtab <- crtabs(~ religion + obama12, crunch_ds)
```

``` r
round(prop.table(xtab, margin = 1), 3)
```

    ##                            obama12
    ## religion                        0     1
    ##   Protestant                0.656 0.344
    ##   Roman Catholic            0.607 0.393
    ##   Mormon                    0.858 0.142
    ##   Eastern or Greek Orthodox 0.657 0.343
    ##   Jewish                    0.373 0.627
    ##   Muslim                    0.427 0.573
    ##   Buddhist                  0.346 0.654
    ##   Hindu                     0.562 0.438
    ##   Atheist                   0.264 0.736
    ##   Agnostic                  0.336 0.664
    ##   Nothing in particular     0.495 0.505
    ##   Something else            0.520 0.480
    ##   Skipped                     NaN   NaN
    ##   Not Asked                   NaN   NaN

you can view it here like this

``` r
webApp(crunch_ds)
```
