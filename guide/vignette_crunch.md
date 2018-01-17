More on Crunch
================

Crunch is a interface for viewing and manipulating datasets.

``` r
library(crunch)
```

For example, start with a survey you already have

``` r
library(haven)
library(curl)
library(dplyr)
library(tibble)

# dataset
cc12_link <- curl("www.shirokuriwaki.com/datasets/cces_sample.dta")
cc12 <- read_dta(cc12_link) %>%
  mutate_if(is.labelled, as_factor) 
# switch lablled to factor b/c crunch doesn't accept
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

Apply meta-data for each variable to format names and descriptions.

``` r
lapply(crunch_ds, function(v){
  name(v) <-        cc12_meta$name[cc12_meta$alias == alias(v)]
  description(v) <- cc12_meta$description[cc12_meta$alias == alias(v)]
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

``` r
crunch_ds
```

    ## Dataset "2018-01-03_sample_cces-2012"
    ## 
    ## Contains 44802 rows of 8 variables:
    ## 
    ## $state: State (categorical)
    ## $age: Age (numeric)
    ## $gender: Gender (categorical)
    ## $race: Race (categorical)
    ## $employ: Employment Status (categorical)
    ## $family_income: Family Income (categorical)
    ## $religion: Religion (categorical)
    ## $obama12: Obama Vote (numeric)

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
