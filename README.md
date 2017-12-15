CCES cumulative
================

Directory for building a CCES cumulative file collaboratively. Project page [here](https://github.com/kuriwaki/cces_cumulative/projects/1).

Please feel free to file any questions or requests about the cumulative file as Github issues.

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

-   `build/01_pull-08to11.R` pulls out some variables that we need separately from 2008 - 2011 CCES common content, that was masked in the 2006-2012 cumulative file.
-   `build/02_stack.R` pulls out the variabales of interest from annual CCES files. Then, we stack this into a long dataset where each row is a respondent from CCES. This is uploaded to Crunch.
-   `build/09_format-crunch.R` logs into Crunch, and adds variable names, descriptions, groupings, and other Crunch attributes to the Crunch dataset.
