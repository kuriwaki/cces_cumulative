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

-   `build/01_pull-08to11.R` pulls out some variables that we need separately from 2008 - 2011 CCES common content, that was masked in the 2006-2012 cumulative file.
-   `build/02_stack.R` pulls out the variabales of interest from annual CCES files. Then, we stack this into a long dataset where each row is a respondent from CCES. This is uploaded to Crunch.
-   `build/09_format-crunch.R` logs into Crunch, and adds variable names, descriptions, groupings, and other Crunch attributes to the Crunch dataset.
