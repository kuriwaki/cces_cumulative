# CCES cumulatives

------

Directory for building a CCES cumulative file collaboratively. Project page [here](https://github.com/kuriwaki/cces_cumulative/projects/1).

Please feel free to file any questions or requests about the cumulative file as Github issues. 




Structure of the directory
------

## code
- `build` stores code that is used to wrangle data and build datasets
- `analyze` stores code to visualize and analyze data.
- `old_code` for code that is out of use

## data
- `data/source` is where all the input data is. Things in here should not be overwritten. Instead, manipulated output should go in `data/output`.
- `data/source/cces` stores all the CCES common content material.
- `data/source/census` stores all the census-related data, and so on.


Note: most data files are not tracked on git.


## other references
- `figures` for generated figures
- `tables` for generated tables
- `guides` stores all the codebooks, guides, etc.. to explain the common content.