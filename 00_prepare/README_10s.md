By Nathan Kaplan, 2017-08-18

# 05_build_voteview.R
This is code to read in voteview meta data on congress people from Congresses 109-115.
The code also binds all of these dataframes together into one, as well as create a new column called last name to make merging with CQ data possible/easier

# 06_build_CQ.R
This is code to read in CQ meta data on congress people from Congresses 109-115.
The code reads through all files downloaded from the CQ website.
NOTE: gathering this data was inconvenient, as CQ only lets you download 10 people at a time.
The code also cleans the dataframes (names columns, makes sure all dataframes line up for merging).
Then, the dataframes are merged together.

# 07_build_DIME.R
This code reads in DIME data and strips it down into a data frame that only includes relevant columns (FEC, ICPSR, seat).

# 08_join_MCs.R
Joins the three datasets described above.

To do:
Check to see how many congress people are missing FEC’s and/or ICPSR’s
Check to see how many congress people are missing from the list all together
Strip down columns deemed unnecessary (?)


