library(readr)
library(dplyr)
library(glue)
library(stringr)
library(ggplot2)
library(scales)


## code that associates FIPS codes with state names
statecode <- read_csv("data/source/statecode.csv")


# Read in data -----------------

read_zcta <- function(cong) {
  read_delim(
    glue("data/source/census_zcta/{cong}/zcta_cd{cong}_natl.txt"),
    delim = ",",
    col_types = cols(),
    skip = 2,
    col_names = c("fips", "zipcode", "distnum")
  ) %>%
    mutate(fips = as.integer(fips)) %>%
    filter(!is.na(as.numeric(distnum)))
}


national109 <- read_zcta(109)
national110 <- read_zcta(110)
national113 <- read_zcta(113)
national115 <- read_zcta(115)


# Recode at large ------
# 113 and 115 don't use at-large districts; so drop these
national109 <- filter(national109, distnum != "00")
national110 <- filter(national110, distnum != "00")


# Group -------

# sometimes zipcodes straddle districts! This will be a pain to account for by
# just merging, let's somehow manipulate the dataset so that each row is a
# unique zipcode.

# group the dataset into state-zipcodes. Then, let's just list up the distnums
# that are _within each group_

by_zip <- function(tbl) {
  tbl %>%
    group_by(fips, zipcode) %>%
    summarize(distnums = paste0(distnum, collapse = ","))
}

n109_byzip <- by_zip(national109)
n110_byzip <- by_zip(national110)
n113_byzip <- by_zip(national113)
n115_byzip <- by_zip(national115)

mkey <- c("fips", "zipcode")
n109_115_byzip <- left_join(n109_byzip, n110_byzip, by = mkey) %>%
  left_join(n113_byzip, mkey) %>%
  left_join(n115_byzip, mkey) %>%
  ungroup()

colnames(n109_115_byzip) <- c("fips", "zipcode", "distnum109", "distnum110", "distnum113", "distnum115")

# add state name -----
n109_115_byzip <- left_join(n109_115_byzip, select(statecode, st, fips), by = "fips") %>%
  select(st, zipcode, matches("distnum"))

## Make a key of districts ------------

all_CDs <- bind_rows(
  national109,
  national110,
  national113,
  national115
) %>%
  left_join(statecode, by = "fips") %>%
  mutate(CD = paste0(st, "-", distnum)) %>%
  distinct(CD) %>%
  arrange(CD) %>%
  pull(CD)


# build container (empty)
container <- tibble(
  CD = all_CDs,
  zips109 = NA,
  zips110 = NA,
  zips113 = NA,
  zips115 = NA
)



## Loop through and store zipcodes for a given district ----

for (d in all_CDs) {
  state_of_d <- gsub("-[0-9]+", "", d)
  distnum_of_d <- gsub("[-A-Z]+", "", d)

  for (cong in c(109, 110, 113, 115)) {
    column_to_search <- glue("distnum{cong}")

    # pull out the zips
    zips_in_d_at_c <- n109_115_byzip %>%
      filter(st == state_of_d & grepl(distnum_of_d, .data[[column_to_search]])) %>%
      pull(zipcode)

    container[which(d == container$CD), glue("zips{cong}")] <- paste(zips_in_d_at_c, collapse = ",")
  }
}


# Loop through districts to get a easure of change ---------
# build new container
over_post <- over_pre <- tibble(
  CD = all_CDs,
  rate_109_110 = NA,
  rate_110_113 = NA,
  rate_113_115 = NA
)


for (d in all_CDs) {
  row_number <- which(d == container$CD)

  for (cong in c(109110, 110113, 113115)) {
    c_start <- substr(cong, start = 1, stop = 3)
    c_end <- substr(cong, start = 4, stop = 6)
    column_name_pre <- glue("zips{c_start}")
    column_name_post <- glue("zips{c_end}")


    zd_pre_char <- as.character(container[row_number, column_name_pre])
    zd_post_char <- as.character(container[row_number, column_name_post])

    zd_pre_vec <- str_split(zd_pre_char, ",")[[1]]
    zd_post_vec <- str_split(zd_post_char, ",")[[1]]

    zd_pre_notpost <- setdiff(x = zd_pre_vec, y = zd_post_vec)
    zd_post_notpre <- setdiff(x = zd_post_vec, y = zd_pre_vec)
    zd_pre_post <- intersect(zd_pre_vec, zd_post_vec)


    # sizes
    n_zd_pre <- length(zd_pre_vec)
    n_zd_post <- length(zd_post_vec)
    n_zd_pre_notpost <- length(zd_pre_notpost)
    n_zd_post_notpre <- length(zd_post_notpre)
    n_zd_pre_post <- length(zd_pre_post)

    # set disappearing districts to NA
    if (zd_post_char == "") {
      n_zd_pre_post <- NA
    }

    # figure out which row corresponds to district d
    over_pre[which(d == container$CD), glue("rate_{c_start}_{c_end}")] <-
      (n_zd_pre_post / n_zd_pre)

    over_post[which(d == container$CD), glue("rate_{c_start}_{c_end}")] <-
      (n_zd_pre_post / n_zd_post)

    rm(n_zd_pre, n_zd_post, n_zd_pre_post, zd_pre_char, zd_post_char)
  }
}


# write to table -----
write_excel_csv(over_pre, "data/output/03_contextual/zipchange_CD_overpre.csv", na = "")
write_excel_csv(over_post, "data/output/03_contextual/zipchange_CD_overpost.csv", na = "")


# visualize ------
df_toplot <- over_pre %>%
  reshape2::melt(
    id.vars = "CD",
    variable.name = "window",
    value.name = "prop_zips_stay"
  ) %>%
  mutate(
    st = gsub("-[0-9]+", "", CD),
    prop_zips_stay = as.numeric(prop_zips_stay)
  ) %>%
  left_join(select(statecode, st, division)) %>%
  filter(st != "DC") %>%
  tbl_df()


lineplot <- ggplot(df_toplot, aes(x = window, y = prop_zips_stay, group = CD)) +
  facet_wrap(~ division, ncol = 3) +
  geom_line(alpha = 0.35) +
  scale_x_discrete(labels = c("2006-2008\n(109-110th)", "2009-2015\n(110-113th)", "2016-2019\n(113-115)")) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(size = 6)) +
  labs(
    y = "Proportion of CD's zipcodes from previous congress \n that stayed remained in CD",
    x = "Time Window (in Congresses)",
    caption = "Each line is a Congressional District code (e.g. TX-23)"
  )

ggsave("figures/district_change_lineplot.pdf", lineplot, w = 7, h = 5.5)
