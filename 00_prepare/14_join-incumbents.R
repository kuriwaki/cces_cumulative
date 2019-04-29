library(dplyr)


#  read datasets ---
vv_all <- readRDS("data/output/03_contextual/voteview_mcs.Rds")
vvH <- readRDS("data/output/03_contextual/voteview_H_key.Rds")
vvS <- readRDS("data/output/03_contextual/voteview_S_key.Rds")

cq <- readRDS("data/output/03_contextual/cq_profiles.Rds")

fec <- readRDS("data/output/03_contextual/fec_fmt.Rds")



# add firstname to special election people -- people who didn't make it to cq

add_first <-  vv_all %>% 
  filter(chamber %in% c("H", "S")) %>% 
  mutate(dist = replace(dist, chamber == "S", 99L)) %>%  # force a merge
  select(congress, chamber, st, icpsr, dist, namelast, bioname) %>% 
  left_join(mutate(cq, dist = replace(dist, chamber == "S", 99L)),  
            by = c("congress", "chamber", "st", "dist", "namelast")) %>% 
  filter(is.na(namefirst)) %>% 
  distinct(congress, chamber, icpsr, namelast, bioname) %>% 
  mutate(bio_first = str_extract(bioname, "(?<=,\\s)[A-z]+"))

cq_df <- cq %>%
  left_join(add_first, by = c("namelast", "congress", "chamber")) %>% 
  mutate(namefirst = coalesce(namefirst, bio_first)) %>% 
  select(-bio_first, -bioname, -icpsr)


# join Voteview and FEC list (with appropriate offices) ----
vv_fec_H <- left_join(
  vvH,
  filter(fec, office_sought == "federal:house") %>% distinct(icpsr_num, fec),
  by = c("icpsr" = "icpsr_num")
)

vv_fec_S <- left_join(
  vvS,
  filter(fec, office_sought == "federal:senate") %>% distinct(icpsr_num, fec),
  by = c("icpsr" = "icpsr_num")
)

# join CQ ---
df_H <- left_join(vv_fec_H, cq_df, by = c("namelast", "chamber", "st", "dist", "congress"))
df_S <- left_join(vv_fec_S, cq_df, by = c("namelast", "chamber", "st", "congress")) %>% 
  add_row(congress = 115, 
          chamber = "S",
          icpsr = 41705, 
          st = "AL",
          namelast = "JONES",
          namefirst = "Doug") %>% 
  add_row(congress =  115,
          chamber = "S",
          icpsr = 41706,
          st = "MN",
          namelast = "SMITH",
          namefirst = "Tina") %>% 
  add_row(congress =  115,
          chamber = "S",
          icpsr = 41707,
          icpsr = "MS",
          namelast = "HYDE-SMITH",
          namefirst = "Cindy") %>% 
  add_row(congress =  115,
          chamber = "S",
          icpsr = 15429,
          st = "AZ",
          namelast = "KYL",
          namefirst = "John")




# save -----
write_rds(df_H, "data/output/03_contextual/incumbents_H.Rds")
write_rds(df_S, "data/output/03_contextual/incumbents_S.Rds")

write_csv(df_H, "data/output/03_contextual/incumbents_H.csv", na = "")
write_csv(df_S, "data/output/03_contextual/incumbents_S.csv", na = "")
