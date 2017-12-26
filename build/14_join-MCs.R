library(dplyr)


#  read datasets ---
vv_all <- readRDS("data/output/03_contextual/voteview_mcs.Rds")
vvH <- readRDS("data/output/03_contextual/voteview_H_key.Rds")
vvS <- readRDS("data/output/03_contextual/voteview_S_key.Rds")

cq <- readRDS("data/output/03_contextual/cq_profiles.Rds")

dime <- readRDS("data/output/03_contextual/dime_fmt.Rds")


# join Voteview and FEC list (with appropriate offices) ----
vv_fec_H <- left_join(
  vvH,
  filter(dime, seat == "federal:house"),
  by = "icpsr"
)

vv_fec_S <- left_join(
  vvS,
  filter(dime, seat == "federal:senate"),
  by = "icpsr"
)
# join CQ ---
df_H <- left_join(vv_fec_H, cq, by = c("namelast", "chamber", "st", "dist", "congress"))
df_S <- left_join(vv_fec_S, cq, by = c("namelast", "chamber", "st", "congress"))



# save -----
saveRDS(df_H, "data/output/03_contextual/incumbents_H.Rds")
saveRDS(df_S, "data/output/03_contextual/incumbents_S.Rds")
