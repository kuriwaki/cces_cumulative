library(haven)
library("dplyr")


cc16_full <- read_dta("~/Dropbox/cces_cumulative/data/source/cces/2016_cc_vv.dta")


# select columns
cc16 <- cc16_full %>%
  rename(caseID = V101,
         intent_turnout = CC16_364,
         intent_pres_16 = CC16_364c,
         post_turnout = CC16_401,
         ev_pres_16 = CC16_364b,
         voted_pres_16 = CC16_410a,
         vv_16 = CL_E2016GVM)  %>%
  mutate(state = as.character(as_factor(inputstate)),
         intent_pres_16 = replace(intent_pres_16, intent_pres_16 == 9, NA),
         ev_pres_16 = replace(ev_pres_16, ev_pres_16 == 9, NA),
         combined_pres_pre = coalesce(zap_labels(intent_pres_16), zap_labels(ev_pres_16)),
         vote_hrc_pre = as.numeric(combined_pres_pre == 2),
         vote_djt_pre = as.numeric(combined_pres_pre == 1),
         vote_hrc_post = as.numeric(voted_pres_16 == 2),
         vote_djt_post = as.numeric(voted_pres_16 == 1),
         turnout_wgt = case_when(intent_turnout == 3 ~ 1.0,
                                 intent_turnout == 1 ~ 0.9,
                                 intent_turnout == 2 ~ 0.3,
                                 intent_turnout == 5 ~ 0.1,
                                 intent_turnout %in% c(4,8,9, NA) ~ 0),
         post_turnout =  as.numeric(post_turnout == 5),
         vv_turnout = as.numeric(vv_16 != "")) %>%
  select(caseID, commonweight, commonweight_post, tookpost, state,
         matches("vote_"), combined_pres_pre, voted_pres_16,
         intent_turnout, post_turnout, turnout_wgt, vv_turnout)



saveRDS(cc16, "data/input/cces2016_slim.Rds")
