library("Rvoteview")
library("here")
wd <- "~/Dropbox/cces_cumulative"
res <- voteview_search(startdate = 2006, enddate = 2017, keyvote = "CQ")
rcobject <- voteview_download(res$id[1:207], perrequest = 15, keeplong = T)
rclongdataframe <- melt_rollcall(rcobject, votecols = c("chamber", "congress"))
saveRDS(res, file.path(wd, "data/output/res_kaplan.Rds"))
saveRDS(rclongdataframe, file.path(wd,"data/output/rclongdataframe_kaplan.Rds"))
saveRDS(rcobject, file.path(wd, "data/output/rcobject_kaplan.Rds"))
getSubset <- function(topic){foo <- which(res$codes.Clausen==topic)
topic <- res[foo, ]
return(topic)
}
Agriculture <- getSubset("Agriculture")
CivilLiberties <- getSubset("Civil Liberties")
ForeignDefensePolicy <- getSubset("Foreign and Defense Policy")
GovernmentManagement <- getSubset("Government Management")
SocialWelfare <- getSubset("Social Welfare")
MiscPolicy <- getSubset("Miscellaneous Policy")