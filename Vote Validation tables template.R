setwd("/Users/kiraflemke/Desktop/Research Projects/NBC/Tables/State Vote Validation")

library(rio)
library(weights)

#### MI Recoding ----
state.vote <- import("RBS Sample Matches/MI_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/Final MI.sav")

state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSMI20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSMI20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSMI20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
#state.raw$LALVOTERID <- state.raw$lalvoterid
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

# save final prop table as xtable object
library(xtable)
table1 <- xtable(vote.table.prop, type = "latex")

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

# save final prop table as xtable object
table2 <- xtable(vote.cand.table.prop, type = "latex")

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop

# save final prop table as xtable object
table3 <- xtable(vote.meth.table.prop, type = "latex")

#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### MI tables ----
mi.table.w <- table.w
mi.table.v <- table.v

mi.num <- nrow(state.raw)
mi.m.num <- nrow(state.valid)

mi.denom <- cbind(c("MI"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(mi.denom) <- NULL

mi.2 <- cbind(c("MI"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(mi.2) <- NULL

xtable(mi.2, type = "latex",
       caption = "Validated vote proportion of each vote method and candidate",
       caption.placement = 'top')


mi.1a <- t(vote.table.prop)
row.names(mi.1a) <- "Total"

mi.1b <- t(vote.cand.table.prop)
mi.1c <- t(vote.meth.table.prop)

mi.table <- t(rbind(mi.1a, mi.1b, mi.1c))
mi.table2 <- cbind(c("MI", " "),rownames(mi.table), mi.table)
rownames(mi.table2) <- NULL

xtable(mi.table2, 
       type = "latex",
       caption = "Survey respondent validated voting behavior",
       caption.placement = 'top',
       include.rownames=FALSE)

#### AZ Recoding ----
state.vote <- import("RBS Sample Matches/AZ_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/AZ Final Data.sav")


state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                   state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSAZ20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSAZ20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSAZ20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
#state.raw$LALVOTERID <- state.raw$lalvoterid
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop


#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### AZ tables ----
az.table.w <- table.w
az.table.v <- table.v

az.num <- nrow(state.raw)
az.m.num <- nrow(state.valid)

az.denom <- cbind(c("AZ"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(az.denom) <- NULL

az.2 <- cbind(c("AZ"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(az.2) <- NULL

az.1a <- t(vote.table.prop)
row.names(az.1a) <- "Total"

az.1b <- t(vote.cand.table.prop)
az.1c <- t(vote.meth.table.prop)

az.table <- t(rbind(az.1a, az.1b, az.1c))
az.table2 <- cbind(c("AZ", " "),rownames(az.table), az.table)
rownames(az.table2) <- NULL

#### MN Recoding ----
state.vote <- import("RBS Sample Matches/MN_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/MN Final.sav")

state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                   state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSMN20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSMN20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSMN20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
#state.raw$LALVOTERID <- state.raw$lalvoterid
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop


#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### MN tables ----
mn.table.w <- table.w
mn.table.v <- table.v

mn.num <- nrow(state.raw)
mn.m.num <- nrow(state.valid)

mn.denom <- cbind(c("MN"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(mn.denom) <- NULL

mn.2 <- cbind(c("MN"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(mn.2) <- NULL

mn.1a <- t(vote.table.prop)
row.names(mn.1a) <- "Total"

mn.1b <- t(vote.cand.table.prop)
mn.1c <- t(vote.meth.table.prop)

mn.table <- t(rbind(mn.1a, mn.1b, mn.1c))
mn.table2 <- cbind(c("MN", " "),rownames(mn.table), mn.table)
rownames(mn.table2) <- NULL

#### FL Recoding ----
state.vote <- import("RBS Sample Matches/FL_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/FL Final with election day.Sav")

state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                   state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSFL20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSFL20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSFL20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
#state.raw$LALVOTERID <- state.raw$lalvoterid
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop


#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### FL tables ----
fl.table.w <- table.w
fl.table.v <- table.v

fl.num <- nrow(state.raw)
fl.m.num <- nrow(state.valid)

fl.denom <- cbind(c("FL"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(fl.denom) <- NULL

fl.2 <- cbind(c("FL"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(fl.2) <- NULL

fl.1a <- t(vote.table.prop)
row.names(fl.1a) <- "Total"

fl.1b <- t(vote.cand.table.prop)
fl.1c <- t(vote.meth.table.prop)

fl.table <- t(rbind(fl.1a, fl.1b, fl.1c))
fl.table2 <- cbind(c("FL", " "),rownames(fl.table), fl.table)
rownames(fl.table2) <- NULL

#### NC Recoding ----
state.vote <- import("RBS Sample Matches/NC_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/NC Final with eday.sav")

state.raw$LALVOTERID <- state.raw$lalvoterid
state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                   state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSNC20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSNC20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSNC20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop


#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### NC tables ----
nc.table.w <- table.w
nc.table.v <- table.v

nc.num <- nrow(state.raw)
nc.m.num <- nrow(state.valid)

nc.denom <- cbind(c("NC"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(nc.denom) <- NULL

nc.2 <- cbind(c("NC"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(nc.2) <- NULL

nc.1a <- t(vote.table.prop)
row.names(nc.1a) <- "Total"

nc.1b <- t(vote.cand.table.prop)
nc.1c <- t(vote.meth.table.prop)

nc.table <- t(rbind(nc.1a, nc.1b, nc.1c))
nc.table2 <- cbind(c("NC", " "),rownames(nc.table), nc.table)
rownames(nc.table2) <- NULL

#### NV Recoding ----

state.vote <- import("RBS Sample Matches/NV_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/NV Final with election day.sav")

state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                   state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSNV20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSNV20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSNV20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
#state.raw$LALVOTERID <- state.raw$lalvoterid
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop


#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### NV tables ----
nv.table.w <- table.w
nv.table.v <- table.v
nv.num <- nrow(state.raw)
nv.m.num <- nrow(state.valid)

nv.denom <- cbind(c("NV"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(nv.denom) <- NULL

nv.2 <- cbind(c("NV"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(nv.2) <- NULL

nv.1a <- t(vote.table.prop)
row.names(nv.1a) <- "Total"

nv.1b <- t(vote.cand.table.prop)
nv.1c <- t(vote.meth.table.prop)

nv.table <- t(rbind(nv.1a, nv.1b, nv.1c))
nv.table2 <- cbind(c("NV", " "),rownames(nv.table), nv.table)
rownames(nv.table2) <- NULL

#### IA Recoding ----
state.vote <- import("RBS Sample Matches/IA_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/IA final.sav")

state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                   state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSIA20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSIA20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSIA20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
#state.raw$LALVOTERID <- state.raw$lalvoterid
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop


#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### IA tables ----
ia.table.w <- table.w
ia.table.v <- table.v

ia.num <- nrow(state.raw)
ia.m.num <- nrow(state.valid)

ia.denom <- cbind(c("IA"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(ia.denom) <- NULL

ia.2 <- cbind(c("IA"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(ia.2) <- NULL

ia.2[,4] <- NA

ia.1a <- t(vote.table.prop)
row.names(ia.1a) <- "Total"

ia.1b <- t(vote.cand.table.prop)
ia.1c <- t(vote.meth.table.prop)

ia.table <- t(rbind(ia.1a, ia.1b, ia.1c))
ia.table2 <- cbind(c("IA", " "),rownames(ia.table), ia.table)
rownames(ia.table2) <- NULL

ia.table2[,6] <- NA

#### TX Recoding ----

state.vote <- import("RBS Sample Matches/TX_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/Final TX data with eday.Sav")

state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                   state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSTX20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSTX20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSTX20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
#state.raw$LALVOTERID <- state.raw$lalvoterid
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.vote$LALVOTERID <- state.vote$id
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop


#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### TX tables ----
tx.table.w <- table.w
tx.table.v <- table.v

tx.num <- nrow(state.raw)
tx.m.num <- nrow(state.valid)

tx.denom <- cbind(c("TX"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(tx.denom) <- NULL

tx.2 <- cbind(c("TX"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(tx.2) <- NULL

tx.2[,4] <- NA

tx.1a <- t(vote.table.prop)
row.names(tx.1a) <- "Total"

tx.1b <- t(vote.cand.table.prop)
tx.1c <- t(vote.meth.table.prop)

tx.table <- t(rbind(tx.1a, tx.1b, tx.1c))
tx.table2 <- cbind(c("TX", " "),rownames(tx.table), tx.table)
rownames(tx.table2) <- NULL

tx.table2[,6] <- NA

#### WI Recoding ----
state.vote <- import("RBS Sample Matches/WI_plus_vote_method.csv")
state.raw <- import("Raw Files for Disposition Project/WI final with eday.sav")

state.raw$validated[!state.raw$LALVOTERID %in% state.vote$id] <- "Unmatched"
state.raw$validated[state.raw$LALVOTERID %in% state.vote$id] <- "Matched"
table.v <- table(state.raw$validated)

table.w <- wtd.table(state.raw$validated ,weights = state.raw$WEIGHT, type = 'table')

# recode actual vote method to labeled binary based on whether the respondent actually voted or not
table(state.vote$vote_method_2020)
state.vote$voted <- NA
state.vote$voted[state.vote$vote_method_2020 == "Absentee" | 
                   state.vote$vote_method_2020 == "Election Day"] <- "Voted"
state.vote$voted[state.vote$vote_method_2020 == "Did not vote"] <- "Did not vote"
table(state.vote$voted)

# recode intended vote method - check that these values are the same for each state  
# 1 - Election Day
# 2 - Mail-in 
# 3 - Early In-person
table(state.raw$VOTEMETHOD)
state.raw$vote.method <- NA
state.raw$vote.method[state.raw$VOTEMETHOD  == 1] <- "Election Day"
state.raw$vote.method[state.raw$VOTEMETHOD  == 2] <- "Mail-in"
state.raw$vote.method[state.raw$VOTEMETHOD  == 3] <- "Early In-person"
table(state.raw$vote.method, useNA = "ifany")

# recode intended candidate to party names - do not include third party
# only include "Undecided" if it makes up a nontrivial proportion of the responses
table(state.raw$PRSWI20)
state.raw$candidate <- NA
state.raw$candidate[state.raw$PRSWI20  == 1] <- "Biden"
state.raw$candidate[state.raw$PRSWI20  == 2] <- "Trump"
table(state.raw$candidate, useNA = "ifany")

#### Merging 

# subset the raw state data to only include LALVOTERID and recoded variables vote.method and candidate 
# check the the variable index for each state dataset
#state.raw$LALVOTERID <- state.raw$lalvoterid
state.sub <- state.raw[,c("LALVOTERID","vote.method", "candidate")]
dim(state.sub)

# merge with file of actual vote data from voter file by voter id "LALVOTERID"
state.valid <- merge(state.vote, state.sub, by = "LALVOTERID")
dim(state.valid)

#### Table 1: Voted 
#create table of actual vote among respondents (all expected voters)
vote.table <- table(state.valid$voted) 
vote.table

# display the values of the table as percent 
vote.table.prop <- round(prop.table(vote.table)*100, 1)
vote.table.prop

#### Table 2: Voted by candidate 
vote.cand.table <- table(state.valid$voted, state.valid$candidate) 
vote.cand.table

# display the values of the table as percent of 
vote.cand.table.prop <- round(prop.table(vote.cand.table, 2)*100, 1)
vote.cand.table.prop

#### Table 3: Voted by Intended Method
vote.meth.table <- table(state.valid$voted, state.valid$vote.method) 
vote.meth.table

# display the values of the table as percent of 
vote.meth.table.prop <- round(prop.table(vote.meth.table, 2)*100, 1)
vote.meth.table.prop


#### Table 4: Vote Method Plan x Candidate 
# vote method plan by candidate total
state.valid.votedON <- state.valid[state.valid$voted == "Voted" | state.valid$voted == "Did not vote",]
vote.meth.cand.table.full <- table(state.valid.votedON$candidate, state.valid.votedON$vote.method) 

# vote method plan by candidate among actual voters
state.valid.voted <- state.valid[state.valid$voted == "Voted",] # subset to only those who voted
vote.meth.table.voted <- table(state.valid.voted$candidate, state.valid.voted$vote.method) 

# proportion that voted out of each planned vote method & candidate group
vote.meth.cand.table <- vote.meth.table.voted/vote.meth.cand.table.full
vote.meth.cand.prop <- round(vote.meth.cand.table*100, 1)
vote.meth.cand.prop

#### WI tables ----
wi.table.w <- table.w
wi.table.v <- table.v

wi.num <- nrow(state.raw)
wi.m.num <- nrow(state.valid)

wi.denom <- cbind(c("WI"," "), rownames(vote.meth.cand.table.full), vote.meth.cand.table.full)
rownames(wi.denom) <- NULL

wi.2 <- cbind(c("WI"," "), rownames(vote.meth.cand.prop), vote.meth.cand.prop)
rownames(wi.2) <- NULL

wi.1a <- t(vote.table.prop)
row.names(wi.1a) <- "Total"

wi.1b <- t(vote.cand.table.prop)
wi.1c <- t(vote.meth.table.prop)

wi.table <- t(rbind(wi.1a, wi.1b, wi.1c))
wi.table2 <- cbind(c("WI", " "),rownames(wi.table), wi.table)
rownames(wi.table2) <- NULL

#### Combine all tables ----

tab.list <-xtableList(list(az.table2, fl.table2, ia.table2, mi.table2, mn.table2, nc.table2, nv.table2, tx.table2, wi.table2),
                      caption = "Survey respondent validated voting behavior")

print(tab.list,
      include.rownames = FALSE,
      type = "latex",
      caption.placement = 'top')

tab.list2 <-xtableList(list(az.2, fl.2, ia.2, mi.2, mn.2, nc.2, nv.2, tx.2, wi.2),
                       caption = "Validated vote proportion of each vote method and candidate")

print(tab.list2,
      include.rownames = FALSE,
      type = "latex",
      caption.placement = 'top')

tab.list3 <-xtableList(list(az.denom, fl.denom, ia.denom, mi.denom, mn.denom, nc.denom, nv.denom, tx.denom, wi.denom),
                       caption = "Total number validated for each vote method and candidate")

print(tab.list3,
      include.rownames = FALSE,
      type = "latex",
      caption.placement = 'top')


total.matched <- cbind.data.frame(c("AZ", "FL", "IA", "MI", "MN", "NC", "NV", "TX", "WI"),
                       c(az.num, fl.num, ia.num, mi.num, mn.num, nc.num, nv.num, tx.num, wi.num),
                       c(az.m.num, fl.m.num, ia.m.num, mi.m.num, mn.m.num, nc.m.num, nv.m.num, tx.m.num, wi.m.num))
names(total.matched) <- c("State", "Complete", "Validated")

total.matched$`Matched %` <- NA
total.matched$`Matched %` <- round((total.matched$Validated/total.matched$Complete)*100, 1)

total.matched1 <- xtable(total.matched,
                       caption = "Validation Stats")
print(total.matched1,
      type = "latex",
      include.rownames = FALSE,
      caption.placement = 'top')

total.w.matched <- rbind.data.frame(az.table.w, fl.table.w, ia.table.w, mi.table.w, mn.table.w, nc.table.w, nv.table.w, tx.table.w, wi.table.w)
total.w.matched2 <- cbind.data.frame(c("AZ", "FL", "IA", "MI", "MN", "NC", "NV", "TX", "WI"), total.w.matched)
total.w.matched2$prop <- total.w.matched2[,2]/(total.w.matched2[,2]+total.w.matched2[,3])*100
names(total.w.matched2) <- c("State", "Matched","Unmatched", "Matched %")
total.w.matched2[,2:4] <- round(total.w.matched2[,2:4], 2)

total.matched.w <- xtable(total.w.matched2,
                         caption = "Weighted Validation Stats")
print(total.matched.w,
      type = "latex",
      include.rownames = FALSE,
      caption.placement = 'top')

total.v.matched <- rbind.data.frame(az.table.v, fl.table.v, ia.table.v, mi.table.v, mn.table.v, nc.table.v, nv.table.v, tx.table.v, wi.table.v)
total.v.matched2 <- cbind.data.frame(c("AZ", "FL", "IA", "MI", "MN", "NC", "NV", "TX", "WI"), total.v.matched)
total.v.matched2$total <- total.v.matched2[,2]+total.v.matched2[,3]
total.v.matched2$prop <- round(total.v.matched2[,2]/(total.v.matched2[,2]+total.v.matched2[,3])*100, 2)
names(total.v.matched2) <- c("State", "Matched","Unmatched", "Total", "Matched %")

total.matched.v <- xtable(total.v.matched2,
                          caption = "Weighted Validation Stats")
print(total.matched.v,
      type = "latex",
      include.rownames = FALSE,
      caption.placement = 'top')


