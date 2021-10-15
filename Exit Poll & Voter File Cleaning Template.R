##
##    Michigan
##
library(rio)
setwd("/Users/kiraflemke/Desktop/Research Projects/NBC")

dat <- AZ

#Recoding dispo
#This will be a bit different for every state, have to use judgement
#1 = Complete
#2 = Refuse
#3 = Unavailable
#4 = DQd
#NA = Other
table(dat$name)
table(dat$dispid)

unavil <- c(6,
         93,
         7,
         92,
         1,
         3,
         4,
         2,
         91,
         90)

refuse <- c(5,
            95,
            11,
            10,
            13,
            98)

dat$dispo.cleaned <- NA
dat$dispo.cleaned[dat$dispid==20]<-1
dat$dispo.cleaned[dat$dispid %in% unavil ] <-2
dat$dispo.cleaned[dat$dispid %in% refuse] <- 3
dat$dispo.cleaned[dat$dispid == 9]<- 4

table(dat$dispid)
table(dat$dispo.cleaned)
table(dat$dispid, dat$dispo.cleaned)


#Gender
table(dat$GENDER)
dat$female <- NA
dat$female[dat$GENDER == "F"] <- 1
dat$female[dat$GENDER == "M"] <- 0
table(dat$female)

#Age
table(is.na(dat$AGE))

dat$AgeUnder25 <- NA
dat$AgeUnder25[dat$AGE <= 25] <- 1
dat$AgeUnder25[dat$AGE > 25] <- 0

dat$Age2635 <- NA
dat$Age2635[dat$AGE > 25 & dat$AGE <=35] <- 1
dat$Age2635[dat$AGE <= 25 | dat$AGE >35] <- 0

dat$Age3645 <- NA
dat$Age3645[dat$AGE > 35 & dat$AGE <=45] <- 1
dat$Age3645[dat$AGE <= 35 | dat$AGE >45] <- 0

dat$Age4655 <- NA
dat$Age4655[dat$AGE > 45 & dat$AGE <=55] <- 1
dat$Age4655[dat$AGE <= 45 | dat$AGE >55] <- 0

dat$Age5665 <- NA
dat$Age5665[dat$AGE > 55 & dat$AGE <=65] <- 1
dat$Age5665[dat$AGE <=55 | dat$AGE >65] <- 0

dat$Age66p <- NA
dat$Age66p[dat$AGE>65] <-1
dat$Age66p[dat$AGE<=65] <-0


#Ethnicity
table(dat$ETHNICGROUPS)

dat$white <- NA
dat$white[dat$ETHNICGROUPS=="European"] <- 1
dat$white[dat$ETHNICGROUPS!="European" & !is.na(dat$ETHNICGROUPS)] <- 0
table(dat$white)

dat$black <- NA
dat$black[dat$ETHNICGROUPS=="Likely African-American"]<- 1
dat$black[dat$ETHNICGROUPS!="Likely African-American" & !is.na(dat$ETHNICGROUPS)]<- 0
table(dat$black)

dat$hispanic <- NA
dat$hispanic[dat$ETHNICGROUPS=="Hispanic and Portuguese"]<- 1
dat$hispanic[dat$ETHNICGROUPS!="Hispanic and Portuguese" & !is.na(dat$ETHNICGROUPS)]<- 0
table(dat$hispanic)

#Party
table(dat$PARTIESDESC)
dat$likely.party <- NA
dat$likely.party[dat$PARTIESDESC=="Democratic"] <- "D"
dat$likely.party[dat$PARTIESDESC=="Registered Independent" | dat$PARTIESDESC=="Non-Partisan"] <- "I"
dat$likely.party[dat$PARTIESDESC=="Republican"] <- "R"
table(dat$likely.party)
table(dat$PARTIESDESC, dat$likely.party)

#Relevel party factor so D is reference
dat$likely.party<- as.factor(dat$likely.party)
dat$likely.party <- relevel(dat$likely.party, ref="D")

#Congressional District
table(dat$CONGDISTRICT)
#Remove cases with no CD (make a note if it's a lot, it shouldn't be)
table(is.na(dat$CONGDISTRICT))
dat <- dat[!is.na(dat$CONGDISTRICT),]
dat$cd <- dat$CONGDISTRICT

#For later merging, make CD a character variable, add a leading 0
dat$cd <- as.character(dat$cd)
dat$cd[nchar(dat$cd)==1] <- paste("0",dat$cd[nchar(dat$cd)==1], sep="")

#ID
dat$id <- dat$LALVOTERID

#Names phone address
dat$first.name <- dat$FNAME
dat$middle.name <- dat$MIDDLENAME
dat$last.name <- dat$LNAME
dat$phone.number <- dat$VOTEFPHONE
dat$address <- dat$ADDRESSLINE
dat$city <- dat$CITY
dat$state <- dat$STATE
dat$zip.code <- dat$ZIP

#Active
dat$active <- dat$ACTIVE

names(dat)

#Cellphone
table(dat$TELCELLFLAG)
dat$cellphone <- NA
dat$cellphone <- 0
dat$cellphone[dat$TELCELLFLAG=="Y"] <- 1
table(dat$cellphone)


keep <- c("id","state","cd","city","zip.code", "address", "phone.number",
          "first.name","middle.name","last.name",
          "active","dispo.cleaned", "likely.party",
          "female",
          "AgeUnder25","Age2635", "Age3645","Age4655","Age5665","Age66p",
          "white","black","hispanic", "cellphone")

keep %in% names(dat)

mi <- dat[keep]

head(mi)

save(mi, file="C:/Users/Marc/Dropbox/PORES (1)/2020 General/Exit Poll Disposition Files/Disposition files/Cleaned Files/MICleaned.Rdata")









