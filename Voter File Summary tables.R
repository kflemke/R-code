setwd("/Users/kiraflemke/Desktop/Research Projects/NBC/Tables")

#### Bind ----

alldta <- rbind(al,az,co,fl,ga,ia,ky,me,mi,mn,mt,nc,nh,nv,oh,or,pa,tx,va,wa,wi)
dim(alldta)

#### Disposition Table ----
alltable <- as.data.frame(xtabs(~ alldta$state+ alldta$DISPO + alldta$dispo.cleaned, data=alldta))
alltable <- alltable[alltable$Freq != 0,]

alltable1 <- alltable[order(alltable$alldta.state),]

alltable1a <- aggregate(alltable1$alldta.DISPO, by=list(alltable1$alldta.state, alltable1$alldta.dispo.cleaned), FUN=toString)

alltable1b <- alltable1a[order(alltable1a$Group.1),]

alltableFIN <- rename(alltable1b,
                      State = Group.1,
                      Code = Group.2,
                      Text = x)
View(alltableFIN)

library(xtable)
pos <- list(seq(from = 4, to = nrow(alltableFIN), by = 4))
command <- "\\\\ & & \\\\"
spaces.x <- cbind(pos,command)
alltableFINX <- xtable(alltableFIN, type = "latex")
align(alltableFINX) <- "p{.02\\textwidth}p{.02\\textwidth}p{.02\\textwidth}p{.9\\textwidth}"
print(alltableFINX, tabular.environment = "longtable", 
      floating = FALSE,
      include.rownames=FALSE, 
      #add.to.row = list(pos,command),
      file = "Disposition Table Revised.tex")

#### Sumary table ----
alldtaL1 <- list(al,az,co,fl,ga,ia,ky,me,mi,mn,mt,nc,nh,nv,oh,or,pa,tx,va,wa,wi)
alldtaL1.char <- toupper(c("al","az","co","fl","ga","ia","ky","me","mi","mn","mt","nc","nh","nv","oh","or","pa","tx","va","wa","wi"))

alldtaL <- list(al,az,co,fl,ia,mi,mn,nc,nv,pa,tx,wi)
alldtaL.char <- toupper(c("al","az","co","fl","ia","mi","mn","nc","nv","pa","tx","wi"))

dfall <- data.frame(State = character(),
                    Total = numeric(),
                    Completed = numeric(),
                    Refused = numeric(),
                    Contacted = numeric(),
                    Cooperation = numeric(),
                    stringsAsFactors = F) 

for(i in 1:12){
  dfall[i,] <- NA
  dfall$State[i] <- print(alldtaL.char[i])
  dfall$Total[i] <- print(length(alldtaL[[i]]$dispo.cleaned))
  dfall$Completed[i] <- print(length(which(alldtaL[[i]]$dispo.cleaned == 1)))
  dfall$Refused[i] <- print(length(which(alldtaL[[i]]$dispo.cleaned == 2)))
  dfall$Contacted[i] <- print(dfall$Completed[i] + dfall$Refused[i])
  dfall$Cooperation[i] <- print(dfall$Completed[i] / dfall$Contacted[i])
}

dfall$Party <- NA
dfall$Party <- c("Imputed",
                 "Registration",
                 "Registration",
                 "Registration",
                 "Registration",
                 "Imputed",
                 "Imputed",
                 "Registration",
                 "Registration",
                 "Registration",
                 "Imputed",
                 "Imputed")

dfall$`Final Poll` <- NA
dfall$`Final Poll` <- c("N", "Y", "Y", "N", "Y", "Y", "Y", "N", "N", "Y", "N", "Y")
dfall$`Final Poll`[dfall$Final == "Y"] <- "Yes"
dfall$`Final Poll`[dfall$Final == "N"] <- "No"

dfall$Cooperation <- round(dfall$Cooperation, 3)

dfall1 <- dfall[,c(1:2,5,4,3,6:8)]

library(xtable)
dfalltable <- xtable(dfall1, type = "latex", 
                     digits=c(0,0,0,0,0,0,3,0,0),
                     align = c("l","l","r","r","r","r","c","l","c"))
print(dfalltable, 
      floating = FALSE,
      include.rownames=FALSE, 
      file = "Disposition Summary Table Final3.tex")
