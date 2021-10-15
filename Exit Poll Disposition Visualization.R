setwd("/Users/kiraflemke/Desktop/Research Projects/NBC/Graphs")

library(ggplot2)
library(rio)

dat.orig <- dat

dat <- dat[!is.na(dat$dispo.cleaned),]

dat$complete <- NA
dat$complete[dat$dispo.cleaned == 1] <- "complete"
dat$complete[dat$dispo.cleaned != 1 & !is.na(dat$dispo.cleaned)] <- "attempt"
table(dat$complete)

exitpoll <- import("xtab.csv")
cols <- c(13,15)
rows <- c(118:119, 151:153, 169:171, 246:241, 113:115)
exitdem <- exitpoll[rows, cols]

exitdem$Groups <- c(exitdem$AnswerString[1:2],"Hispanic",exitdem$AnswerString[4:5], "Independent", exitdem$AnswerString[7:14], "Rural", "Suburban", "Urban")
exitdem1 <- exitdem[9:14,]
exitdem.n <- exitdem[1:3,]

exitdem.n[1,1] <- "Under 30"
exitdem.n[1,3] <- "Under 30"
exitdem.n[1,2] <- exitdem[10,2]+exitdem[9,2]

exitdem.n[2,1] <- "30-49"
exitdem.n[2,3] <- "30-49"
exitdem.n[2,2] <- exitdem[11,2]+exitdem[12,2]

exitdem.n[3,1] <- "Over 50"
exitdem.n[3,3] <- "Over 50"
exitdem.n[3,2] <- exitdem[13,2]+exitdem[14,2]
exitdem.n

exitdem <- rbind(exitdem[c(1:8),], exitdem.n, exitdem[c(15:17),])

## RACE ----
table(dat$white)
table(dat$black)
table(dat$hispanic)

dat$race <- NA
dat$race[dat$white == 1] <- "White"
dat$race[dat$black == 1] <- "Black"
dat$race[dat$hispanic == 1] <- "Hispanic"
table(dat$race, useNA = "ifany")

racetab <- table(dat$complete, dat$race, useNA = "ifany")
racetab.p <- prop.table(racetab, 1)

## GENDER ----
table(dat$female)
dat$gender <- NA
dat$gender[dat$female == 1] <- "Female"
dat$gender[dat$female == 0] <- "Male"

gentab <- table(dat$complete, dat$gender)
gentab.p <- prop.table(gentab, 1)
 

## PARTY ----
table(dat$likely.party)

dat$party <- NA
dat$party[dat$likely.party == "D"] <- "Democrat"
dat$party[dat$likely.party == "R"] <- "Republican"
dat$party[dat$likely.party == "I"] <- "Independent"


partytab <- table(dat$complete, dat$party)
partytab.p <- prop.table(partytab, 1)

## AGE ----
table(dat$AgeUnder30)
table(dat$Age3039)
table(dat$Age4049)
table(dat$Age5059)
table(dat$Age6074)
table(dat$Age75p)

dat$age <- NA
dat$age[dat$AgeUnder30 == 1] <- "Under 30"
dat$age[dat$Age3039 == 1] <- "30-49"
dat$age[dat$Age4049 == 1] <- "30-49"
dat$age[dat$Age5059 == 1] <- "Over 50"
dat$age[dat$Age6074 == 1] <- "Over 50"
dat$age[dat$Age75p == 1] <- "Over 50"
table(dat$age, useNA = "ifany")

agetab <- table(dat$complete, dat$age)
agetab.p <- prop.table(agetab, 1)

## POP DENSITY ---- 
table(dat$density)
table(dat$state.density)

dat$den <- NA
dat$den[dat$density == "R"] <- "Rural"
dat$den[dat$density == "S"] <- "Suburban"
dat$den[dat$density == "U"] <- "Urban"


dentab <- table(dat$complete, dat$den)
dentab.p <- prop.table(dentab, 1)

#####
## Plotting ----

comtab <- cbind(gentab.p, racetab.p[,c(1:3)], partytab.p, agetab.p, dentab.p)
pops <- c("Sample", "Nonresponse")
dems <- c("Race", "Party", "Age", "Density")
dems2 <- c("Gender", "Race", "Party", "Age", "Density")

sample.group <- c(rep(pops , ncol(comtab)), rep("Exit Poll Final", ncol(comtab)))
group.prop <- c(comtab[1:length(comtab)], exitdem$Total)
group.name <- c(rep(colnames(comtab), each = 2), exitdem$Groups)
dem.group1 <- c(rep("Gender" , 2), rep(dems, each = 3) )
dem.group <- c(rep(dem.group1, each = 2), dem.group1)
               
comtab.order <- c(colnames(comtab)[1:8] , colnames(comtab)[c(11,9,10)], colnames(comtab)[12:14])
pops.order <- c("Sample", "Nonresponse", "Exit Poll Final")

comdta <- data.frame(Source = factor(sample.group, levels = pops.order),
                      Data = group.prop,
                      Labels =  factor(group.name, levels = comtab.order), 
                      Demtype = factor (dem.group, levels = dems2))

newlev <- c(paste(levels(comdta$Labels)[1:5]),c("Dem", "Ind", "Rep"), c("18-30","30-49","50+"), paste(levels(comdta$Labels)[12:14]))
levels(comdta$Labels) <- newlev

comdta <- comdta[order(comdta$Labels),]


## Test Plots ----
cols <- c("white", "grey", "black")
  
ggplot(comdta, aes(Labels, Data, fill = Source)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_text(aes(label = round(Data, 2)), position = position_dodge(width = 1), vjust=-0.25, size = 3) +
  theme_classic() + 
  scale_fill_manual(values = cols)+ 
  xlab("") +
  ylab("Proportion")

ggplot(comdta, aes(Labels, Data, color = Source)) +
  geom_bar(stat="identity", fill = "white", position = "dodge") +
  geom_text(aes(label = round(Data, 2)), position = position_dodge(width = 1), vjust=-0.25, size = 3) +
  theme_classic() + 
  xlab("") +
  ylab("Proportion")

ggplot(comdta, aes(Labels, Data, fill = Source)) +
  geom_bar(stat="identity", position = position_dodge(width=0.5), width = .15) +
  geom_text(aes(label = round(Data, 2)), position = position_dodge(width = .15), vjust=-0.25, size = 3) +
  theme_classic() + 
  xlab("") +
  ylab("Proportion")

## Final Plots ----
labelcat <- c("Gender", "Race", "Party", "Age", "Density")
labelbr <- c(2.5, 5.5,8.5,11.5, 14.5)

  # grouped by demographic measure ----
ggplot(comdta, aes(Labels, Data, fill = Source, color = Source)) +
  geom_bar(stat="identity", position = position_dodge(width=.5), width = .07) +
  geom_text(aes(label = round(Data*100, 0)), position = position_dodge2(width = 0.8, padding = .2), vjust=-0.25, size = 3) +
  theme_classic()+
  theme(legend.position="top", legend.title = element_blank(), legend.key.size = unit(.5,"line"))+
  geom_vline(xintercept = labelbr) +
  annotate(geom="text", x = (labelbr -.15), y=rep(.84 , 5), label=labelcat, angle = 90, fontface =2)+
  xlab("") +
  ylab("")

  # stacked by sample source ----

comdta$Dem.Source <- NA
comdta$Dem.Source <- paste(comdta$Demtype, comdta$Source, sep = ".")
comdta$Dem.Source <- factor(comdta$Dem.Source, levels = unique(comdta$Dem.Source)[c(4:15,1:3)])

comdta2 <- comdta[order(comdta$Dem.Source),]

label.order <- c(3:14, 1:2)

labels2 <- paste(labelcat[c(2:5,1)], sep = paste(rep(" ", 20)))

ggplot(comdta2, aes(Dem.Source, Data, fill = factor(Labels, levels = unique(comdta$Labels)[label.order]), color = factor(Labels, levels = unique(comdta$Labels)[label.order]))) +
  geom_bar(stat="identity", position = position_stack(), width = .07) +
  geom_text(aes(label = round(Data*100, 0)), position = position_stack(),size = 3, hjust = 1.4) +
  theme_classic()+
  theme(axis.ticks.x = element_blank()) +
  scale_x_discrete(breaks = comdta2$Dem.Source, labels=comdta2$Source, guide = guide_axis(angle = 45)) +
  labs(subtitle = paste(labelcat[c(2:5,1)], collapse = "                        ")) +
  theme(legend.position="top", legend.title = element_blank(), legend.key.size = unit(1,"line"), 
        legend.spacing.x = unit(0.25, 'cm'),legend.spacing.y = unit(0.02, 'cm'), 
        legend.text = element_text(margin = margin(r = 57, unit = "pt")), 
        plot.subtitle = element_text(face = "bold", size = 12)) +
  xlab("") +
  ylab("")

  # diff from exit ----

n.exit <- comdta[comdta$Source != "Exit Poll Final",]
n.exit <- n.exit[order(n.exit$Source),]

exit <- comdta[comdta$Source == "Exit Poll Final",]
colnames(exit) <- paste(colnames(exit), "ex", sep = ".")

exit2 <- rbind(exit,exit)
comdta2 <- cbind(n.exit, exit2)

comdta2$Ex.diff <- NA
comdta2$Ex.diff <- comdta2$Data.ex*100 - comdta2$Data*100

comdta3 <- comdta2[order(comdta2$Labels),]
valsigns <- ifelse(comdta3$Ex.diff >= 0, -.6, 1)

ggplot(comdta2, aes(Labels, Ex.diff, fill = Source, color = Source)) +
  geom_bar(stat="identity", position = position_dodge(width=.5), width = .2) +
  geom_text(aes(label = round(Ex.diff, 2)), position = position_dodge2(width = .75), size = 3, vjust = 1.2*valsigns) +
  theme_classic()+
  scale_color_manual(values = c("grey55","black")) +
  scale_fill_manual(values = c("grey55", "black"))+
  theme(legend.position="top", legend.title = element_blank(), legend.key.size = unit(.5,"line"),
        plot.title = element_text(hjust = 0.5, size = 12))+
  geom_vline(xintercept = labelbr) +
  geom_vline(xintercept = (c(1:14)+.5), size = .25, colour = "grey") +
  geom_hline(yintercept = 0, size =.25, linetype = "dashed") +
  annotate(geom="label", x = (labelbr -1.25), y=rep(15 , 5), label=labelcat, fontface =2, size = 4)+
  ggtitle("Difference in Demographic Percentage of Phone Poll Sample Groups Compared to Exit Poll") +
  labs(caption = "values = percent of final weighted exit poll - percent of sample group") +
  xlab("") +
  ylab("Difference of Percents")


  # exit diff ----

comdta2a <- cbind(n.exit, exit2)

comdta2a$Diff.ex <- NA
comdta2a$Diff.ex <- comdta2a$Data*100 - comdta2a$Data.ex*100

comdta2a <- comdta2a[order(comdta2a$Labels),]
valsigns2 <- ifelse(comdta2a$Diff.ex >= 0, -.6, 1)

ggplot(comdta2a, aes(Labels, Diff.ex, fill = Source, color = Source)) +
  geom_bar(stat="identity", position = position_dodge(width=.5), width = .2) +
  geom_text(aes(label = round(Diff.ex, 2)), position = position_dodge2(width = .75), size = 3, vjust = 1.2*valsigns2) +
  theme_classic()+
  scale_color_manual(values = c("grey55","black")) +
  scale_fill_manual(values = c("grey55", "black"))+
  theme(legend.position="top", legend.title = element_blank(), legend.key.size = unit(.5,"line"),
        plot.title = element_text(hjust = 0.5, size = 12))+
  geom_vline(xintercept = labelbr) +
  geom_vline(xintercept = (c(1:14)+.5), size = .25, colour = "grey") +
  geom_hline(yintercept = 0, size =.25, linetype = "dashed") +
  annotate(geom="label", x = (labelbr -1.25), y=rep(15 , 5), label=labelcat, fontface =2, size = 4)+
  ggtitle("Difference in Demographic Percentage of Phone Poll Sample Groups Compared to Exit Poll") +
  labs(caption = "values = percent of sample group - percent of final weighted exit poll") +
  xlab("") +
  ylab("Difference of Percents")

