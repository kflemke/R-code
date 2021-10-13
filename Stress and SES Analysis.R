setwd("/Users/kiraflemke/Desktop/R Projects/Stress")

library(ggplot2)
library(maps)
library(questionr)

# map of US states by mandatory kindergarten ----
# pull state boundaries from map_data
states <- map_data("state")

#View(states)

# load data 
library(rio)
kind.dta <- import("Kindergarten Data.xlsx")

# recode character to binary
kind.dta$State <- tolower(kind.dta$State)
kind.dta$`Require Kindergarten`[kind.dta$`Require Kindergarten` == "Yes"] <- 1
kind.dta$`Require Kindergarten`[kind.dta$`Require Kindergarten` == "Yes*"] <- 0
kind.dta$`Require Kindergarten`[kind.dta$`Require Kindergarten` == "No"] <- 0
kind.dta$`Require Kindergarten`[kind.dta$`Require Kindergarten` == "No*"] <- 1
kind.dta$`Require Kindergarten`[kind.dta$`Require Kindergarten` == "NA"] <-NA
kind.dta$`Require Kindergarten` <- as.numeric(kind.dta$`Require Kindergarten`)

# merge data retaining information from all states
k.states <- merge(states, kind.dta, by.x = "region", by.y = "State", all.x = T)

# plot map data based on kindergarten policy
ggplot(data=k.states) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Require Kindergarten`), 
               colour="black", size=.2) +
  scale_fill_gradientn(colours = c("skyblue4", "cyan3"), breaks = c(0,1),
                       limits=c(0,1), guide = "legend",
                       name = "Kindergarten Attendance \n Required by State Law", labels = c("No", "Yes")) +
  coord_fixed(1.3) +
  theme_void()

# policy list ----
# import education policy data 
policy.dta <- import("Edu Policy Data.xlsx")

# recode to display characters 
policy.dta[policy.dta == 1] <- "X"
policy.dta[policy.dta == 0] <- ""

# transpose and subset
policy.dta2 <- data.frame(t(policy.dta))
colnames(policy.dta2) <- policy.dta$State
policy.dta2 <- policy.dta2[2:7,]

# format for display
library(gt)
policy.dta1 <- gt(policy.dta2, 
                  rownames_to_stub = T)

# create organized state policy list 
data_color(policy.dta1,
  columns = c(2:51),
  colors = c("white", "darkseagreen1"),
  apply_to = c("fill")
)

policy.dta[,2] <- NULL
policy.dta1X <- gt(policy.dta)

# output html format to embed in webpage
data_color(policy.dta1X,
           columns = c(2:6),
           colors = c("white", "darkseagreen1"),
           apply_to = c("fill")
)


# NHIS - stress and income adult ----
# load person-level data from the National Health Interview Study 
#View(person.dta)
summary(person.dta$WTFA)

# subset to those that mentioned suffering from anxiety 
anx.person.dta <- person.dta[person.dta$LAHCA17 == "(1) Mentioned",]
dim(anx.person.dta)

# apply weights to identify income level among anxious and non-anxious respondents 
table(person.dta$RECTYPE)
table1 <- wtd.table(anx.person.dta$ERNYR_P, weights = anx.person.dta$WTFA, digits = 0, na.rm = TRUE)

table1.all <- wtd.table(person.dta$ERNYR_P, weights = person.dta$WTFA, digits = 0, na.rm = TRUE)

inc.prop.tb <- (table1/table1.all)
Linc.prop.tb <- as.data.frame(inc.prop.tb)

library(RColorBrewer)
ggplot(Linc.prop.tb, aes(x = Var1, y = Freq, fill = Freq))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = percent(Freq)), size = 4, hjust = -.1) +
  scale_fill_gradient2(low = "peachpuff", high = "orangered3")+
  coord_flip()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  scale_x_discrete(labels = substr(Linc.prop.tb$Var1,5,21))+
  scale_y_continuous(limits = c(0,0.03))+
  ggtitle("Reported Depression and/or Anxiety by Income") +
  ylab("Percent of each income group (weighted)") +
  xlab("") +
  labs(caption = "* Data Source: 2011 National Health Interview Study")




# NHIS - stress and income child ----
# load child and family level data from National Health Interview Study 
child.dta <- da36145.0005
fam.dta <- da36145.0002

# subset family data to selected income and ID variables
sub.fam.dta <- fam.dta[,c("INCGRP2", "INCGRP3", "HHX")]

# collapse into new variable to include both boys and girls in analysis
table(child.dta$MHIGRL2)
table(child.dta$MHIBOY2)

child.dta$MHI <- NA
child.dta$MHI <- ifelse(!is.na(child.dta$MHIBOY2), child.dta$MHIBOY2, child.dta$MHIGRL2)

# merge to retain all individual child level data paired with family income 
child.new.dta <- merge(child.dta, sub.fam.dta, by = "HHX", all.x = T)

table(child.new.dta$INCGRP2, child.new.dta$MHI)

# identify weighted averages
m1 <- wtd.mean(child.new.dta[child.new.dta$INCGRP2 == "(1) $0 - $34,999",]$MHI, 
         weights = child.new.dta[child.new.dta$INCGRP2 == "(1) $0 - $34,999",]$WTFA, na.rm = TRUE)

m2 <- wtd.mean(child.new.dta[child.new.dta$INCGRP2 == "(2) $35,000 - $49,999",]$MHI, 
         weights = child.new.dta[child.new.dta$INCGRP2 == "(2) $35,000 - $49,999",]$WTFA, na.rm = TRUE)

m3 <- wtd.mean(child.new.dta[child.new.dta$INCGRP2 == "(3) $50,000 - $74,999",]$MHI, 
         weights = child.new.dta[child.new.dta$INCGRP2 == "(3) $50,000 - $74,999",]$WTFA, na.rm = TRUE)

m4 <- wtd.mean(child.new.dta[child.new.dta$INCGRP2 == "(4) $75,000 - $99,999",]$MHI, 
               weights = child.new.dta[child.new.dta$INCGRP2 == "(4) $75,000 - $99,999",]$WTFA, na.rm = TRUE)

m5 <- wtd.mean(child.new.dta[child.new.dta$INCGRP2 == "(5) $100,000 and over",]$MHI, 
               weights = child.new.dta[child.new.dta$INCGRP2 == "(5) $100,000 and over",]$WTFA, na.rm = TRUE)

newdf <- cbind.data.frame(c(m1,m2,m3,m4,m5), c("(1) $0 - $34,999", "(2) $35,000 - $49,999", "(3) $50,000 - $74,999", "(4) $75,000 - $99,999", "(5) $100,000 and over"))
names(newdf) <- c("mean", "group")

ggplot(newdf, aes(x = group, y = mean, fill = mean))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = round(mean, 3)), size = 4, hjust = -.1) +
  scale_fill_gradient2(low = "white", high = "brown")+
  coord_flip()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  scale_x_discrete(labels = substr(newdf$group,5,21))+
  scale_y_continuous(limits = c(0,2))+
  ggtitle("Child Mental Health Score by Houshold Income") +
  ylab("Mean value of each houshold income group (weighted)") +
  xlab("") +
  labs(caption = "**Mental Health Score ranges from 0 (no mental health problems) to 8 (severe mental health problems)  \n *Data Source: 2011 National Health Interview Study")

table(child.new.dta$INCGRP2)
child.new.dta$INCGRP <- child.new.dta$INCGRP2
child.new.dta$INCGRP[child.new.dta$INCGRP %in% c("(6) $0 - $49,999 (no further detail)", "(7) $50,000 and over (no further detail)", "(8) $50,000 - $99,999 (no further detail)")] <- NA

# collapse to 2 income groups - over and under $50k - do not have personal ID info like location to account for local cost of living
child.new.dta.50 <- child.new.dta[child.new.dta$INCGRP2 %in% c("(1) $0 - $34,999", "(6) $0 - $49,999 (no further detail)", "(2) $35,000 - $49,999"),]
child.new.dta.ov <- child.new.dta[child.new.dta$INCGRP2 %in% c("(7) $50,000 and over (no further detail)", "(8) $50,000 - $99,999 (no further detail)",
                                                               "(3) $50,000 - $74,999", "(4) $75,000 - $99,999", "(5) $100,000 and over"),]

#density plot
colors <- c("< $50k" = "red", "> $50k" = "blue")

ggplot(child.new.dta, aes(MHI, weight = WTFA_SC)) + 
  geom_density(data = child.new.dta.50, fill = "red", color = "red", alpha = 0.2) + 
  geom_density(data = child.new.dta.ov, fill = "blue", color = "blue", alpha = 0.2) + 
  theme_classic()+
  geom_text(aes(label = "Annual Household Income > $50k", x = 1, y = .45), color = "blue", hjust = -.05) +
  geom_text(aes(label = "Annual Household Income < $50k", x = 2, y = .25), color = "red", hjust = -.1)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0,8,1))+
  ggtitle("Child Mental Health Score Density by Houshold Income") +
  ylab("Proportion of each houshold income group (weighted)") +
  xlab("Mental Health Score") +
  labs(caption = "**Mental Health Score ranges from 0 (no mental health problems) to 8 (severe mental health problems)  \n *Data Source: 2011 National Health Interview Study")

# histogram version
ggplot(child.new.dta, aes(MHI, weight = WTFA_SC)) + 
  geom_histogram(data = child.new.dta.50, aes(y=..density..), fill = "red", color = "red", alpha = 0.2) + 
  geom_histogram(data = child.new.dta.ov, aes(y=..density..), fill = "blue", color = "blue", alpha = 0.2) + 
  theme_classic()+
  geom_text(aes(label = "Annual Household Income > $50k", x = 1, y = .45), color = "blue", hjust = -.05) +
  geom_text(aes(label = "Annual Household Income < $50k", x = 2, y = .25), color = "red", hjust = -.1)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0,8,1))+
  ggtitle("Child Mental Health Score Density by Houshold Income") +
  ylab("Proportion of each houshold income group (weighted)") +
  xlab("Mental Health Score") +
  labs(caption = "**Mental Health Score ranges from 0 (no mental health problems) to 8 (severe mental health problems)  \n *Data Source: 2011 National Health Interview Study")

# NCES - income and MH memory ----

# load data from national Center for Education Statistics

### recode memory issues variable and plot density difference between SES groups
nces.dta <- hsls_17_student_pets_sr_v1_0
colnames(nces.dta)

summary(nces.dta$X1SES_U)
nces.dta$X1SES_U[nces.dta$X1SES_U == -8] <- NA

attributes(nces.dta$S4MHDISBL)
N.mh.nces.dta <- nces.dta[nces.dta$S4MHDISBL == "No",]
Y.mh.nces.dta <- nces.dta[nces.dta$S4MHDISBL == "Yes",]

ggplot(nces.dta, aes(X1SES_U, weight = W1STUDENT)) + 
  geom_density(data = N.mh.nces.dta, fill = "darkmagenta", color = "darkmagenta", alpha = 0.2) + 
  geom_density(data = Y.mh.nces.dta, fill = "hotpink1", color = "hotpink1", alpha = 0.2) + 
  geom_vline(xintercept = wtd.mean(N.mh.nces.dta$X1SES_U, weights = N.mh.nces.dta$W1STUDENT), color = "darkmagenta", linetype = "dashed")+
  geom_vline(xintercept = wtd.mean(Y.mh.nces.dta$X1SES_U, weights = Y.mh.nces.dta$W1STUDENT), color = "hotpink1", linetype = "dashed")+
  theme_classic()
  
# barplot of SES quintiles

nces.dta$MH[nces.dta$S4MHDISBL == "No"] <- 0
nces.dta$MH[nces.dta$S4MHDISBL == "Yes"] <- 1

tableMH <- wtd.table(nces.dta$MH, nces.dta$X1SESQ5_U, weights = nces.dta$W1STUDENT)
tableMH.prop <- prop.table(tableMH[,1:5], margin = 2)
tableMH.propF <- data.frame(tableMH.prop)

ggplot(tableMH.propF, aes(x=Var2, y=Freq, fill = Var1, color = Var1)) +
  geom_bar(stat="identity", position = "identity", alpha = 0.3)+
  theme_classic()+
  scale_fill_manual(name = "Experienced difficulty concentrating/remembering/deciding due to mental health", 
                    values = c("mediumpurple3", "hotpink1"), 
                    labels = c("No", "Yes")) +
  scale_color_manual(name = "Experienced difficulty concentrating/remembering/deciding due to mental health", 
                     values = c("mediumpurple3", "hotpink1"), 
                     labels = c("No", "Yes")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")+
  ggtitle("SES and Mental-health-related Learning Difficulty \n (among individuals reporting mental health issues)") +
  ylab("Proportion of SES quintile") +
  xlab("") +
  labs(caption = "**SES quintile value takes into account parent education, income, occupation, and local population density  \n *Data Source: 2017 NCES High School Longitudinal Study")


# NCES - income and mental health difficulty ----
  
attributes(nces.dta$P1MOOD)
  Lot.nces.dta <- nces.dta[nces.dta$P1MOOD == "A lot of difficulty" ,]
  Lit.nces.dta <- nces.dta[nces.dta$P1MOOD == "A little difficulty",]
  No.nces.dta <- nces.dta[nces.dta$P1MOOD == "No difficulty",]

  plot1 <- ggplot(nces.dta, aes(X1SES_U, weight = W1STUDENT)) + 
    geom_density(data = No.nces.dta, fill = "darkgoldenrod1", color = "darkgoldenrod1", alpha = 0.2) + 
    geom_density(data = Lit.nces.dta, fill = "orangered", color = "orangered", alpha = 0.2) + 
    geom_density(data = Lot.nces.dta, fill = "darkred", color = "darkred", alpha = 0.2) + 
    geom_vline(xintercept = wtd.mean(No.nces.dta$X1SES_U, weights = No.nces.dta$W1STUDENT), color = "darkgoldenrod1", linetype = "dashed")+
    geom_vline(xintercept = wtd.mean(Lot.nces.dta$X1SES_U, weights = Lot.nces.dta$W1STUDENT), color = "darkred", linetype = "dashed")+
    geom_vline(xintercept = wtd.mean(Lit.nces.dta$X1SES_U, weights = Lit.nces.dta$W1STUDENT), color = "orangered", linetype = "dashed")+
    theme_classic() 
    
plot1+
    geom_text(aes(label = "Experienced A LOT \n of difficulty feeling \n anxious or depressed ", x = -1.4, y = .55), color = "darkred", size = 3) +
    geom_text(aes(label = "Experienced A LITTLE difficulty \n feeling anxious or depressed ", x = .7, y = .4), color = "orangered", size = 3)+
    geom_text(aes(label = "Experienced NO difficulty \n feeling anxious or depressed ", x = 1.2, y = .3), color = "darkgoldenrod1", size = 3)+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("SES Distribution among groups of students \n who experienced different levels of mental health difficulties") +
    ylab("Proportion of each mental health experience group (weighted)") +
    xlab("SES status composite score") +
    labs(caption = "**SES composite score takes into account parent education, income, occupation, and local population density  \n *Data Source: 2017 NCES High School Longitudinal Study")
 
  no.mean <- wtd.mean(No.nces.dta$X1SES_U, weights = No.nces.dta$W1STUDENT)
  lit.mean <- wtd.mean(Lit.nces.dta$X1SES_U, weights = Lit.nces.dta$W1STUDENT)
  lot.mean <- wtd.mean(Lot.nces.dta$X1SES_U, weights = Lot.nces.dta$W1STUDENT)
  
  plot2 + 
    geom_label(aes(label = no.mean, x = no.mean, y = .6), color = "darkgoldenrod1", hjust = .05, size = 2, hjust = -.2) +
    geom_label(aes(label = lit.mean, x = lit.mean, y = .6), color = "orangered", hjust = .05, size = 2) +
    geom_label(aes(label = lot.mean, x = lot.mean, y = .6), color = "darkred", hjust = .05, size = 2, hjust = .2) 
    
  
  # two difficulty groups
  attributes(nces.dta$P1MOOD)
  some.nces.dta <- nces.dta[nces.dta$P1MOOD %in%  c("A lot of difficulty", "A little difficulty"),]
  none.nces.dta <- nces.dta[nces.dta$P1MOOD == "No difficulty",]
  
  ggplot(nces.dta, aes(X1SES_U, weight = W1STUDENT)) + 
    geom_density(data = none.nces.dta, fill = "darkgoldenrod1", color = "darkgoldenrod1", alpha = 0.2) + 
    geom_density(data = some.nces.dta, fill = "red", color = "red", alpha = 0.2) + 
    geom_vline(xintercept = wtd.mean(none.nces.dta$X1SES_U, weights = none.nces.dta$W1STUDENT), color = "darkgoldenrod1", linetype = "dashed")+
    geom_vline(xintercept = wtd.mean(some.nces.dta$X1SES_U, weights = some.nces.dta$W1STUDENT), color = "red", linetype = "dashed")+
    theme_classic()
  
# NCES - work paying job on school day ----
  
  unique(nces.dta$S1HRWORK)
  nces.dta$WORK <- NA
  nces.dta$WORK[nces.dta$S1HRWORK == "Less than 1 hour"] <- 0
  nces.dta$WORK[nces.dta$S1HRWORK != "Less than 1 hour" & 
                  nces.dta$S1HRWORK != "Unit non-response"& 
                  nces.dta$S1HRWORK != "Missing"] <- 1
  
  tablew <- wtd.table(nces.dta$WORK, nces.dta$X1SESQ5_U, weights = nces.dta$W1STUDENT)
  tablew.prop <- prop.table(tablew[,1:5], margin = 2)
  tablew.propF <- data.frame(tablew.prop)
  
  library(scales)
  ggplot(tablew.propF, aes(x=Var2, y=Freq, fill = Var1, color = Var1)) +
    geom_bar(stat="identity", position = "identity", alpha = 0.3)+
    theme_classic()+
    geom_text(aes(label = percent(round(Freq, 3))),
              size = 3.8, vjust = -.4, show.legend = FALSE) +
    scale_fill_manual(name = "Student works a paying job on typical schoolday", 
                      values = c("goldenrod2", "darkgreen"), 
                      labels = c("No", "Yes")) +
    scale_color_manual(name = "Student works a paying job on typical schoolday", 
                       values = c("goldenrod2", "darkgreen"), 
                       labels = c("No", "Yes")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "top")+
    ggtitle("SES and after-school employment") +
    ylab("Proportion of SES quintile") +
    xlab("") +
    labs(caption = "**SES quintile value takes into account parent education, income, occupation, and local population density  \n *Data Source: 2017 NCES High School Longitudinal Study")
  
# NCES - GPA x SES ----

nces.dta$X3TGPAACAD[nces.dta$X3TGPAACAD < 0] <- NA
  
  ggplot(nces.dta) +
    geom_point(aes(x = nces.dta$X1SES_U), 
               y = nces.dta$X3TGPAACAD) + 
    geom_smooth(method = "lm",formula = y ~ x, 
                aes(x=X1SES_U, y=X3TGPAACAD, group=1))+
    ggtitle("Cumulative GPA by SES") +
    xlab("SES status composite score") +
    ylab("Unweighted cumulative GPA in academic courses") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))+ 
    labs(caption = "**SES composite score takes into account parent education, income, occupation, and local population density  \n *Data Source: 2017 NCES High School Longitudinal Study")

nces.dta$poc <- NA
nces.dta$poc[nces.dta$X1RACE == "White, non-Hispanic"] <- "White"
nces.dta$poc[nces.dta$X1RACE != "White, non-Hispanic" & 
               nces.dta$X1RACE != "Missing"] <- "POC"
table(nces.dta$poc)

nces.dta.poc <- nces.dta[nces.dta$poc == "POC",]
nces.dta.w <- nces.dta[nces.dta$poc == "White",]
    
  poc.plot <- ggplot(nces.dta.poc, aes(x = nces.dta.poc$X1SES_U, 
         y = nces.dta.poc$X3TGPAACAD, weight = nces.dta.poc$W1STUDENT))+
    stat_density_2d(aes(fill = ..level..), geom = "polygon", breaks = seq(0.03,0.3,0.03))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,4)) +
    scale_fill_distiller(palette = "PuRd", direction = 1) +
    geom_smooth(data = nces.dta.w, method = "lm",formula = y ~ x, 
                aes(x=nces.dta.w$X1SES_U, y=nces.dta.w$X3TGPAACAD, weight = nces.dta.w$W1STUDENT), color = "deepskyblue")+
    geom_smooth(data = nces.dta.poc, method = "lm",formula = y ~ x, 
                aes(x=nces.dta.poc$X1SES_U, y=nces.dta.poc$X3TGPAACAD, weight = nces.dta.poc$W1STUDENT), color = "deeppink")+
    ggtitle("Students of Color")+
    theme(plot.title = element_text(hjust = 0.5, color = "deeppink"), legend.position = "none")+ 
    xlab("") +
    ylab("") 

   
  w.plot <- ggplot(nces.dta.w, aes(x = nces.dta.w$X1SES_U, 
                        y = nces.dta.w$X3TGPAACAD , weight = nces.dta.w$W1STUDENT))+
    stat_density_2d(aes(fill = ..level..), geom = "polygon", breaks = seq(0.03,0.3,0.03))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,4)) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    geom_smooth(data = nces.dta.w, method = "lm",formula = y ~ x, 
                aes(x=nces.dta.w$X1SES_U, y=nces.dta.w$X3TGPAACAD, weight = nces.dta.w$W1STUDENT), color = "deepskyblue")+
    geom_smooth(data = nces.dta.poc, method = "lm",formula = y ~ x, 
                aes(x=nces.dta.poc$X1SES_U, y=nces.dta.poc$X3TGPAACAD, weight = nces.dta.poc$W1STUDENT), color = "deeppink")+
    ggtitle("White Students") +
    theme(plot.title = element_text(hjust = 0.5, color = "deepskyblue"), legend.position = "none")+ 
    xlab("") +
    ylab("") 
    
  library(ggplot2)
  library(gtable)
  library(grid)
  library(gridExtra)
   
  # diplay plots in same window 
  grid.arrange(poc.plot, w.plot, ncol=2,
                 top = textGrob("Density of Cumulative GPA by SES \n ", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
                 left = textGrob("Unweighted cumulative GPA in academic courses", rot = 90, vjust = 1),
                 bottom = textGrob("SES status composite score"))
    
  # jitter plot 
  nces.dta$n.X1SESQ5_U <- NA
  nces.dta$n.X1SESQ5_U[nces.dta$X1SESQ5_U == "Fifth quintile (highest)"] <- 5
  nces.dta$n.X1SESQ5_U[nces.dta$X1SESQ5_U == "Fourth quintile"] <- 4
  nces.dta$n.X1SESQ5_U[nces.dta$X1SESQ5_U == "Third quintile"] <- 3
  nces.dta$n.X1SESQ5_U[nces.dta$X1SESQ5_U == "Second quintile"] <- 2
  nces.dta$n.X1SESQ5_U[nces.dta$X1SESQ5_U == "First quintile (lowest)"] <- 1
  
  ggplot(nces.dta, aes(x = nces.dta$X1SESQ5_U, 
                       y = nces.dta$X3TGPAACAD))+
    geom_jitter(pch = 1)+
    geom_smooth(method = "lm",formula = y ~ x, 
                aes(y=nces.dta$n.X1SESQ5_U, x=nces.dta$X3TGPAACAD, group=nces.dta$poc))+
    ggtitle("Cumulative GPA by SES") +
    xlab("SES status composite score") +
    ylab("Unweighted cumulative GPA in academic courses") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))+ 
    labs(caption = "**SES composite score takes into account parent education, income, occupation, and local population density  \n *Data Source: 2017 NCES High School Longitudinal Study")
  
  