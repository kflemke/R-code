
#### load data ----
load("NYC taxi trips.Rdata")
load("Sample 1000 - NYC taxi trips.Rdata")
load("16 Personality Data.Rdata")
#### Line Graph example 1 - Slide 13 ----
# create data frame of trip counts for each day in March
tripsxday <- data.frame(table(nyc.taxi.dta$day))

# plot the number of trips per day 
# over the course of March 2020
ggplot(tripsxday) +
  geom_line(aes(y = Freq, 
                x = Var1,
                group = 1))

#### Grouped line Graph example 2 - Slide 14 ----
# create two-way table trip counts for each day in March
# broken out by passenger number
tripsxday.pass <- data.frame(table(nyc.taxi.dta$day, 
                                   nyc.taxi.dta$passenger_count))
# subset to only passenger numbers between 1 and 6
tripsxday.pass <- tripsxday.pass[tripsxday.pass$Var2 %in% c(1:6),]
# rename columns
names(tripsxday.pass) <- c("Date in March 2020", 
                           "Passenger #", "Number of trips")

# plot the number of trips per day over the course of March 2020
# with each line group & color representing a passenger number
ggplot(tripsxday.pass) +
  geom_line(aes(y = `Number of trips`, x = `Date in March 2020`,
                group = `Passenger #`, 
                color = `Passenger #`))

#### Scatterplot example Slides 16-26 ----
install.packages("ggplot")
library(ggplot2)
library(scales)
library(RColorBrewer)

ggplot(nyc.taxi.dta.sub) +
  geom_point(aes(x = as.numeric(duration, "minutes"), 
                 y = tip_amount, 
                 color = time.of.day,
                 size = passenger_count),pch = 1) + 
  facet_wrap(vars(payment_type)) +
  ggtitle("NYC Taxi tip amount by ride length in March 2020", ) +
  xlab("Duration of ride in minutes") +
  ylab("Value of tip in USD") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")+ 
  scale_x_continuous(limits = c(0,80), breaks = seq(0,80,10))+
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0,21), breaks = seq(0,21,2))+
  scale_color_brewer(palette="Set1", na.translate = F)+
  scale_size(limits = c(1,6))+
  labs(col="Time of Day", size = "Passenger Number")

#### Simple Bar plot examples Slides 29-32 ----
personality.dta1 <- personality.dta[personality.dta$country %in% c("CA", "US", "AU"),]
dim(personality.dta1)

personality.dta1$gender[personality.dta1$gender == 1] <- "Men"
personality.dta1$gender[personality.dta1$gender == 2] <- "Women"

# create gender frequency table
genders <- table(personality.dta1$gender)
# display frequencies as proportions
genders.prop <- prop.table(genders)
# save as a data frame 
genders.prop.dta <- data.frame(genders.prop)

# repeat the process to create a country data table
countries <- table(personality.dta1$country)
countries.prop <- prop.table(countries)
countries.prop.dta <- data.frame(countries.prop)

# simple gender bar plot
ggplot(countries.prop.dta, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

# changing theme content
ggplot(countries.prop.dta, aes(x=Var1, y=Freq, 
                               fill = Var1)) +
  geom_bar(stat="identity", color = "black") +
  geom_text(aes(label = percent(round(Freq, 2))),
            size = 3.8, vjust = -.4) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.position = "none")+
  ggtitle("Country of Respondents") +
  ylab("Proportion of total responses") +
  xlab("") 

# simple country bar plot
ggplot(genders.prop.dta, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

# changing bar color by country and theme content
ggplot(genders.prop.dta, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity" , fill = "lightblue", 
           color = "black") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),)+
  ggtitle("Gender of Respondents") +
  ylab("Proportion of total responses") +
  xlab("") 

#### Grouped Bar plot example Slides 34-36 ----
# create two-way frequency table for country and gender
countries.gen <- table(personality.dta1$country, 
                       personality.dta1$gender)

# display as row proportions
# gender proportions within each country
countries.gen.prop <- prop.table(countries.gen, 1)

# save as a data frame 
countries.gen.prop.dta <- data.frame(countries.gen.prop)

# stacked bar plot
ggplot(countries.gen.prop.dta, 
       aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")+
  geom_text(y = rep(c(.75,.5),3, each = T),
            aes(label = round(Freq, 2)), 
            size = 3.8,)

# grouped bar plot - by country
ggplot(countries.gen.prop.dta, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = round(Freq, 2)), 
            position = position_dodge(width = .9),
            size = 3.8, vjust = -.4) +
  theme_classic()+
  scale_fill_manual(values = c("aquamarine3", "mediumpurple3")) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))+
  ggtitle("Gender of respondents from each country") +
  ylab("Proportion of total country responses") +
  xlab("") +
  labs(fill = "Gender")

# grouped bar plot - by gender
ggplot(countries.gen.prop.dta, aes(x=Var2, y=Freq, fill = Var1)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label = round(Freq, 2)), 
            position = position_dodge(width = .9),
            size = 3.8, vjust = -.4) +
  theme_classic()+
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))+
  ggtitle("Gender of respondents from each country") +
  ylab("Proportion of total country responses") +
  xlab("") +
  labs(fill = "Country")



#### Mean value Bar plot example Slide 38 ----
# identifying the mean of each personality trait score
personality.means <- data.frame(sapply(personality.dta[169:184], mean))
# binding together row names and values to create a labeled long-format data frame
personality.means.dta <- cbind.data.frame(rownames(personality.means), personality.means)
# renaming columns
names(personality.means.dta) <- c("trait", "mean.val")
# removing original row names
rownames(personality.means.dta) <- NULL


ggplot(personality.means.dta, 
       aes(x = reorder(trait, mean.val), y = mean.val)) +
  geom_bar(stat = "identity", fill = "plum", 
           color = "black", width = .7) +
  geom_text(aes(label = round(mean.val, 2)), 
            size = 3.8, hjust = -.1) +
  coord_flip()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Mean Value of 16 Personality Traits") +
  ylab("Mean Value (Scale 0 to 1)") +
  xlab("") 

#### Gender grouped Mean value Bar plot example Slide 40-41 ----
personality.dta$gender[personality.dta$gender == 3] <- NA

# find the mean of each personality trait by gender
personality.gen <- aggregate(. ~ personality.dta$gender , 
                             personality.dta[169:184], FUN = mean)
# transpose the dataframe 
personality.gen <- t(personality.gen)
# binding together row names and values to create a labeled data frame
personality.gen.dta <- cbind.data.frame(rownames(personality.gen), personality.gen)
# removing original row names
rownames(personality.gen.dta) <- NULL
# removing first column containing gender totals
personality.gen.dta <- personality.gen.dta[-1,]
# renaming columns
names(personality.gen.dta) <- c("Trait","Men", "Women")

# reshaping data to be in a long format - this step is necessary 
# because we have more than one categorical variable 
long.personality.gen.dta <- pivot_longer(personality.gen.dta,
                                         cols = c(2:3),
                                         names_to = "gender",
                                         values_to = "mean")

ggplot(data=long.personality.gen.dta, 
       aes(x = reorder(Trait, mean), y = mean, 
           fill = gender)) +
  geom_bar(stat="identity")+
  coord_flip()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Mean Value of 16 Personality Traits") +
  ylab("Mean Value (Scale 0 to 1)") +
  xlab("") 

ggplot(data=long.personality.gen.dta, 
       aes(x = reorder(Trait, mean), y = mean, 
           fill = gender)) +
  geom_bar(stat="identity")+
  coord_flip()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Mean Value of 16 Personality Traits") +
  ylab("Mean Value (Scale 0 to 1)") +
  xlab("") 

ggplot(data=long.personality.gen.dta, 
       aes(x = reorder(Trait, mean), y = mean, fill = gender)) +
  geom_bar(stat="identity", 
           position = position_dodge()) +
  geom_text(aes(label = round(mean, 2)), size = 3, hjust = -.1,
            position = position_dodge(width = .75)) +
  theme_classic()+
  coord_flip()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Mean Value of 16 Personality Traits") +
  ylab("Mean Value (Scale 0 to 1)") +
  xlab("") 

#### Gender difference Mean value Bar plot example Slide 42 ----
personality.gen.dta$Diff <- personality.gen.dta$Men - personality.gen.dta$Women

ggplot(data=personality.gen.dta, 
       aes(x = reorder(Trait, Diff), y = Diff)) +
  geom_bar(stat="identity", fill = "darkseagreen")+
  geom_text(aes(label = percent(round(Diff, 3))), size = 4) +
  coord_flip()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Percent difference between men and women
          in average value of 16 personality traits") +
  ylab("Mean value among men - mean value among women") +
  xlab("") 
