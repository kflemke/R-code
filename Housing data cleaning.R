#### Change in population Balt ----
popc <- get_acs(
  geography = "tract",
  variables = c("B01003_001"),
  state = "MD",
  year = 2019,
  county = "Baltimore City",
  geometry = TRUE
)

popc$year <- NA
popc$year <- "2019"

pop1c <- get_acs(
  geography = "tract",
  variables = c("B01003_001"),
  state = "MD",
  year = 2015,
  county = "Baltimore City",
  geometry = TRUE
)

pop1c$year <- NA
pop1c$year <- "2015"

library(sf)
pop2c <- st_join(popc, pop1c, largest = T)

pop2c$diff <- NA
#2019 - 2015 pop
pop2c$diff <- pop2c$estimate.x - pop2c$estimate.y

pop.change.balt <- pop2c

#### Change in population Philly -----
popc <- get_acs(
  geography = "tract",
  variables = c("B01003_001"),
  state = "PA",
  year = 2019,
  county = "Philadelphia",
  geometry = TRUE
)

popc$year <- NA
popc$year <- "2019"

pop1c <- get_acs(
  geography = "tract",
  variables = c("B01003_001"),
  state = "PA",
  year = 2015,
  county = "Philadelphia",
  geometry = TRUE
)

pop1c$year <- NA
pop1c$year <- "2015"

library(sf)
pop2c <- st_join(popc, pop1c, largest = T)

pop2c$diff <- NA
#2019 - 2015 pop
pop2c$diff <- pop2c$estimate.x - pop2c$estimate.y

pop.change.phil <- pop2c

#### Race Balt ----
pop2 <- get_acs(
  geography = "tract",
  variables = c("B02001_001","B02001_002"),
  state = "MD",
  county = "Baltimore City",
  geometry = TRUE
)
pop2 <- pop2[,-5]
pop <- spread(pop2, variable, estimate)

pop$whiteprop <- NA
pop$whiteprop <- pop$B02001_002/pop$B02001_001

pop$pocprop <- NA
pop$pocprop <- 1 - pop$whiteprop

pop <- pop[!is.na(pop$pocprop),]
popT <- left_join(pop, balt1, by = "GEOID")

balt.race <- popT
#### Race Phil ----
pop2 <- get_acs(
  geography = "tract",
  variables = c("B02001_001","B02001_002"),
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE
)
pop2 <- pop2[,-5]
pop <- spread(pop2, variable, estimate)

pop$whiteprop <- NA
pop$whiteprop <- pop$B02001_002/pop$B02001_001

pop$pocprop <- NA
pop$pocprop <- 1 - pop$whiteprop

pop <- pop[!is.na(pop$pocprop),]
popT <- left_join(pop, phil1, by = "GEOID")

phil.race <- popT
#### Home val Balt ----
popx <- get_acs(
  geography = "tract",
  variables = "B25075_001",
  state = "MD",
  county = "Baltimore City",
  geometry = TRUE
)

balt.home <- popx[popx$estimate != 0,] 

#### Home val Phil ----
popx <- get_acs(
  geography = "tract",
  variables = "B25075_001",
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE
)

phil.home <- popx[popx$estimate != 0,] 
#### Baltimore Vacancy ####
pop2b <- get_decennial(
  geography = "tract",
  variables = c("H003003","H003001"),
  state = "MD",
  county = "Baltimore City",
  geometry = TRUE
)

popb <- spread(pop2b, variable, value)

popb$vacpropb <- NA
popb$vacpropb <- popb$H003003/popb$H003001

balt <- popb[!is.na(popb$vacpropb),]
#### Philly Vacancy ####

pop2a <- get_decennial(
  geography = "tract",
  variables = c("H003003","H003001"),
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE
)

popa <- spread(pop2a, variable, value)

popa$vacpropa <- NA
popa$vacpropa <- popa$H003003/popa$H003001
popa$vacpropa[350] <- .5

phil.vac <- popa[!is.na(popa$vacpropa),]

# merge ----
balt1 <- st_join(pop.change.balt, balt.race, largest = T)
balt1$GEOID.y <- NULL
balt1$NAME.y <- NULL
balt1$GEOID.x <- NULL
balt1$NAME.x <- NULL
balt1[,c(1:8)] <- NULL
balt1[,4:5] <- NULL

balt2 <- st_join(balt1, balt.home, largest = T)
balt2$home.val <- balt2$estimate
balt2$GEOID.y <- NULL
balt2$NAME.y <- NULL
balt2$variable <- NULL
balt2[, c(9,10)] <- NULL

balt.dta <- st_join(balt2, balt.vac, largest = T)
balt.dta$GEOID.x <- NULL
balt.dta$NAME.x <- NULL
balt.dta[, c(9:10)] <- NULL

phil1 <- st_join(pop.change.phil, phil.race, largest = T)
phil1$GEOID.y <- NULL
phil1$NAME.y <- NULL
phil1$GEOID.x <- NULL
phil1$NAME.x <- NULL
phil1[,c(1:8)] <- NULL
phil1[,4:5] <- NULL

phil2 <- st_join(phil1, phil.home, largest = T)
phil2$home.val <- phil2$estimate
phil2$estimate <- NULL
phil2$GEOID.y <- NULL
phil2$NAME.y <- NULL
phil2$variable <- NULL
phil2$moe <- NULL

phil.dta <- st_join(phil2, phil.vac, largest = T)
phil.dta$GEOID.x <- NULL
phil.dta$NAME.x <- NULL
phil.dta[, c(9:10)] <- NULL

# regressions ----
balt.dta$vacperc <- balt.dta$vacpropb*100
balt.dta$pocperc <- balt.dta$pocprop*100

phil.dta$vacperc <- phil.dta$vacpropa*100
phil.dta$pocperc <- phil.dta$pocprop*100

summary(lm(balt.dta$vacperc ~ balt.dta$pocperc))
summary(lm(phil.dta$vacperc ~ phil.dta$pocperc))
