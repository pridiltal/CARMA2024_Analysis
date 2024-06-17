# CARMA 2024 conference Analysis
### Supplemental Materials (Data and scripts)
###  DFrom Crisis to Opportunity: A Google Trends
### Analysis of Global Interest in Distance Education
### Tools During and Post the COVID-19 Pandemic
### Updated the analysis on 17/5/2024 for CARMA conference

## ---- load
library(tidyverse)
library (gtrendsR)
library(RColorBrewer)
library(ggpubr)
library(viridis)
library(fpp3)
library(countrycode)
library(coronavirus)
library(patchwork)
library(coronavirus)
library(dtw)
#set the time window for the analysis
time <- ("2019-12-01 2024-04-30")


## ---- covidImpactWorld
## COVID-19 Impact on Education (global data)
## Data source: https://en.unesco.org/covid19/educationresponse
## Data download: 16-08-2021
world_impact <- read.csv(
  here::here("data", "covid_impact_education_full.csv"))
wimpact <- world_impact |>
  mutate(Date = as.Date(Date, "%d/%m/%Y"),
         Status = as.factor(Status)) 

wimpact <- wimpact |> select(-ISO) |>
  group_by(Date, Status) |>
  summarize(Count = n())|>
  as_tsibble(index = Date, key = Status) 

p <- wimpact |> autoplot(Count) +
  geom_line(size=1) +
  xlab ("Time") +
  scale_color_brewer(palette="Dark2")+
  ylab("Number of Countries")+
  theme_bw() + 
  theme( text = element_text(size = 14), legend.position = "bottom")+
  guides(colour = guide_legend(nrow = 2, title="Status"))

print(p)



## ---- covidImpactContinent
## COVID-19 Impact on Education - continent wise 
## Data source: https://en.unesco.org/covid19/educationresponse
## Data download: 16-08-2021
world_impact <- read.csv(
  here::here("data", "covid_impact_education_full.csv"))
wimpact <- world_impact |>
  mutate(Date = as.Date(Date, "%d/%m/%Y"),
         Status = as.factor(Status)) 

wimpact$Continent <- countrycode(sourcevar = wimpact[, "ISO"],
                                 origin = "iso3c",
                                 destination = "continent")

wimpact <- wimpact |> select(-ISO) |>
  group_by(Date, Status, Continent) |>
  summarize(Count = n())

wimpact1<- wimpact|>pivot_wider(names_from = Continent, values_from = Count )

wimpact1$Asia <- (wimpact1$Asia - min(wimpact1$Asia, na.rm = TRUE))/(max(wimpact1$Asia, na.rm = TRUE) - min(wimpact1$Asia, na.rm = TRUE))

wimpact1$Africa <- (wimpact1$Africa - min(wimpact1$Africa, na.rm = TRUE))/(max(wimpact1$Africa, na.rm = TRUE) - min(wimpact1$Africa, na.rm = TRUE))

wimpact1$Africa <- (wimpact1$Americas - min(wimpact1$Americas, na.rm = TRUE))/(max(wimpact1$Americas, na.rm = TRUE) - min(wimpact1$Americas, na.rm = TRUE))

wimpact1$Europe <- (wimpact1$Europe - min(wimpact1$Europe, na.rm = TRUE))/(max(wimpact1$Europe, na.rm = TRUE) - min(wimpact1$Europe, na.rm = TRUE))

wimpact1$Oceania <- (wimpact1$Oceania - min(wimpact1$Oceania, na.rm = TRUE))/(max(wimpact1$Oceania, na.rm = TRUE) - min(wimpact1$Oceania, na.rm = TRUE))


wimpact2<- wimpact1 |> pivot_longer(
  cols = Asia:Oceania,
  names_to = c("Continent"),
  values_to = "count"
)

p <- wimpact |>
  ggplot(aes(x = Date, y = Count, color = Status)) +
  geom_line(size = 1) +
  facet_grid(vars(Continent), scales = "free_y") +   # only assigns the row variable
  xlab("Time)") +
  scale_color_brewer(palette="Dark2")+
  theme_bw() + 
  theme( text = element_text(size = 14), legend.position = "bottom")+
  guides(colour = guide_legend(nrow = 2))+
  ylab("Number of Countries")

print(p)



## ---- distance_learning_world
## Distance Learning related terms online search
# data retrieval
keywords= c("Online Learning", "Online teaching",
            "Distance learning","Distance education",
            "online proctoring")
category <- categories[
  categories$name=="Education",2] |> 
  as.numeric()
#set channels 
channel <- 'web'
trends <-  gtrends(keywords,
                   gprop =channel,
                   time = "2017-12-01 2024-04-30" )
trends <- trends$interest_over_time
write.csv(trends, here::here(
  "data", "education_search_world.csv"))



## ---- covid19_trend_world
##  covid 19 related search term data retrieval
keywords= c("covid 19", "corona", "covid")
#set channels 
channel <- 'web'
trends <-  gtrends(keywords, gprop =channel,
                   time = "2017-12-01 2024-04-30" )
trends <- trends$interest_over_time
write.csv(trends, here::here( "data",
                              "covid_search_world.csv"))


## ---- covid_epidemic_datasave
# Data retrieved from coronavirus R 
# package on 17-05-2024
data("coronavirus")

covid_cases <- coronavirus |> 
  group_by(type, date) |>
  summarise(total_cases = sum(cases)) |>
  pivot_wider(names_from = type, 
              values_from = total_cases) |>
  arrange(date) 

covid_cases <- covid_cases |> as_tsibble(index = date)
write.csv(covid_cases, here::here(
  "data", "covid_cases.csv"))


## ---- distanceLearningWorldAnalysis
# covid actual cases vs internet search for covid related 
# terms and the distance education related terms
covidcasesW <- read.csv(here::here( "data",
                                    "covid_cases.csv"))
covidcasesW <- covidcasesW |> select(-X)

covidcasesW1 <- covidcasesW |> pivot_longer(
  cols = confirmed : recovery,
  names_to = "Type",
  values_to = "Counts") |>
  mutate(Type = factor(Type, levels = c("confirmed", "death", "recovery")))|>
  mutate(date = as.Date(date)) |>
  as_tsibble(index = date, key = Type) |>
  mutate(Counts = ifelse(Counts <0, NA, Counts))


p1 <- covidcasesW1 |>  autoplot() +
  xlab("date") + 
  scale_color_brewer(palette="Dark2") +
  scale_x_date(limits = as.Date(c('2017-12-03','2024-04-30'))) +
  theme_bw()+
  theme(legend.position = "bottom", text = element_text(size = 22)) +
  ggtitle("(a) COVID-19 cases worldwide ")+
  xlab("")



## Covid search
data_covid <- read.csv( here::here( "data",
                                    "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)

keyword_ord<- data_covid |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_covid |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")

p2<- dataW |>  ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) + 
  scale_color_brewer(palette="Dark2")  +
  scale_x_date(limits = as.Date(c('2017-12-03','2024-04-30'))) +
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 22)) +
  ggtitle("(b) Google search trends of COVID-19 related terms")+
  xlab("")


## covid google trends
data_world <- read.csv( here::here( "data", 
                                    "education_search_world.csv"))

# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)

keyword_ord<- data_world |> 
  group_by(keyword) |>
  summarise(total_hits = sum(hits, na.rm = TRUE)) |>
  arrange(total_hits) |> select(keyword) |> as_vector()

dataW <- data_world |>
  select(date, keyword, hits) |>
  mutate(keyword = factor(keyword, levels = keyword_ord))
dataW$date = as.Date(dataW$date, "%Y-%m-%d")


p3<- dataW|> ggplot(aes(x=date, y=hits, colour = keyword))+
  geom_line(size=1) +
  scale_color_brewer(palette="Dark2")  +
  scale_x_date(limits = as.Date(c('2017-12-03','2024-04-30'))) +
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 22))+
  guides(colour = guide_legend(nrow = 2, title="keywords"))+
  ggtitle("(c) Google search trends of distance education-related terms")
xlab("Time")


p <- p1/p2/p3

print(p)




## ---- dtw
# Dynamic Time Warping Alignments
# Covid cases
covidcasesW <- read.csv(here::here( "data", "covid_cases.csv"))
covidcasesW <- covidcasesW |> select(-X)

## covid search google trends
data_covid <- read.csv( here::here(
  "data", "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)
# covid search google trends - arrange in wide format
covid_search <- data_covid |>
  select(date, hits, keyword) |>
  pivot_wider(names_from = keyword, values_from = hits)

## online learning google trends
data_world <- read.csv( here::here(
  "data", "education_search_world.csv"))
# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)
# arrange in wide format
edu_search <- data_world |>
  select(date, hits, keyword) |>
  pivot_wider(names_from = keyword, values_from = hits)

## Time Lagged Cross Correlation 
library(fpp3)
data <- full_join(covid_search, 
                  edu_search,  by = "date" ) |>
  mutate(date = as.Date(date)) |>
  as_tsibble(index = date)


x<- scale(data$`Online Learning`)
y<- scale(data$corona)
z<- scale(data$covid)

xyz <- cbind(x,y,z)
xyz <- na.omit(xyz)

x <- zoo(x, data$date)
y <- zoo(y, data$date)
z <- zoo(z, data$date)


# #align1 <- dtw(xyz[,1], xyz[,1]*0, keep = T)
# align1 <- dtw(xyz[,1], xyz[,2], keep = T)
# align2 <- dtw(xyz[,1], xyz[,3], keep = T)

align1 <- dtw(x, y, keep = T)
align2 <- dtw(x, z, keep = T)

## ---- dtw1
# par(mfrow=c(2,2), mar=c(0,1,0.75,0))
p1 <- dtwPlotTwoWay(align1,
                    main = "(a) Pointwise comparison between 'Online Learning' and 'corona'")
p2 <- dtwPlotDensity(align1, normalize = TRUE, main = "(b) Cumulative cost density 
                     with the warping path between 'Online Learning' and 'corona' ")

## ---- dtw2
p3 <- dtwPlotTwoWay(align2, main = "(a) Pointwise comparison between 'Online Learning' and 'covid'")
p4 <-dtwPlotDensity(align2, normalize = TRUE, main = "(b) Cumulative cost density 
                    with the warping path between 'Online Learning' and 'covid'")


## ---- ccfAnalysis
# Dynamic Time Warping Alignments
# Covid cases
covidcasesW <- read.csv(here::here("data", "covid_cases.csv"))
covidcasesW <- covidcasesW |> select(-X)

## covid search google trends
data_covid <- read.csv( here::here( "data", "covid_search_world.csv"))
hits1<- ifelse(data_covid$hits ==  "<1", 0, data_covid$hits)
data_covid$hits <- as.numeric(hits1)
# covid search google trends - arrange in wide format
covid_search <- data_covid |>
  select(date, hits, keyword) |>
  pivot_wider(names_from = keyword, values_from = hits)

## online learning google trends
data_world <- read.csv( here::here( "data", "education_search_world.csv"))
# hits is a character variable. Convert hits into a numeric variable
hits1<- ifelse(data_world$hits ==  "<1", 0, data_world$hits)
data_world$hits <- as.numeric(hits1)
# arrange in wide format
edu_search <- data_world |>
  select(date, hits, keyword) |>
  pivot_wider(names_from = keyword, values_from = hits)

## Time Lagged Cross Correlation 
library(fpp3)
data <- full_join(covid_search, edu_search,  by = "date" ) |>
  mutate(date = as.Date(date)) |>
  as_tsibble(index = date, regular = FALSE)


p1 <- data |> 
  mutate(diff_corona = difference(corona),
         diff_online = difference(`Online Learning`)) |>
  CCF(diff_corona, diff_online ) |>
  autoplot()+
  labs(title= "(a) 'Corona' with 'Online learning'")

p2 <- data |> 
  mutate(diff_corona = difference(corona),
         diff_dist = difference(`Distance learning`)) |>
  CCF(diff_corona, diff_dist ) |>
  autoplot()+
  labs(title= "(b) 'Corona' with 'Distance learning'")

p3 <- data |> 
  mutate(diff_corona = difference(corona),
         diff_teach = difference(`Online teaching`)) |>
  CCF(diff_corona, diff_teach ) |>
  autoplot()+
  labs(title= "(c) 'Corona' with 'Online teaching'")

p4 <- data |> 
  mutate(diff_corona = difference(corona),
         diff_edu = difference(`Distance education`)) |>
  CCF(diff_corona, diff_edu ) |>
  autoplot()+
  labs(title= "(d) 'Corona' with 'Distance education'")

p5 <- data |> 
  mutate(diff_corona = difference(corona),
         diff_proc = difference(`online proctoring`)) |>
  CCF(diff_corona, diff_proc ) |>
  autoplot()+
  labs(title= "(e) 'Corona' with 'Online proctoring'")



pa <- data |> 
  mutate(diff_corona = difference(covid),
         diff_online = difference(`Online Learning`)) |>
  CCF(diff_corona, diff_online ) |>
  autoplot()+
  labs(title= "(f) 'Covid' with 'Online learning")

pb <- data |> 
  mutate(diff_corona = difference(covid),
         diff_dist = difference(`Distance learning`)) |>
  CCF(diff_corona, diff_dist ) |>
  autoplot()+
  labs(title= "(g) 'Covid' with 'Distance learning'")

pc <- data |> 
  mutate(diff_corona = difference(covid),
         diff_teach = difference(`Online teaching`)) |>
  CCF(diff_corona, diff_teach ) |>
  autoplot()+
  labs(title= "(h) 'Covid' with 'Online teaching'")

pd <- data |> 
  mutate(diff_corona = difference(covid),
         diff_edu = difference(`Distance education`)) |>
  CCF(diff_corona, diff_edu ) |>
  autoplot()+
  labs(title= "(i) 'Covid' with 'Distance education'")

pe <- data |> 
  mutate(diff_corona = difference(covid),
         diff_proc = difference(`online proctoring`)) |>
  CCF(diff_corona, diff_proc ) |>
  autoplot()+
  labs(title= "(j) 'Covid' with 'Online proctoring'")


p <- (p1 | pa ) / 
  (p2 | pb )/
  (p3 | pc )/
  (p4 | pd )/
  (p5 | pe )

