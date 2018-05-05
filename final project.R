library(tidyverse)
library(ggplot2)
library(lubridate)
library(Quandl)
library(dplyr)
library(ggmap)
library(devtools)

## roadmap to show Michigan
map <- get_googlemap(center = c(-84.630239, 43.657635), zoom = 7)
ggmap(map)

## marked road map, Flint, Grand Rapids, Detroit, Ann Arbor, Kalamazoo and Lansing
## These are the cities I will be focusing on 
ggmap(map) +
  geom_point(
    aes(x = -83.687456 , y = 43.012527),
    color = "red", size = 3) +
  geom_point(
    aes(x = -85.668086 , y = 42.963360),
    color = "green", size = 3) +
  geom_point(
    aes(x = -83.045754 , y = 42.331427),
    color = "blue", size = 3)+
  geom_point(
    aes(x = -83.743038, y = 42.280826),
    color = "yellow", size = 3)+
  geom_point(
    aes(x = -85.587229 , y = 42.291707),
    color = "orange", size = 3)+
  geom_point(
    aes(x = -84.555535, y = 42.732535),
    color = "purple", size = 3)

## Data from Quandl. It is annual average house prices in Michigan and the previous cities
mihomeprices <- read.csv(file="Michigan Data.csv", header=TRUE, sep=",")

## make data continuous
mihomeprices$Year <- as.numeric(as.character(mihomeprices$Year))
mihomeprices$Avg.Price <- as.numeric(as.character(mihomeprices$Avg.Price))

## plot data

## average MI Home prices from 1999 to 2016
ggplot(data = filter(mihomeprices, Place == "Michigan")) +
  geom_line(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Median Home Prices in Michigan")

## average Flint, MI Home prices from 1999 to 2016
ggplot(data = filter(mihomeprices, Place == "Flint")) +
  geom_line(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016") +
  ylab("House Price") +
  ggtitle("Median Home Prices in Flint, MI")

## average Flint, MI Home prices from 1999 to 2016
ggplot(data = filter(mihomeprices, Place == "Flint")) +
  geom_point(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016") +
  ylab("House Price") +
  ggtitle("Median Home Prices in Flint, MI")

## average Grand Rapids, MI Home prices from 1999 to 2016
ggplot(data = filter(mihomeprices, Place == "Grand Rapids")) +
  geom_line(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Median Home Price in Grand Rapids, MI")

## average Detroit, MI Home prices from 1999 to 2016
ggplot(data = filter(mihomeprices, Place == "Detroit")) +
  geom_line(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Median Home Prices in Detroit, MI")

## average Ann Arbor, MI Home prices from 1999 to 2016
ggplot(data = filter(mihomeprices, Place == "Ann Arbor")) +
  geom_line(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Median Home Prices in Ann Arbor, MI")

## average Kalamazoo, MI Home prices from 1999 to 2016
ggplot(data = filter(mihomeprices, Place == "Kalamazoo")) +
  geom_line(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Median Home Prices in Kalamazoo, MI")

## average Lansing, MI Home prices from 1999 to 2016
ggplot(data = filter(mihomeprices, Place == "Lansing")) +
  geom_line(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Median Home Prices in Lansing, MI")

## making new data tables
flintprices <- filter(mihomeprices, Place == "Flint")
miprices <- filter(mihomeprices, Place == "Michigan")
fmprices <- rbind(flintprices, miprices)
kzooprices <- filter(mihomeprices, Place == "Kalamazoo")
kmprices <- rbind(kzooprices, miprices)
detroitprices <- filter(mihomeprices, Place == "Detroit")
dmprices <- rbind(detroitprices, miprices)
lansingprices <- filter(mihomeprices, Place == "Lansing")
lmprices <- rbind(lansingprices, miprices)
annarborprices <- filter(mihomeprices, Place == "Ann Arbor")
amprices <- rbind(annarborprices, miprices)

## graph comparing state of Michigan with Flint
ggplot(fmprices) +
         geom_point(mapping = aes(x = Year, y = Avg.Price)) +
         xlab("Year (1999 - 2016)") +
         ylab("House Price") +
         ggtitle("Michigan vs Flint Home Prices") 

## graph comparing state of Michigan with Detroit
ggplot(dmprices) +
  geom_point(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Michigan vs Detroit Home Prices")

## graph comparing state of Michigan with Ann Arbor
ggplot(amprices) +
  geom_point(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Michigan vs Ann Arbor Home Prices")

## graph comparing state of Michigan with Kalamazoo
ggplot(kmprices) +
  geom_point(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Michigan vs Kalamazoo Home Prices")

## graph comparing state of Michigan with Lasning
ggplot(lmprices) +
  geom_point(mapping = aes(x = Year, y = Avg.Price)) +
  xlab("Year (1999 - 2016)") +
  ylab("House Price") +
  ggtitle("Michigan vs Lansing Home Prices")

## my variables for the t tests are 'city'_b for before the crisis and 
## 'city'_a for after the crisis

## t tests before and after water crisis in flint
## in michigan
michb <- mihomeprices %>% filter(Place_ba == "Michigan_b") %>% select(Avg.Price)
micha <- mihomeprices %>% filter(Place_ba == "Michigan_a") %>% select(Avg.Price)
michb$Avg.Price <- as.numeric(michb$Avg.Price)
micha$Avg.Price <- as.numeric(micha$Avg.Price)
t.test(michb$Avg.Price, micha$Avg.Price)

## in flint
flintb <- mihomeprices %>% filter(Place_ba == "Flint_b") %>% select(Avg.Price)
flinta <- mihomeprices %>% filter(Place_ba == "Flint_a") %>% select(Avg.Price)
flintb$Avg.Price <- as.numeric(flintb$Avg.Price)
flinta$Avg.Price <- as.numeric(flinta$Avg.Price)
t.test(flintb$Avg.Price, flinta$Avg.Price)

## in detroit
detroitb <- mihomeprices %>% filter(Place_ba == "Detroit_b") %>% select(Avg.Price)
detroita <- mihomeprices %>% filter(Place_ba == "Detroit_a") %>% select(Avg.Price)
detroitb$Avg.Price <- as.numeric(detroitb$Avg.Price)
detroita$Avg.Price <- as.numeric(detroita$Avg.Price)
t.test(detroitb$Avg.Price, detroita$Avg.Price)

## in Ann Arbor
aab <- mihomeprices %>% filter(Place_ba == "Ann Arbor_b") %>% select(Avg.Price)
aaa <- mihomeprices %>% filter(Place_ba == "Ann Arbor_a") %>% select(Avg.Price)
aab$Avg.Price <- as.numeric(aab$Avg.Price)
aaa$Avg.Price <- as.numeric(aaa$Avg.Price)
t.test(aab$Avg.Price, aaa$Avg.Price)

## in Kalamazoo
kzoob <- mihomeprices %>% filter(Place_ba == "Kalamazoo_b") %>% select(Avg.Price)
kzooa <- mihomeprices %>% filter(Place_ba == "Kalamazoo_a") %>% select(Avg.Price)
kzoob$Avg.Price <- as.numeric(kzoob$Avg.Price)
kzooa$Avg.Price <- as.numeric(kzooa$Avg.Price)
t.test(kzoob$Avg.Price, kzooa$Avg.Price)

## in lansing
lansingb <- mihomeprices %>% filter(Place_ba == "Lansing_b") %>% select(Avg.Price)
lansinga <- mihomeprices %>% filter(Place_ba == "Lansing_a") %>% select(Avg.Price)
lansingb$Avg.Price <- as.numeric(lansingb$Avg.Price)
lansinga$Avg.Price <- as.numeric(lansinga$Avg.Price)
t.test(lansingb$Avg.Price, lansinga$Avg.Price)

