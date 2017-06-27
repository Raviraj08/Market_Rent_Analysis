FMR_New <- read.csv('FairMarketRent.csv')
dim(FMR_New)

FMR=FMR_New[c(-12,-13,-14,-15)]
View(FMR)


FMR_New <- FMR

FMR_New$State.Code <- as.character(FMR_New$State.Code)
View(FMR_New)
typeof(FMR_New$State.Code)

FMR_New$State.Code[FMR_New$State.Code=="AL"] <- 0
FMR_New$State.Code[FMR_New$State.Code=="AK"] <- 1
FMR_New$State.Code[FMR_New$State.Code=="AZ"] <- 2
FMR_New$State.Code[FMR_New$State.Code=="AR"] <- 3
FMR_New$State.Code[FMR_New$State.Code=="CA"] <- 4
FMR_New$State.Code[FMR_New$State.Code=="CO"] <- 5
FMR_New$State.Code[FMR_New$State.Code=="CT"] <- 6
FMR_New$State.Code[FMR_New$State.Code=="DE"] <- 7
FMR_New$State.Code[FMR_New$State.Code=="DC"] <- 8
FMR_New$State.Code[FMR_New$State.Code=="FL"] <- 9
FMR_New$State.Code[FMR_New$State.Code=="GA"] <- 10
FMR_New$State.Code[FMR_New$State.Code=="HI"] <- 11
FMR_New$State.Code[FMR_New$State.Code=="ID"] <- 12
FMR_New$State.Code[FMR_New$State.Code=="IL"] <- 13
FMR_New$State.Code[FMR_New$State.Code=="IN"] <- 14
FMR_New$State.Code[FMR_New$State.Code=="IA"] <- 15
FMR_New$State.Code[FMR_New$State.Code=="KS"] <- 16
FMR_New$State.Code[FMR_New$State.Code=="KY"] <- 17
FMR_New$State.Code[FMR_New$State.Code=="LA"] <- 18
FMR_New$State.Code[FMR_New$State.Code=="ME"] <- 19
FMR_New$State.Code[FMR_New$State.Code=="MD"] <- 20
FMR_New$State.Code[FMR_New$State.Code=="MA"] <- 21
FMR_New$State.Code[FMR_New$State.Code=="MI"] <- 22
FMR_New$State.Code[FMR_New$State.Code=="MN"] <- 23
FMR_New$State.Code[FMR_New$State.Code=="MS"] <- 24
FMR_New$State.Code[FMR_New$State.Code=="MO"] <- 25
FMR_New$State.Code[FMR_New$State.Code=="MT"] <- 26
FMR_New$State.Code[FMR_New$State.Code=="NE"] <- 27
FMR_New$State.Code[FMR_New$State.Code=="NV"] <- 28
FMR_New$State.Code[FMR_New$State.Code=="NH"] <- 29
FMR_New$State.Code[FMR_New$State.Code=="NJ"] <- 30
FMR_New$State.Code[FMR_New$State.Code=="NM"] <- 31
FMR_New$State.Code[FMR_New$State.Code=="NY"] <- 32
FMR_New$State.Code[FMR_New$State.Code=="NC"] <- 33
FMR_New$State.Code[FMR_New$State.Code=="ND"] <- 34
FMR_New$State.Code[FMR_New$State.Code=="OH"] <- 35
FMR_New$State.Code[FMR_New$State.Code=="OK"] <- 36
FMR_New$State.Code[FMR_New$State.Code=="OR"] <- 37
FMR_New$State.Code[FMR_New$State.Code=="PA"] <- 38
FMR_New$State.Code[FMR_New$State.Code=="RI"] <- 39
FMR_New$State.Code[FMR_New$State.Code=="SC"] <- 40
FMR_New$State.Code[FMR_New$State.Code=="SD"] <- 41
FMR_New$State.Code[FMR_New$State.Code=="TN"] <- 42
FMR_New$State.Code[FMR_New$State.Code=="TX"] <- 43
FMR_New$State.Code[FMR_New$State.Code=="UT"] <- 44
FMR_New$State.Code[FMR_New$State.Code=="VT"] <- 45
FMR_New$State.Code[FMR_New$State.Code=="VA"] <- 46
FMR_New$State.Code[FMR_New$State.Code=="WA"] <- 47
FMR_New$State.Code[FMR_New$State.Code=="WV"] <- 48
FMR_New$State.Code[FMR_New$State.Code=="WI"] <- 49
FMR_New$State.Code[FMR_New$State.Code=="WY"] <- 50
FMR_New$State.Code[FMR_New$State.Code=="PR"] <- 51

View(FMR_New)

write.csv(FMR_New,file="D:/Group5_FairMarketRent/FairMarketRent_Updated.csv")

FMR_New<-read.csv('D:/Group5_FairMarketRent/FairMarketRent_Updated.csv')

fmr.scaled <- scale(data.frame(FMR_New$State.Code,FMR_New$FMR.Studio,FMR_New$FMR.1bb,FMR_New$FMR.2bb,FMR_New$FMR.3bb,FMR_New$FMR.4bb,FMR_New$Year,FMR_New$Civilian.labor.force,FMR_New$Employed,FMR_New$Unemployed))
colnames(fmr.scaled)
totwss <- vector()
btwss <- vector()
for(i in 2:15)
{
  set.seed(1234)
  temp <-kmeans(fmr.scaled,centers=i)
  totwss[i] <- temp$tot.withinss
  btwss[i] <- temp$betweenss
}


plot(totwss,xlab="Number of Cluster", type ="b",
     ylab="Total within sum of square")

plot(btwss,xlab="Number of Cluster", type ="b",
     ylab="Total Between sum of square")

install.Packages("Rserve")
library(Rserve)
Rserve()
