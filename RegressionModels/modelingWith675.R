install.packages("Hmisc", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("rgl", dependencies = TRUE)
library(Hmisc)
library(psych)
library(car)
library(ggplot2)
library(rgl)

regressionData <- read.csv("RegressionData_Master675.csv")
regressionData2 <- regressionData[-c(5,10,15,20,25,35,45,50,55,65,70,80,90,95,100,110,115,
                                     125,140,145,155,170,184,185,190,199,200,214,215,230,
                                     235,245,250,260,270,275,280,290,295,305,320,325,335,
                                     350,365,395,409,410,424,439,440,455,460,470,475,485,
                                     500,505,515,545,560,590,605,635,650,664),] #remove <5000
regressionData3 <- regressionData[-c(5,10,15,20,25,30,35,45,50,55,60,65,70,75,80,90,95,100,
                                     105,110,115,120,125,135,140,145,150,155,160,165,170,
                                     180,184,185,190,194,195,199,200,205,209,210,214,215,
                                     219,224,225,230,235,240,245,250,255,260,270,275,280,
                                     285,290,295,300,305,315,320,325,330,335,340,345,350,
                                     360,365,370,375,380,385,390,395,405,409,410,415,419,
                                     420,424,425,430,434,435,439,440,449,450,455,460,465,
                                     470,475,480,485,490,495,500,505,510,515,520,525,530,
                                     535,540,545,550,555,560,565,570,575,580,585,590,595,
                                     600,605,610,615,620,625,630,634,635,640,644,645,649,
                                     650,655,659,660,664,665,669,670,674,675),] #remove <12000
regressionData4 <- regressionData[-c(5,10,15,20,25,30,35,45,50,55,60,65,70,75,80,90,95,
                                     100,105,110,115,120,125,135,140,145,150,155,160,165,
                                     170,180,184,185,190,194,195,199,200,205,209,210,214,
                                     215,219,224,225,230,235,240,245,250,255,260,270,275,
                                     280,285,290,295,300,305,315,320,325,330,335,340,345,
                                     350,360,365,370,375,380,385,390,395,405,409,410,415,
                                     419,420,424,425,430,434,435,439,440,449,450,455,460,
                                     465,470,475,480,485,490,495,500,505,510,515,520,525,
                                     530,535,540,545,550,555,560,565,570,575,580,585,590,
                                     595,600,605,610,615,620,625,630,634,635,640,644,645,
                                     649,650,655,659,660,664,665,669,670,674,675),] #no changes lol

lm.Earnings <- lm(Earnings ~ CirculatingNurse_Count*PreOpNurse_Count*MaintenanceGroup_Count*
                    ScrubNurse_Count*PACUNurse_Count, data=regressionData)
summary(lm.Earnings)
plot(lm.Earnings)
coefficients(lm.Earnings)

lm.logEarnings <- lm(log1p(Earnings) ~ log1p(CirculatingNurse_Count)*log1p(PreOpNurse_Count)*log1p(MaintenanceGroup_Count)
                     *log1p(ScrubNurse_Count)*log1p(PACUNurse_Count), data=regressionData3)
plot(lm.logEarnings)

auto.explore <- subset(regressionData, select=c(Earnings, CirculatingNurse_Count,
                                                PreOpNurse_Count, MaintenanceGroup_Count,
                                                ScrubNurse_Count, PACUNurse_Count))
pairs.panels(auto.explore, col="red")
#Correlation plot. Shows relationship of earnings, and the support staff count 
auto.explore2 <- subset(regressionData2, select=c(Earnings, CirculatingNurse_Count,
                                                PreOpNurse_Count, MaintenanceGroup_Count,
                                                ScrubNurse_Count, PACUNurse_Count))
pairs.panels(auto.explore2, col="red")
#----------------------
auto.explore3 <- subset(regressionData3, select=c(Earnings, CirculatingNurse_Count,
                                                  PreOpNurse_Count, MaintenanceGroup_Count,
                                                  ScrubNurse_Count, PACUNurse_Count))
pairs.panels(auto.explore3, col="red")

lm.Earnings3 <- lm(Earnings ~ CirculatingNurse_Count*PreOpNurse_Count*MaintenanceGroup_Count*
                    ScrubNurse_Count*PACUNurse_Count, data=regressionData3)
coefficients(lm.Earnings3)
plot(lm.Earnings3)


coplot(Earnings ~ CirculatingNurse_Count + PreOpNurse_Count + ScrubNurse_Count + PACUNurse_Count | Earnings, 
       data=regressionData, col="orange")
coplot(Earnings ~ CirculatingNurse_Count * PreOpNurse_Count * ScrubNurse_Count * PACUNurse_Count | Earnings, 
       data=regressionData3, col="orange")
coplot(Earnings ~ CirculatingNurse_Count * PreOpNurse_Count * MaintenanceGroup_Count 
       * ScrubNurse_Count * PACUNurse_Count 
       | Earnings, data=regressionData3, col="orange")
coplot(Earnings ~ CirculatingNurse_Count * MaintenanceGroup_Count * ScrubNurse_Count 
       | Earnings, data=regressionData3, col="orange")
coplot(Overtime ~ CirculatingNurse_Count * MaintenanceGroup_Count * ScrubNurse_Count 
       | Earnings, data=regressionData3, col="orange")
coplot(CirculatingNurse_Count  ~  ScrubNurse_Count * MaintenanceGroup_Count
       | Earnings * Overtime, data=regressionData3, col="orange") #cool but not productive
#These six plots are guided by the value of Earnings. Which is the conditioning variable for this probplot.
#The system identified different ranges of Earnings which are worth grouping, and plotting as seperate charts. 
#Bottom left chart is explained by the bottom left range. And moves upwards. 

scatter3d(x=auto$Curb.weight, z=auto$Peak.rpm, y=auto$Price)
scatter3d(x=auto$Horsepower, z=auto$City.mpg, y=auto$Price)
scatter3d(x=auto$Horsepower, z=auto$Curb.weight, y=auto$Normalized.losses)

### Compare the effect of data cleanup on the model, use only training data

# Raw data but without missing values
scatter3d(x=auto[train.index,]$Curb.weight, 
          z=auto[train.index,]$Peak.rpm, 
          y=auto[train.index,]$Price)

# Data with transformed variables
scatter3d(x=auto.sel[train.index,]$Curb.weight, 
          z=auto.sel[train.index,]$Peak.rpm, 
          y=auto.sel[train.index,]$Price)

# Data without extreme values
scatter3d(x=regressionData$Earnings, 
          z=regressionData$CirculatingNurse_Count, 
          y=regressionData$ScrubNurse_Count)

summary(lm.Earnings3)
