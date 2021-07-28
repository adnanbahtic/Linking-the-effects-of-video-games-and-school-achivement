
rm(list=ls())

setwd("C:\\Users\\mladi\\Desktop\\Teza\\POKUSAJ")


library(tidyverse)
library(party)
library(fpc)
library(ggplot2)
library(gridExtra)
library(hrbrthemes)
library(dplyr)
library(cowplot)
library(ggpubr)
library(VIM)
library(plyr)
library(psych)
library(corrplot)
library(corrgram)
library(rlang)
library(randomForest)

data = read.csv("data.csv")


#2.3 Renaming data

data$Spol...F.≈.ensko...M.Mu≈.ko.
data$Koliko.ima≈..godina..
data$Koji.si.razred..
data$Sa.kojom.zakljuƒ.nom.ocjenom.si.zavr≈.io.la.predhodni.razred....od.1.00.do.5.00.
data$Zakljuƒ.na.ocjena.u.ovom.polugodi≈.tu...od.1.00.do.5.00.
data$Da.li.igra≈..video.igrice..
data$Koliko.dana.u.sedmici.igra≈..video.igrice..
data$Koliko.sati.dnevno.provodi≈..u.igranju.video.igrica.....od.0.do.24.
data$Na.skali.od..1.do.10.koliko.voli≈..da.igra≈..video.igrice..
data$Da.li.bi.≈.elio.la.da.vi≈.e.vremena.provodi≈..igrajuƒ.i.video.igrice..
data$Tvoje.mi≈.ljenje...da.li.previ≈.e.vremena.provodi≈..igrajuƒ.i.video.igrice.
data$Tvoje.mi≈.ljenje...da.li.video.igrice.utiƒ.u.na.tvoj.uspijeh.u.≈.koli.
data$Da.li.si.ikada.bio.la.ka≈.njen.a.od.strane.roditelja.zbog.vremena.koje.tro≈.i≈..na.video.igrice.

names(data)[names(data) == 'Spol...F.≈.ensko...M.Mu≈.ko.'] <- 'Gender'
names(data)[names(data) == 'Koliko.ima≈..godina..'] <- 'Age'
names(data)[names(data) == 'Koji.si.razred..'] <- 'Grade'
names(data)[names(data) == 'Sa.kojom.zakljuƒ.nom.ocjenom.si.zavr≈.io.la.predhodni.razred....od.1.00.do.5.00.'] <- 'GLS'
names(data)[names(data) == 'Zakljuƒ.na.ocjena.u.ovom.polugodi≈.tu...od.1.00.do.5.00.'] <- 'GDS'
names(data)[names(data) == 'Da.li.igra≈..video.igrice..'] <- 'PVG'
names(data)[names(data) == 'Koliko.dana.u.sedmici.igra≈..video.igrice..'] <- 'DIW'
names(data)[names(data) == 'Koliko.sati.dnevno.provodi≈..u.igranju.video.igrica.....od.0.do.24.'] <- 'HPD'
names(data)[names(data) == 'Na.skali.od..1.do.10.koliko.voli≈..da.igra≈..video.igrice..'] <- 'Scale'
names(data)[names(data) == 'Da.li.bi.≈.elio.la.da.vi≈.e.vremena.provodi≈..igrajuƒ.i.video.igrice..'] <- 'PT'
names(data)[names(data) == 'Tvoje.mi≈.ljenje...da.li.previ≈.e.vremena.provodi≈..igrajuƒ.i.video.igrice.'] <- 'OPN_TPG'
names(data)[names(data) == 'Tvoje.mi≈.ljenje...da.li.video.igrice.utiƒ.u.na.tvoj.uspijeh.u.≈.koli.'] <- 'OPN_AYG'
names(data)[names(data) == 'Da.li.si.ikada.bio.la.ka≈.njen.a.od.strane.roditelja.zbog.vremena.koje.tro≈.i≈..na.video.igrice.']
<- 'PP'


#2.4 Data and 2.5 missing values

table(is.na(data))
#we have 16 missing values 

sapply(data,function(x) sum(is.na(x)))# Number of NA values with respect to each feature



#The missing fata graph shows the distribution of missing values in each variable
aggr(data, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, 
     labels=names(data), gap=2, cex.axis=1,cex.numbers=0.5,
     ylab=c("Histogram of missing data","Pattern"))

#Removing children with missing values = Age
data<-data[!is.na(data$Age),]

#Removing children with missing values = Grade
data<-data[!is.na(data$Grade),]


#Removing children with missing values = GLS
data<-data[!is.na(data$GLS),]

#Removing children with missing values = GDS
data<-data[!is.na(data$GDS),]

#Removing children with missing values = DIW
data<-data[!is.na(data$DIW),]

#Removing children with missing values = HPD
data<-data[!is.na(data$HPD),]



#Data exploration 

paletteEcoFin <- c("#D10007", "#198EF5")

ggplot(data, aes(x="", y="", fill=Gender)) + 
  geom_bar(stat="identity", width=1 , color="white") +
  coord_polar("y", start=0) + theme_void()+
  scale_fill_manual(values=paletteEcoFin) + ggtitle("Participants in this observation")


table(data$Gender)
prop.table(table(data$Gender))


typeof(data$Grade)

data$Grade<-as.factor(data$Grade)


paletteEcoFin1 <- c("#9A5EA6", "#E5C473", "#B98B50", "#61276D")
ggplot(data, aes(x="", y="", fill=Grade)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  scale_fill_manual(values=paletteEcoFin1) + ggtitle("Grade that are participants going")



#Creating dateset with only 5th graders
Grade_5<-data[data$Grade !="6" & data$Grade !="7"& data$Grade !="8", ]
table(Grade_5$Gender)

prop.table(table(Grade_5$Gender))


#Creating dateset with only 6th graders
Grade_6<-data[data$Grade !="5" & data$Grade !="7"& data$Grade !="8", ]
table(Grade_6$Gender)
prop.table(table(Grade_6$Gender))

#Creating dateset with only 7th graders
Grade_7<-data[data$Grade !="6" & data$Grade !="5"& data$Grade !="8", ]

table(Grade_7$Gender)
prop.table(table(Grade_7$Gender))


#Creating dateset with only 8th graders
Grade_8<-data[data$Grade !="6" & data$Grade !="7"& data$Grade !="5", ]
table(Grade_8$Gender)
prop.table(table(Grade_8$Gender))





#Scatter Plot of final grade last semester
ggplot(data, aes(x=GLS, y=Grade, color=Gender)) +
  labs(color = "Gender:")+
  scale_x_continuous(name = "Final Grade Last Semestar",
                     limits = c(3, 5),
                     breaks = seq(3, 5, by = 0.10))+
  scale_y_continuous(name = "Grade ",
                     limits = c(5, 8),
                     breaks = seq(5, 8, by = 1))+
  geom_point(size=4)+theme_minimal() 


#Summary of all grades for last semester
summary(Grade_5$GLS)
sd(Grade_5$GLS)

summary(Grade_6$GLS)
sd(Grade_6$GLS)

summary(Grade_7$GLS)
sd(Grade_7$GLS)

summary(Grade_8$GLS)
sd(Grade_8$GLS)


#Scatter Plot of final grade this semester
ggplot(data, aes(x=GDS, y=Grade, color=Gender)) +
  labs(color = "Gender:")+
  scale_x_continuous(name = "Final Grade This Semestar",
                     limits = c(3, 5),
                     breaks = seq(3, 5, by = 0.10))+
  scale_y_continuous(name = "Grade ",
                     limits = c(5, 8),
                     breaks = seq(5, 8, by = 1))+
  geom_point(size=4)+theme_minimal() 


#Summary of all grades for this semester
summary(Grade_5$GDS)
sd(Grade_5$GDS)

summary(Grade_6$GDS)
sd(Grade_6$GDS)

summary(Grade_7$GDS)
sd(Grade_7$GDS)

summary(Grade_8$GDS)
sd(Grade_8$GDS)



#Histogram of days in week
ggplot(data = data) + 
  geom_bar(mapping = aes(x = DIW, fill = Gender), position = "dodge")+
  scale_y_continuous(name = "Number Of Children")+ xlab("How many days in week do children play video games") + 
  scale_x_continuous(breaks = seq(-1, 8, by = 1))+theme_minimal() 

table(data$DIW,data$Gender)
prop.table(table(data$DIW,data$Gender))


#Histogram of hours per day
ggplot(data = data) + 
  geom_bar(mapping = aes(x = HPD, fill = Gender), position = "dodge")+
  scale_y_continuous(name = "Number Of Children")+ xlab("Hours per day")+ 
  scale_x_continuous(breaks = seq(-1, 7, by = 1))+theme_minimal() 

table(data$HPD)
table(data$HPD,data$Gender)

#Count of categorical variables
table(data$PT)
table(data$PT,data$Gender)
prop.table(table(data$PT,data$Gender))

table(data$OPN_TPG)
table(data$OPN_TPG,data$Gender)
prop.table(table(data$OPN_TPG,data$Gender))

table(data$OPN_AYG)
table(data$OPN_AYG,data$Gender)
prop.table(table(data$OPN_AYG,data$Gender))

table(data$PP)
table(data$PP,data$Gender)
prop.table(table(data$PT,data$Gender))


#Creating new datasets

Male<-data[data$Gender !="F", ]

Female<-data[data$Gender !="M", ]



#Density plot
ggplot(data, aes(x=GLS, color=Gender)) +
  geom_density()+ geom_vline(aes(xintercept=mean(GLS)),color="blue", linetype="dashed", size=1)+
  xlab("Finale Grade Last Semestar")+theme_minimal() 

ggplot(data, aes(x=GDS, color=Gender)) +
  geom_density()+ geom_vline(aes(xintercept=mean(GDS)),color="blue", linetype="dashed", size=1)+
  xlab("Finale Grade This Semestar")+theme_minimal() 




#two sample t-test
t.test(data$Age~data$Gender ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)
t.test(data$GLS~data$Gender ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)
t.test(data$GDS~data$Gender ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)
t.test(data$DIW~data$Gender ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)
t.test(data$HPD~data$Gender ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)
t.test(data$Scale~data$Gender ,mu=0,alt="two.sided",conf=0.95,var.eq=F,paired=F)


#p-values

compare_means(Age~Gender,data = data, ref.group = ".all.",method = "t.test")
compare_means(GDS~Gender,data = data, ref.group = ".all.",method = "t.test")
compare_means(GLS~Gender,data = data, ref.group = ".all.",method = "t.test")
compare_means(DIW~Gender,data = data, ref.group = ".all.",method = "t.test")
compare_means(HPD~Gender,data = data, ref.group = ".all.",method = "t.test")


#Plot of age

Age1<-ggboxplot(data, x = "Gender", y = "Age",
               color="Gender",palette = "jco",
               add = "jitter")+ stat_compare_means(method = "t.test") 

Age2<-ggboxplot(data, x = "Gender", y = "Age",
               color = "Gender", palette = "jco")+    # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.") 

plot_grid(Age1,Age2, labels = "AUTO")



#Plot of GLS
GLS1<-ggboxplot(data, x = "Gender", y = "GLS",
                color="Gender",palette = "jco",
                add = "jitter")+ stat_compare_means(method = "t.test") 

GLS2<-ggboxplot(data, x = "Gender", y = "GLS",
                color = "Gender", palette = "jco")+    # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.") 

plot_grid(GLS1,GLS2)


#Plot of GDS
GDS1<-ggboxplot(data, x = "Gender", y = "GDS",
                color="Gender",palette = "jco",
                add = "jitter")+ stat_compare_means(method = "t.test") 

GDS2<-ggboxplot(data, x = "Gender", y = "GDS",
                color = "Gender", palette = "jco")+    # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.") 

plot_grid(GDS1,GDS2)

#Plot of DIW
DIW1<-ggboxplot(data, x = "Gender", y = "DIW",
                color="Gender",palette = "jco",
                add = "jitter")+ stat_compare_means(method = "t.test") 

DIW2<-ggboxplot(data, x = "Gender", y = "DIW",
                color = "Gender", palette = "jco")+    # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.") 

plot_grid(DIW1,DIW2)


HPD1<-ggboxplot(data, x = "Gender", y = "HPD",
                color="Gender",palette = "jco",
                add = "jitter")+ stat_compare_means(method = "t.test") 

HPD2<-ggboxplot(data, x = "Gender", y = "HPD",
                color = "Gender", palette = "jco")+    # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.") 

plot_grid(HPD1,HPD2)

#correlation

#change  Type 

data$Age<-as.numeric(data$Age)
data$Grade<-as.numeric(data$Grade)
data$GLS<-as.numeric(data$GLS)
data$GDS<-as.numeric(data$GDS)
data$DIW<-as.numeric(data$DIW)
data$HPD<-as.numeric(data$HPD)
data$Scale<-as.numeric(data$Scale)



Corrall<-data[c(3,4,5,6,8,9,10)]
corrplot(corrgram(Corrall), method = "number")

my_cols <- c("#00AFBB", "#E7B800") 
pairs(data [c(3,4,5,6,8,9,10)], pch = 19,  cex = 1,
      col = my_cols[data$Gender],
      lower.panel=NULL)

pairs.panels(data,pch=10)
pairs.panels(data [c(3,4,5,6,8,9,10)])


Decision_Tree <- ctree(Gender ~Age + GLS + GDS + DIW+HPD, data=data)
print(Decision_Tree)
plot(Decision_Tree)



  #linear regresion
data=data[,-1]

library(caTools)

# Randomly split the data into training and testing sets
set.seed(6000)
split = sample.split(data$Gender, SplitRatio = 0.70)

# Split up the data using subset
data_train = subset(data, split==TRUE)
data_test = subset(data, split==FALSE)


#----------Model 1----------------


typeof(data$Age)
typeof(data$GLS)
data$Age<-as.numeric(data$Age)


typeof(data$Age)
typeof(data$HPD)

#linear model for GLS all values
Linear_Model_1<-lm(GLS ~Gender+Age+Grade+PVG+DIW+HPD+Scale+PT+OPN_TPG+OPN_AYG+PP, data=data_train)
summary(Linear_Model_1)
plot(Linear_Model_1)


predictTest1 = predict(Linear_Model_1, newdata=data_test)
predictTest1
summary(predictTest1)

#sum of errors



SSE <- sum(Linear_Model_1$residuals^2)
SST = sum((data_test$GLS - mean(data_test$GLS))^2)
R2= 1 - SSE/SST
RMSE <- sqrt(SSE / nrow(data_train))

#----------Model 2----------------


#linear model for GLS all values
Linear_Model_2<-lm(GLS ~Gender+Age+Grade+PVG+DIW+HPD, data=data_train)
summary(Linear_Model_2)
plot(Linear_Model_2)


predictTest2 = predict(Linear_Model_2, newdata=data_test)
predictTest2
summary(predictTest1)

#sum of errors

SSE <- sum(Linear_Model_2$residuals^2)
SST = sum((data_test$GLS - mean(data_test$GLS))^2)
R2= 1 - SSE/SST
RMSE <- sqrt(SSE / nrow(data_train))


#----------Model 3----------------


#linear model for GLS all values
Linear_Model_3<-lm(GDS ~Gender+Age+Grade+PVG+DIW+HPD+Scale+PT+OPN_TPG+OPN_AYG+PP, data=data_train)
summary(Linear_Model_3)
plot(Linear_Model_3)


predictTest3 = predict(Linear_Model_3, newdata=data_test)
predictTest3
summary(predictTest1)

#sum of errors



SSE <- sum(Linear_Model_3$residuals^2)
SST = sum((data_test$GDS - mean(data_test$GDS))^2)
R2= 1 - SSE/SST
RMSE <- sqrt(SSE / nrow(data_train))




#----------Model 4----------------

#linear model for GLS all values
Linear_Model_4<-lm(GDS ~Gender+Age+Grade+PVG+DIW+HPD, data=data_train)
summary(Linear_Model_4)
plot(Linear_Model_4)


predictTest4 = predict(Linear_Model_4, newdata=data_test)
predictTest4
summary(predictTest4)

#sum of errors

SSE <- sum(Linear_Model_4$residuals^2)
SST = sum((data_test$GDS - mean(data_test$GDS))^2)
R2= 1 - SSE/SST
RMSE <- sqrt(SSE / nrow(data_train))

#RandomForest

NewData<-rfImpute(Gender~.,data=data,iter=6)
Rf1<-randomForest(Gender ~ ., data = NewData,proximity=TRUE)

ob.error.data <- data.frame(
  Trees=rep(1:nrow(Rf1$err.rate), times=3),
  Type=rep(c("OOB", "M", "F"), each=nrow(Rf1$err.rate)),
  Error=c(Rf1$err.rate[,"OOB"], 
          Rf1$err.rate[,"M"],
          Rf1$err.rate[,"F"]))

ggplot(data=ob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))+theme_classic()

