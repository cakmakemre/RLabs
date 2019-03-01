#Emre Çakmak
#lab
data("faithful")
head(faithful)

faith <- faithful$
faith

max<-max(faithful$eruptions)
min<-min(faithful$eruptions)

breaks<-seq(1,5.5,by=0.5)
breaks
faithful.cut<-cut(faith,breaks,right=FALSE)
faithful.cut.groupFreq<-table(faithful.cut)
cbind(faithful.cut.groupFreq)

cat("Max of the interval is",max(faithful.cut.groupFreq))


#2

library(MASS)
library(plyr)

whoschool<-painters$School

whoschool

school.freq<-count(whoschool)
school.freq
df<-data.frame(school.freq)
df

painters
df


library(MASS)
school <- painters$School
school.freq <- table(school)

xxx<-max(school.freq)
yyy<-which(school.freq==xxx)
names(yyy)

#3
cities<-read.delim("ctiesTemperature.txt")
cities

apply(cities,2,max)
apply(cities,2,min)


#4



citiesTempature <- read.delim("ctiesTemperature.txt", header = T )
cbind(citiesTempature)
citiesTempature
class(citiesTempature)


maximum <- apply(citiesTempature,2,max)
maximum
minimum <-apply(citiesTempature,2,min)
minimum

x <- table(citiesTempature)

cbind(school.freq)


findLetterGrade <- function(dataframe){
  grade <- dataframe$m1*20/100 + dataframe$m2*35/100 + dataframe$final*45/100
  dataframe$grade <- 4*grade/100
  dataframe$letterGrade <- ifelse( dataframe$grade >= 3.41, "A",
                                   ifelse( dataframe$grade >= 2.41, "B",
                                           ifelse( dataframe$grade >= 1.41, "C", 
                                                   ifelse( dataframe$grade >=1.0, "D", "F"))))
  
  return (dataframe)
}

df <- read.csv("grades.csv")
df <- findLetterGrade(df)

percAgrade <- 100 * nrow(df[df$letterGrade=="A",]) / length(df$m1)
percBgrade <- 100 * nrow(df[df$letterGrade=="B",]) / length(df$m1)
percCgrade <- 100 * nrow(df[df$letterGrade=="C",]) / length(df$m1)
percDgrade <- 100 * nrow(df[df$letterGrade=="D",]) / length(df$m1)
percFgrade <- 100 * nrow(df[df$letterGrade=="F",]) / length(df$m1)

tail(df)


freqtable <- table(df$letterGrade)
cbind(freqtable)


freqrel <- table(df$grade)
cbind(freqrel)


final <- df$final
breaks<- seq(0,100,by=5)
freqnew <- cut(final,breaks,right=FALSE)
newnew <- table(freqnew)
cbind(newnew)


barplot(newnew)

newbreaks <- seq(15, 95, by=5) 

overal <- df$grade*25

hist(overal,breaks = newbreaks)


barplot(freqtable)
