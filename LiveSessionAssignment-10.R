
#Load libraries and data

library(ggplot2)
library(doBy)
library(caroline)

# Data obtained in zipped form from GitHub at https://github.com/oreillymedia/doing_data_science

data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt5.csv"))
head(data1)

#Create age_group variable to categorize by ages < 18, 18-24, 25-34, 35-44, 45-54, 55-64 and 65+

## Create age_group category
data1$ageGroup <-cut(data1$Age,c(-Inf,18,24,34,44,54,64,Inf))# Categorize users by "<18","18-24","25-34","35-44","45-54","55-64", and "65+"
head(data1)# Check to see that age_group categories added
levels(data1$ageGroup) <- c("<18","18-24","25-34","35-44","45-54","55-64","65+") # Update category names (cut() created factor so we need to change the factor levels)
levels(data1$ageGroup)  # Confirm change
head(data1)

#Plot Impressions by age_group for EDA

## Explore Impressions by age_group
ggplot(data = data1, aes(x=ageGroup, y=Impressions, fill=ageGroup)) +
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill="blue") +     # Add blue mean diamond marker to boxplot
  xlab("Age Group") + ylab("Number of Impressions") + ggtitle("Number of Impressions by Age Group")

## Add CTR column

impsub<-subset(data1, Impressions>0)
head(impsub)
impsub$CTR <- impsub$Clicks/impsub$Impressions
head(impsub)
y=t(impsub$Gender)
ncol(y)
y3=t(impsub$Signed_In)
ncol(y3)

#Plot CTR for EDA

ggplot(data =impsub, aes(x=ageGroup, y=CTR, fill=ageGroup)) +
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill="blue") +     # Add blue mean diamond marker to boxplot
  xlab("Age Group") + ylab("CTR (Clicks/Impressions)") + ggtitle("Click-Through-Rate by Age Group")




#Plot log of CTR for EDA

ggplot(data = impsub, aes(x=ageGroup, y=log(CTR), fill=ageGroup)) +
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill="blue") +     # Add blue mean diamond marker to boxplot
  xlab("Age Group") + ylab("Logarithmic CTR (Clicks/Impressions)") + ggtitle("Logarithmic Click-Through-Rate by Age Group") # Provide labels

# Define a new variable to segment users based on click -through-rate (CTR) behavior

impsub$CTRSeg <-cut(impsub$CTR,c(-Inf,0.2,0.4,0.6,0.8,Inf))
head(impsub)
levels(impsub$CTRSeg) <- c("CTR<0.2","0.2<=CTR<0.4","0.4<=CTR<0.6","0.6<=CTR<0.8","CTR>0.8") 
levels(impsub$CTRSeg)

# Get the total number of Male, Impressions, Clicks and Signed_In

head(impsub)
impsubMale<-subset(impsub, Gender>0)
head(impsubMale)
x=t(impsubMale$Gender)
ncol(x)
x1=t(impsubMale$Impressions)
ncol(x1)
x2=t(impsubMale$Clicks)
ncol(x2)
x3=t(impsubMale$Signed_In)
ncol(x3)
pctMale<-(ncol(x)/ncol(y))*100
pctMale
pctSigned<-(ncol(x3)/ncol(y3))*100
pctSigned

# Get the mean of Age, Impressions, Clicks, CTR and percentage of males and signed_In 

sapply(impsubMale, mean, na.rm=TRUE)


# Get the means of Impressions, Clicks, CTR and percentage of males and signed_In  by AgeGroup.

meanImp<-tapply(impsubMale$Impressions, impsubMale$ageGroup, mean, na.rm=TRUE) 
meanImp
meanclicks<-tapply(impsubMale$Clicks, impsubMale$ageGroup, mean, na.rm=TRUE) 
meanclicks
meanCTR<-tapply(impsubMale$CTR, impsubMale$ageGroup, mean, na.rm=TRUE) 
meanCTR
meanMale<-tapply(impsubMale$Gender, impsubMale$ageGroup, sum, na.rm=TRUE) 
meanMale
meanMalepct<-as.data.frame(meanMale)
meanMalepct1<-meanMalepct/ncol(x)*100
meanMalepct1
meanSigned<-tapply(impsubMale$Signed_In, impsubMale$ageGroup, sum, na.rm=TRUE) 
meanSigned
meanSignedpct<-as.data.frame(meanSigned)
meanSignedpct1<-meanSignedpct/ncol(x3)*100
meanSignedpct1

# Create a table of CTRGroup vs AgeGroup counts


clen <- function(x){c(length(x))}
etable1<-summaryBy(CTRSeg~ageGroup,data = impsubMale, FUN=clen)
etable1
etable2<-summaryBy(ageGroup~CTRSeg,data = impsubMale, FUN=clen)
etable2

#Plot Impressions by age_group for EDA


ggplot(data = impsubMale, aes(x=ageGroup, y=Impressions, fill=ageGroup)) +
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill="blue") +     # Add blue mean diamond marker to boxplot
  xlab("Age Group") + ylab("Number of Impressions") + ggtitle("Number of Impressions by Age Group")

#Plot CTR for EDA

ggplot(data =impsubMale, aes(x=ageGroup, y=CTR, fill=ageGroup)) +
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill="blue") +     # Add blue mean diamond marker to boxplot
  xlab("Age Group") + ylab("CTR (Clicks/Impressions)") + ggtitle("Click-Through-Rate by Age Group")



#Plot log of CTR for EDA

ggplot(data = impsubMale, aes(x=ageGroup, y=log(CTR), fill=ageGroup)) +
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=23, size=3, fill="blue") +     # Add blue mean diamond marker to boxplot
  xlab("Age Group") + ylab("Logarithmic CTR (Clicks/Impressions)") + ggtitle("Logarithmic Click-Through-Rate by Age Group") # Provide labels

