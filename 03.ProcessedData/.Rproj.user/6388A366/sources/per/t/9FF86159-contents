
#####################
# Code for EDA
# Dataset: Collected.xlsx
# 
# EDAs to run: 
# . Summary
# . Sample proportion
# . Skew, missing values, distribution, sharpness of peak 
# . Correlations 

# Qns to answer: 
# . Distribution of F/S across time + total # of hard drives 
# . Which variables have many missing values? Is there a pattern to the missing values?
# . What variable does not have any variance? Or very few values?
# . What is the shape of the graph? 

#####################

library (openxlsx)
library(lattice)
library (DataExplorer) # https://datascienceplus.com/blazing-fast-eda-in-r-with-dataexplorer/
library(corrplot)
library(dplyr)


a <- cor(t[6:32], use = "pairwise.complete.obs")
corrplot(a, method="circle")
         
         
#### Variables 
filename <- paste("collected",".xlsx", sep = "")

#### Load dataset
hd = read.xlsx(filename, detectDates= TRUE)

### using dplyr to get sums 
count(hd,failure)


#### Using DataExplorer
PlotStr(hd)
PlotMissing(hd)

HistogramContinuous(hd)

### EDA: Describe the data
dim(hd)
summary(hd)
object.size(hd)
PlotStr(hd)
PlotMissing(hd)

#### remove all missing values
# _normalized

hd1 <- hd[, -grep("normalized$", colnames(hd))]

# Remove columns > 97% missing values
a <- colSums(is.na(hd1))/nrow(hd1)
b <- a <0.03
hd2 <- hd1[,b]

### outlier box plot 
bwplot( ~ smart_12_raw,     # use interaction
                    data=t,
                    scales=list(relation="free",x=list(cex=1.1),y=list(cex=1.25)),
                    main= list("All Metric Values", cex=2.5),
                    xlab=list("Treatments", cex=2.5),
                    ylab=list("Metric Value", cex=2.5),
                    do.out = FALSE,
                    col="black",   
                    coef=4
)

for (i in 5:32) {
  
  d1 <- t[,i]
  d1 <- d1[!is.na(d1)]
  n <- colnames(t)[i]
  
  tPlot <- bwplot(d1, ylab="Values", main=n) 
  show(tPlot)
}

### EDA: Correlations
CorrelationContinuous(hd2)