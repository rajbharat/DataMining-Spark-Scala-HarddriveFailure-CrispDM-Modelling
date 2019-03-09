
library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.
building <- TRUE
scoring  <- ! building
crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2019-03-03 20:43:40 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/Bharat/Downloads/normalized.csv" 
crs$dataset <- read.csv(fname,
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-03-03 20:43:43 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=5215 train=3650 validate=782 test=783

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test

# The following variable selections have been noted.

crs$input     <- c("X", "date", "serial_number", "failure",
                   "smart_1_raw", "smart_4_raw", "smart_5_raw",
                   "smart_7_raw", "smart_9_raw", "smart_12_raw",
                   "smart_187_raw", "smart_188_raw", "smart_189_raw",
                   "smart_190_raw", "smart_191_raw", "smart_192_raw",
                   "smart_193_raw", "smart_194_raw", "smart_195_raw",
                   "smart_196_raw", "smart_197_raw", "smart_198_raw",
                   "smart_199_raw", "smart_200_raw", "smart_201_raw",
                   "smart_220_raw", "smart_224_raw", "smart_225_raw",
                   "smart_226_raw", "smart_242_raw", "smart_250_raw",
                   "smart_251_raw")

crs$numeric   <- c("X", "failure", "smart_1_raw", "smart_4_raw",
                   "smart_5_raw", "smart_7_raw", "smart_9_raw",
                   "smart_12_raw", "smart_187_raw", "smart_188_raw",
                   "smart_189_raw", "smart_190_raw", "smart_191_raw",
                   "smart_192_raw", "smart_193_raw", "smart_194_raw",
                   "smart_195_raw", "smart_196_raw", "smart_197_raw",
                   "smart_198_raw", "smart_199_raw", "smart_200_raw",
                   "smart_201_raw", "smart_220_raw", "smart_224_raw",
                   "smart_225_raw", "smart_226_raw", "smart_242_raw",
                   "smart_250_raw", "smart_251_raw")

crs$categoric <- c("date", "serial_number")

crs$target    <- "naVal"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- "model"
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-03-03 20:43:54 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=5215 train=3650 validate=782 test=783

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test

# The following variable selections have been noted.

crs$input     <- c("smart_1_raw", "smart_4_raw", "smart_5_raw",
                   "smart_7_raw", "smart_9_raw", "smart_12_raw",
                   "smart_187_raw", "smart_188_raw", "smart_189_raw",
                   "smart_190_raw", "smart_191_raw", "smart_192_raw",
                   "smart_193_raw", "smart_194_raw", "smart_195_raw",
                   "smart_196_raw", "smart_197_raw", "smart_198_raw",
                   "smart_199_raw", "smart_200_raw", "smart_201_raw",
                   "smart_220_raw", "smart_224_raw", "smart_225_raw",
                   "smart_226_raw", "smart_242_raw", "smart_250_raw",
                   "smart_251_raw")

crs$numeric   <- c("smart_1_raw", "smart_4_raw", "smart_5_raw",
                   "smart_7_raw", "smart_9_raw", "smart_12_raw",
                   "smart_187_raw", "smart_188_raw", "smart_189_raw",
                   "smart_190_raw", "smart_191_raw", "smart_192_raw",
                   "smart_193_raw", "smart_194_raw", "smart_195_raw",
                   "smart_196_raw", "smart_197_raw", "smart_198_raw",
                   "smart_199_raw", "smart_200_raw", "smart_201_raw",
                   "smart_220_raw", "smart_224_raw", "smart_225_raw",
                   "smart_226_raw", "smart_242_raw", "smart_250_raw",
                   "smart_251_raw")

crs$categoric <- NULL

crs$target    <- "failure"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("X", "date", "serial_number", "model", "naVal")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-03-03 20:44:33 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation normalized.csv using Pearson",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2019-03-03 20:45:29 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="spearman")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation normalized.csv using Spearman",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)


#=======================================================================
# Rattle timestamp: 2019-03-03 20:54:28 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

# crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="kendall")
# 
# # Order the correlations by their strength.
# 
# crs$ord <- order(crs$cor[1,])
# crs$cor <- crs$cor[crs$ord, crs$ord]
# 
# # Display the actual correlations.
# 
# print(crs$cor)
# 
# # Graphically display the correlations.
# 
# opar <- par(cex=0.5)
# corrplot(crs$cor, mar=c(0,0,1,0))
# title(main="Correlation normalized.csv using Kendall",
#       sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
# par(opar)

########EDA#################


#=======================================================================
# Rattle timestamp: 2019-03-03 21:36:01 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for smart_1_raw

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=smart_1_raw)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2019-Mar-03 21:36:01 Bharat") +
  ggplot2::ggtitle("Distribution of smart_1_raw (sample)") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2019-03-03 21:36:02 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for smart_1_raw

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(smart_1_raw) %>%
  ggplot2::ggplot(ggplot2::aes(x=smart_1_raw)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("smart_1_raw\n\nRattle 2019-Mar-03 21:36:02 Bharat") +
  ggplot2::ggtitle("Distribution of smart_1_raw (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)





#=======================================================================
# Rattle timestamp: 2019-03-03 23:55:36 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for smart_199_raw

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  ggplot2::ggplot(ggplot2::aes(y=smart_199_raw)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=failure, fill=failure), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=failure), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("failure\n\nRattle 2019-Mar-03 23:55:36 Bharat") +
  ggplot2::ggtitle("Distribution of smart_199_raw (sample)\nby failure") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2019-03-03 23:55:37 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for smart_199_raw

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  dplyr::select(smart_199_raw, failure) %>%
  ggplot2::ggplot(ggplot2::aes(x=smart_199_raw)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=failure, colour=failure), alpha=0.55) +
  ggplot2::xlab("smart_199_raw\n\nRattle 2019-Mar-03 23:55:37 Bharat") +
  ggplot2::ggtitle("Distribution of smart_199_raw (sample)\nby failure") +
  ggplot2::labs(fill="failure", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)


#=======================================================================
# Rattle timestamp: 2019-03-03 23:56:15 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for smart_189_raw

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  ggplot2::ggplot(ggplot2::aes(y=smart_189_raw)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=failure, fill=failure), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=failure), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("failure\n\nRattle 2019-Mar-03 23:56:15 Bharat") +
  ggplot2::ggtitle("Distribution of smart_189_raw (sample)\nby failure") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2019-03-03 23:56:17 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for smart_189_raw

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  dplyr::select(smart_189_raw, failure) %>%
  ggplot2::ggplot(ggplot2::aes(x=smart_189_raw)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=failure, colour=failure), alpha=0.55) +
  ggplot2::xlab("smart_189_raw\n\nRattle 2019-Mar-03 23:56:17 Bharat") +
  ggplot2::ggtitle("Distribution of smart_189_raw (sample)\nby failure") +
  ggplot2::labs(fill="failure", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)



#=======================================================================
# Rattle timestamp: 2019-03-03 23:59:46 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for smart_194_raw

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  ggplot2::ggplot(ggplot2::aes(y=smart_194_raw)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=failure, fill=failure), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=failure), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("failure\n\nRattle 2019-Mar-03 23:59:46 Bharat") +
  ggplot2::ggtitle("Distribution of smart_194_raw (sample)\nby failure") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2019-03-03 23:59:47 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for smart_194_raw

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  dplyr::select(smart_194_raw, failure) %>%
  ggplot2::ggplot(ggplot2::aes(x=smart_194_raw)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=failure, colour=failure), alpha=0.55) +
  ggplot2::xlab("smart_194_raw\n\nRattle 2019-Mar-03 23:59:47 Bharat") +
  ggplot2::ggtitle("Distribution of smart_194_raw (sample)\nby failure") +
  ggplot2::labs(fill="failure", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)




#=======================================================================
# Rattle timestamp: 2019-03-04 00:00:15 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for smart_193_raw

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  ggplot2::ggplot(ggplot2::aes(y=smart_193_raw)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=failure, fill=failure), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=failure), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("failure\n\nRattle 2019-Mar-04 00:00:15 Bharat") +
  ggplot2::ggtitle("Distribution of smart_193_raw (sample)\nby failure") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2019-03-04 00:00:16 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for smart_193_raw

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  dplyr::select(smart_193_raw, failure) %>%
  ggplot2::ggplot(ggplot2::aes(x=smart_193_raw)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=failure, colour=failure), alpha=0.55) +
  ggplot2::xlab("smart_193_raw\n\nRattle 2019-Mar-04 00:00:16 Bharat") +
  ggplot2::ggtitle("Distribution of smart_193_raw (sample)\nby failure") +
  ggplot2::labs(fill="failure", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)


#=======================================================================
# Rattle timestamp: 2019-03-04 00:00:53 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for smart_226_raw

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  ggplot2::ggplot(ggplot2::aes(y=smart_226_raw)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=failure, fill=failure), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=failure), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("failure\n\nRattle 2019-Mar-04 00:00:53 Bharat") +
  ggplot2::ggtitle("Distribution of smart_226_raw (sample)\nby failure") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2019-03-04 00:00:55 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for smart_226_raw

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(failure=as.factor(failure)) %>%
  dplyr::select(smart_226_raw, failure) %>%
  ggplot2::ggplot(ggplot2::aes(x=smart_226_raw)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=failure, colour=failure), alpha=0.55) +
  ggplot2::xlab("smart_226_raw\n\nRattle 2019-Mar-04 00:00:55 Bharat") +
  ggplot2::ggtitle("Distribution of smart_226_raw (sample)\nby failure") +
  ggplot2::labs(fill="failure", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)

##modelling



#=======================================================================
# Rattle timestamp: 2019-03-04 00:13:21 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(failure ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0),
                   model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.67 secs

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)

#=======================================================================
# Rattle timestamp: 2019-03-04 00:13:41 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree normalized.csv $ failure")



#=======================================================================
# Rattle timestamp: 2019-03-04 00:14:28 x86_64-w64-mingw32 

# Score the validation dataset. 

# Obtain probability scores for the Decision Tree model on normalized.csv [validate].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input)],
                  type="class")

# Obtain cluster number for the KMeans model on normalized.csv [validate].

crs$pr <- predict(crs$kmeans, crs$dataset[crs$validate, c(crs$input)])

# Obtain cluster number for the Hierarchical model on normalized.csv [validate].

crs$pr <- predict(crs$hclust, nclust=10, newdata=crs$dataset[crs$validate, c(crs$numeric)], x=crs$dataset[crs$train, c(crs$numeric)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$validate,], select=c("failure"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="C:\Users\Bharat\Documents\normalized_validate_score_idents.csv", row.names=FALSE)

#=======================================================================
# Rattle timestamp: 2019-03-04 00:14:47 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:15:04 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:15:16 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rpart model on normalized.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree normalized.csv [test] failure")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2019-03-04 00:15:23 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rpart model on normalized.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the rpart model on normalized.csv [train].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$failure),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="rpart", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:15:26 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rpart model on normalized.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the rpart model on normalized.csv [train].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$failure),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="rpart", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:15:30 x86_64-w64-mingw32 

# Score the testing dataset. 


##XGB-Adaptive


#=======================================================================
# Rattle timestamp: 2019-03-04 00:17:57 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(failure ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=6,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 15.20 secs

# Plot the relative importance of the variables.

ada::varplot(crs$ada)

# Plot the error rate as we increase the number of trees.

plot(crs$ada)

# Display tree number 1.

listTreesAda(crs$ada, 1)

# Display tree number 1.

drawTreesAda(crs$ada, 1, ": normalized.csv $ failure")

#=======================================================================
# Rattle timestamp: 2019-03-04 00:18:52 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Extreme Boost model.

# Obtain the response from the Extreme Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:18:59 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Extreme Boost model.

# Obtain the response from the Extreme Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:19:05 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on normalized.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Extreme Boost normalized.csv [test] failure")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2019-03-04 00:19:11 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on normalized.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the ada model on normalized.csv [train].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$failure),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ada", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:19:16 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ada model on normalized.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the ada model on normalized.csv [train].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$failure),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ada", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:19:20 x86_64-w64-mingw32 

# Score the testing dataset. 




#=======================================================================
# Rattle timestamp: 2019-03-04 00:21:57 x86_64-w64-mingw32 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(as.factor(failure) ~ .,
                                     data=crs$dataset[crs$train, c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=5,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted))

# Calculate the AUC Confidence Interval.

pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted))FALSE

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 16.82 secs

#=======================================================================
# Rattle timestamp: 2019-03-04 00:22:35 x86_64-w64-mingw32 

# Plot the relative importance of the variables.

p <- ggVarImp(crs$rf,
              title="Variable Importance Random Forest normalized.csv")
p

# Plot the error rate against the number of trees.

plot(crs$rf, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest normalized.csv",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Plot the OOB ROC curve.

library(verification)
aucc <- verification::roc.area(as.integer(as.factor(crs$dataset[crs$train, crs$target]))-1,
                               crs$rf$votes[,2])$A
verification::roc.plot(as.integer(as.factor(crs$dataset[crs$train, crs$target]))-1,
                       crs$rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Random Forest normalized.csv",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Display tree number 1.

printRandomForests(crs$rf, 1)

#=======================================================================
# Rattle timestamp: 2019-03-04 00:23:09 x86_64-w64-mingw32 

# Score the validation dataset. 

#=======================================================================
# Rattle timestamp: 2019-03-04 00:23:18 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:23:21 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:23:25 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rf model on normalized.csv [test].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest normalized.csv [test] failure")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2019-03-04 00:23:28 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rf model on normalized.csv [test].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the rf model on normalized.csv [train].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="rf", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:23:31 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rf model on normalized.csv [test].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the rf model on normalized.csv [train].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="rf", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:23:34 x86_64-w64-mingw32 

# Score the testing dataset. 




#=======================================================================
# Rattle timestamp: 2019-03-04 00:25:11 x86_64-w64-mingw32 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(failure) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 6.16 secs

#=======================================================================
# Rattle timestamp: 2019-03-04 00:25:23 x86_64-w64-mingw32 

# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$train, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$train, c(crs$input, crs$target)])$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$train, c(crs$input, crs$target)])$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:25:29 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:25:31 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:25:37 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on normalized.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM normalized.csv [test] failure")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2019-03-04 00:25:42 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on normalized.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the ksvm model on normalized.csv [train].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ksvm", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:25:46 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ksvm model on normalized.csv [test].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the ksvm model on normalized.csv [train].

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                           type    = "probabilities")[,2]

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$failure),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ksvm", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:25:49 x86_64-w64-mingw32 

# Score the testing dataset. 



#=======================================================================
# Rattle timestamp: 2019-03-04 00:26:17 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(failure ~ .,
               data=crs$dataset[crs$train, c(crs$input, crs$target)],
               family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(crs$glm))

cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(crs$glm)[1],
            attr(logLik(crs$glm), "df")))

cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            crs$glm$null.deviance-crs$glm$deviance,
            crs$glm$df.null-crs$glm$df.residual))

cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(crs$glm$null.deviance-crs$glm$deviance,
                   crs$glm$df.null-crs$glm$df.residual)))

cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(crs$glm$y, crs$glm$fitted.values)))

cat('\n==== ANOVA ====\n\n')
print(anova(crs$glm, test="Chisq"))
cat("\n")

# Time taken: 6.59 secs

#=======================================================================
# Rattle timestamp: 2019-03-04 00:26:36 x86_64-w64-mingw32 

# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

crs$pr <- as.vector(ifelse(predict(crs$glm, 
                                   type    = "response",
                                   newdata = crs$dataset[crs$train, c(crs$input, crs$target)]) > 0.5, "1", "0"))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$train, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$train, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:26:40 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

crs$pr <- as.vector(ifelse(predict(crs$glm, 
                                   type    = "response",
                                   newdata = crs$dataset[crs$validate, c(crs$input, crs$target)]) > 0.5, "1", "0"))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:26:42 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

crs$pr <- as.vector(ifelse(predict(crs$glm, 
                                   type    = "response",
                                   newdata = crs$dataset[crs$test, c(crs$input, crs$target)]) > 0.5, "1", "0"))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:26:46 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the glm model on normalized.csv [test].

crs$pr <- predict(crs$glm, 
                  type    = "response",
                  newdata = crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear normalized.csv [test] failure")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2019-03-04 00:26:49 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the glm model on normalized.csv [test].

crs$pr <- predict(crs$glm, 
                  type    = "response",
                  newdata = crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the glm model on normalized.csv [train].

crs$pr <- predict(crs$glm, 
                  type    = "response",
                  newdata = crs$dataset[crs$test, c(crs$input, crs$target)])

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$failure),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="glm", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:27:10 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for glm model on normalized.csv [test].

crs$pr <- predict(crs$glm, 
                  type    = "response",
                  newdata = crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the glm model on normalized.csv [train].

crs$pr <- predict(crs$glm, 
                  type    = "response",
                  newdata = crs$dataset[crs$test, c(crs$input, crs$target)])

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$failure),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="glm", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:27:13 x86_64-w64-mingw32 

# Score the testing dataset. 



#=======================================================================
# Rattle timestamp: 2019-03-04 00:27:37 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(failure) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 size=10, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
            paste(crs$nnet$n, collapse="-"),
            length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
            paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
            names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
            sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 5.00 secs

#=======================================================================
# Rattle timestamp: 2019-03-04 00:28:04 x86_64-w64-mingw32 

# Evaluate model performance on the training dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$train, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$train, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$train, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:28:09 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:28:11 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$failure, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$failure, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-03-04 00:28:15 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on normalized.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net normalized.csv [test] failure")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2019-03-04 00:28:19 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the nnet model on normalized.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the nnet model on normalized.csv [train].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$failure),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="nnet", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:28:22 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for nnet model on normalized.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$failure)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the nnet model on normalized.csv [train].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$failure),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="nnet", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  normalized.csv ",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-03-04 00:28:28 x86_64-w64-mingw32 

# Score the testing dataset. 
