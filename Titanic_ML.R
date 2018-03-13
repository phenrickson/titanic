### This script is for inspecting, prepocessing, and engineering features ###


# Load packages
library(ggplot2) 
library(ggthemes) 
library(scales) 
library(dplyr)
library(mice) 
library(plotly)
library(data.table)
library(caret)
library(foreach)

setwd("/Users/philhenrickson/Dropbox/Titanic/")

# read in data
test<-titanic::titanic_test
train<-titanic::titanic_train

# bind together
full<-data.table(bind_rows(test, train))

#### Identify missigness
# missigness from ggplot
library(reshape2)
library(ggplot2)

ggplot_missing <- function(x){
        
        x %>% 
                is.na %>%
                melt %>%
                ggplot(data = .,
                       aes(x = Var2,
                           y = Var1)) +
                geom_raster(aes(fill = value)) +
                scale_fill_grey(name = "",
                                labels = c("Present","Missing")) +
                theme_minimal() + 
                theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
                labs(x = "Variables in Dataset",
                     y = "Rows / observations")
}

ggplot_missing(full)

# Lots of missigness on deck and age
colSums(is.na(full))
# Missing fare from one person

# What about the other variables?
table(full$Embarked)
# Missing Embarked for two people

table(full$Parch)
table(full$Cabin)
# Cabin has lots of missigness



# Let's fiddle with Cabin
length(which(!full$Cabin==""))
# 295 people with cabin reported
# 1014 without cabin

# why do some observations have multiple cabins?
full %>%
        filter(Cabin=="C22 C26")

full %>%
        filter(Cabin=="E46")

# Tickets seem to indicate that passengers stayed in the same cabin; deal with ticket in a second
# Extract info from Cabin
full$Deck<-sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1])
full$Deck[which(is.na(full$Deck))]<-"Missing"
full$Deck<-factor(full$Deck)


# relationship between deck and survival
ggplot(full[-which(is.na(full$Survived)),], aes(x=Deck, fill=factor(Survived))) +
        geom_bar(position='dodge')+
        labs(x = 'Deck') +
        theme_minimal()

# Let's look at ticket
table(full$Ticket)

# letters preceding ticketid probably indiciate crews?
# split this and identify the first letter
foo<-sapply(full$Ticket, function(x) strsplit(x, " ")[[1]][1])

# which ticketids are numeric?
numeric<-substr(foo, 1, 1) %in% as.character(seq(1,9))
table(numeric)
table(numeric, full$Survived)

full$TicketType<-ifelse(numeric==T, "Num", NA)

# subdivisions within the non-numeric tickets
table(full[numeric==F]$Ticket)
alpha<-substr(foo, 1, 1)[numeric==F]

full$TicketType[numeric==F]<-alpha

full$TicketType<-factor(full$TicketType)
rm(numeric, alpha)

# relationship between tickettype and survival
ggplot(full[-which(is.na(full$Survived)),], aes(x=TicketType, fill=factor(Survived))) +
        geom_bar(position='dodge')+
        labs(x = 'TicketType') +
        theme_minimal()

rm(foo)

# Ticket Numbers - split out the ticket numbers
foo<-sapply(full$Ticket, function(x) tail(strsplit(x, " ")[[1]], 1))
table(foo)

full$TicketNumbers<-factor(foo)

# What is the LINE?
filter(full, 
       TicketNumbers=="LINE")

ggplot(full[-which(is.na(full$Survived)),], aes(x=TicketNumbers, fill=factor(Survived))) +
        geom_bar(position='dodge')+
        labs(x = 'TicketNumbers') +
        theme_minimal()

# that is ugly

### Titles
# use real expressions to pick out titles and names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

table(full$Sex, full$Title)


# Create dummies for these
# Rare titles
rare_title<-c('Capt', 'Col', 'Dona', 'Don', 'Dr', 'Jonkheer', 'Lady', 'Major', 'Rev', 'Sir', 'the Countess')
Sir<-c('Capt', 'Col', 'Don', 'Rev', 'Sir', 'Major')
Lady<-c('Dona', 'Lady', 'the Countess', 'Jonkheer')


# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss'
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% Sir]  <- 'Sir'
full$Title[full$Title %in% Lady]  <- 'Lady'

table(full$Title)

full$Title<-factor(full$Title)

# relationship between title and survival
ggplot(full[-which(is.na(full$Survived)),], aes(x=Title, fill=factor(Survived))) +
        geom_bar(position='dodge')+
        labs(x = 'Title') +
        theme_minimal()


### Families
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[-which(is.na(full$Survived)),], aes(x=Fsize, fill=factor(Survived))) +
        geom_bar(position='dodge')+
        labs(x = 'Family Size') +
        theme_minimal()


# grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family variable 
full$FamilyID <- paste(as.character(full$Fsize), full$Surname, sep="")

full$FamilyID[full$Fsize <= 2] <- 'Small'

sort(table(full$FamilyID))

famIDs<-data.frame(table(full$FamilyID))

# bunch of these individual family members still
famIDs <- famIDs[famIDs$Freq <= 2,]

full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'
full$FamilyID <- factor(full$FamilyID)


# reduce the number of factors in familyID
full$FamilyID2 <- full$FamilyID
full$FamilyID2 <- as.character(full$FamilyID2)
full$FamilyID2[full$Fsize <= 3] <- 'Small'
full$FamilyID2 <- factor(full$FamilyID2)


# look at the sage family
filter(full, 
       Surname=="Sage")

# Make Single Variable
full$Single<-ifelse(full$Fsize=="1", 1, 0)

# discrete family size variable
full$FsizeD[full$Fsize=="1"]<-"single"
full$FsizeD[full$Fsize>1 & full$Fsize<5]<-"small"
full$FsizeD[full$Fsize>4]<-"large"

table(full$FsizeD)

ggplot(full[-which(is.na(full$Survived)),], aes(x=FsizeD, fill=factor(Survived))) +
        geom_bar(position='dodge')+
        labs(x = 'Family Size') +
        theme_minimal()



# 
ggplot(full[-which(is.na(full$Survived)),], aes(x=Deck, fill=factor(TicketNumbers))) +
        geom_bar(position='dodge')+
        labs(x = 'Family Size') +
        theme_minimal()




### Deal with Missingness ###

# most missingness is on Age and Deck; very few missing in Fare, who is missing?
missingids_fare<-full[which(is.na(full$Fare)),]

# examine fares of other passengers sharing his departure
ggplot(filter(full, Pclass==3 & Embarked=='S'), aes(x=Fare))+
        geom_density() +
        geom_vline(aes(xintercept=median(Fare, na.rm=T)))

# check ticketnumbers around this guy
as.numeric(missingids_fare$TicketNumbers)

arrange(filter(full, TicketNumbers %in% seq(3500, 4500)) %>%
                filter(Pclass==3) %>%
                filter(Embarked=='S'), TicketNumbers)

full$Fare[full$PassengerId==missingids_fare$PassengerId]<-median(filter(full, Pclass==3 & Embarked=='S')$Fare, na.rm=T)


# inspect fare
ggplot(full, aes(x=Fare))+
        geom_histogram()

# make discrete Fare variable
full$FareD<- train$FareD <- '30+'
full$FareD[train$Fare < 30 & train$Fare >= 20] <- '20-30'
full$FareD[train$Fare < 20 & train$Fare >= 10] <- '10-20'
full$FareD[train$Fare < 10] <- '<10'

# inspect
aggregate(Survived ~ FareD + Pclass + Sex, data=full, FUN=function(x) {sum(x)/length(x)})

# visualize
ggplot(full[-which(is.na(full$Survived)),], aes(x=FareD, fill=factor(Survived))) +
        geom_bar(position='dodge')+
        labs(x = 'Fare') +
        theme_minimal()


# Embarked has missigness that isn't NAs
missingids_embark<-full[which(full$Embarked==""),]
missingids_embark

embark_fare<- full %>%
        filter(!(PassengerId %in% missingids_embark$PassengerId))
embark_fare


# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
        geom_boxplot() +
        geom_hline(aes(yintercept=median(missingids_embark$Fare)), 
                   colour='red', linetype='dashed', lwd=1) +
        scale_y_continuous(labels=dollar_format()) +
        theme_few()

# check surrounding ticket numbers, sorting by Deck
filter(full, 
       TicketNumbers %in% seq(112000, 115000)) %>%
        filter(Deck=='B') %>%
        arrange(Fare)
# hmm

full$Embarked[missingids_embark$PassengerId]<-'C'
colSums(is.na(full))

# Replace Age using multiple imputation/mice
str(full)

factor_vars<-c('Sex', 'PassengerId', 'Pclass', 'Embarked', 'Surname', 'FsizeD', 'Title')
omit_vars<-c('PassengerId','Name','Ticket','Cabin','FamilyID','Surname','Survived', 'TicketNumbers', 'Single')

# change these vars to factors
full<-as.data.frame(full)
full[factor_vars]<- lapply(full[factor_vars], function(x) as.factor(x))
full<-as.data.table(full)

# Set a random seed
set.seed(1999)
# Perform imputation using mice, excluding the variables to omit
mice_mod<-mice(select(full, -one_of(omit_vars)), method="rf")
mice_output <- complete(mice_mod)


# compare distribution of age before and after imputation
age_orig<-ggplot(full, aes(x=Age)) +
        geom_histogram(color="black", fill=rgb(0.8, 0.2, 0.2, 0.9)) +
        theme_few()+
        ggtitle("Age: Original Data")

age_mice<-ggplot(mice_output, aes(x=Age)) +
        geom_histogram(color="black", fill=rgb(0.6, 0.2, 0.2, 0.5)) +
        theme_few()+
        ggtitle("Age: MICE")

library(gridExtra)
library(grid)
library(lattice)
grid.arrange(age_orig, age_mice, ncol=2)

# replace
full$Age<-mice_output$Age

# survival based on Age
ggplot(full[-which(is.na(full$Survived)),], aes(Age, fill = factor(Survived))) + 
        geom_histogram(bins=30) + 
        facet_grid(.~Sex) + 
        theme_few() +
        guides(fill=guide_legend(title="Survived"))

# add child
full$Child[full$Age>=18]<-0
full$Child[full$Age<18]<-1

# add mother
full$Mother<-0
full$Mother[full$Parch>0 & full$Sex=='female' & full$Age>18 & full$Title!='Miss']<-1   

table(full$Mother, full$Survived)

# quick inspect using Child and Sex
aggregate(Survived ~ Child + Sex, data=full, FUN=function(x) {sum(x)/length(x)})

# 

md.pattern(select(full, -one_of(omit_vars)))


ggplot_missing <- function(x){
        
        x %>% 
                is.na %>%
                melt %>%
                ggplot(data = .,
                       aes(x = Var2,
                           y = Var1)) +
                geom_raster(aes(fill = value)) +
                scale_fill_grey(name = "",
                                labels = c("Present","Missing")) +
                theme_minimal() + 
                theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
                labs(x = "Variables in Dataset",
                     y = "Rows / observations")
}

ggplot_missing(full)


train<-full[which(!is.na(full$Survived)),]
test<-full[which(is.na(full$Survived)),]

# split training set into train and validation set
set.seed(1999)
split1 <- createDataPartition(train$Survived, p = .75)[[1]]
trainDat <- train[split1,]
validDat <- train[-split1,]

# view splits
table(trainDat$Survived)
table(validDat$Survived)


# set up for tuning
ctrl<-trainControl(method="cv",
                   n=10,
                   classProbs=T,
                   #  summaryFunction=fiveStats,
                   verboseIter=T,
                   allowParallel=T,
                   savePredictions="final",
                   selectionFunction="oneSE")

# mod DV
# glm needs the outcome to be a factor
trainDat$Survived[which(trainDat$Survived==0)]<- 'no'
trainDat$Survived[which(trainDat$Survived==1)]<- 'yes'

validDat$Survived[which(validDat$Survived==0)]<- 'no'
validDat$Survived[which(validDat$Survived==1)]<- 'yes'


# features at this point
head(full)
names(full)
omit<-c('Ticket', 'TicketNumbers', 'PassengerId', 'Name', 'Cabin', 'Surname', 'FsizeD', 'FareD', 'Single')

# clean out factors for tree based methods
full_omit<-dplyr::select(full, -one_of(omit))
str(full_omit)

## deal with Family ID
# reduce the number of factors in familyID
full_omit$FamilyID2 <- full_omit$FamilyID
full_omit$FamilyID2 <- as.character(full_omit$FamilyID2)
full_omit$FamilyID2[full_omit$Fsize <= 3] <- 'Small'
full_omit$FamilyID2 <- factor(full_omit$FamilyID2)

# split this back into test and train
train<-full_omit[which(!is.na(full_omit$Survived)),]
test<-full[which(is.na(full$Survived)),]

# change DV
train$Survived[which(train$Survived==0)]<- 'no'
train$Survived[which(train$Survived==1)]<- 'yes'


# 
train_y<-train$Survived
train_x<-as.data.frame(dplyr::select(train, -Survived))

# let's run a decision tree and inspect
set.seed(1999)
train_cart<-suppressWarnings(train(y=factor(train_y),
                                   x=dplyr::select(train_x, -FamilyID2),
                                   trControl=ctrl,
                                   method="rpart"))

library(rpart.plot)
rpart.plot(train_cart$finalModel)

# let's try running a random forest to quickly identify relevant features
set.seed(1999)
train_rf<-suppressWarnings(train(y=factor(train_y),
                                 x=dplyr::select(train_x, -FamilyID),
                                 trControl=ctrl,
                                 tuneLength=5,
                                 method="rf"))

varImp(train_rf$finalModel)

# beef this up to cforest
set.seed(1999)
train_cforest<-suppressWarnings(train(y=factor(train_y),
                                      x=dplyr::select(train_x, -FamilyID2),
                                      trControl=ctrl,
                                      tuneLength=5,
                                      method="cforest"))


# output to kaggle
lean_cforest_0310 <- data.frame(PassengerId = test$PassengerId, Survived = ifelse(predict(train_cforest, test)=='yes', 1, 0))
write.csv(lean_cforest_0310, file = "Submissions/lean_cforest_0310.csv", row.names = FALSE)


# top 3%!

# Visualize variable importance of CForest:
varImp(train_cforest$finalModel)

# bootstrap this to represent uncertainty over this
# register parallel backend
# set tuning parameter
train_cforest$bestTune

vimp_cforest<-
        foreach(j=1:100, .combine=rbind, .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
                
                boot<-sample(1:nrow(train), replace=T)
                train_boot<-train[boot,]
                
                cforest_boot<-suppressWarnings(train(y=train$Survived~.,
                                                     x=dplyr::select(train, -Survived,-FamilyID2),
                                                      trControl=ctrl,
                                                      tuneGrid=expand.grid(.mtry=train_cforest$bestTune[1,]),
                                                      method="cforest"))
                
                hold<-data.frame(variable=rownames(varImp(cforest_boot, scale=F)$importance),
                                 importance=varImp(cforest_boot, scale=F)$importance)
                hold
                
        }

permTab<-vimp_cforest %>%
        dplyr::group_by(Variable = variable) %>%
        dplyr::summarise(mean = mean(Overall),
                         se = sd(Overall)) %>%
        ungroup()

cforestTab<-permTab[order(permTab$Overall), , drop=F]

# plot
y.plot<-seq(1, nrow(cforestTab))
bar<-data.frame(cforestTab, y.plot)
rm(y.plot)

# plot in ggplot
cforestPlot<-ggplot(bar, aes(x=mean, y=y.plot))+
        geom_point(size=1.5)+
        geom_errorbarh(aes(xmin=mean-se, xmax=mean+se), alpha=0.5, height=0, lwd=0.75) +
        coord_cartesian(xlim=c(0, 100),ylim=c(1,length(foo)))+
        scale_y_continuous(breaks = pretty(bar$y.plot, n = length(foo)), labels=bar$Variable)+        
        labs(x="Importance")+
        labs(y="")+
        ggtitle("Conditional Random Forest")+
        geom_vline(xintercept = 0, "x")+
        theme_bw()
cforestPlot


# repeat for Ranger



# let's try an elastic net with the same oneSE rule given this subset of features
set.seed(1999)
train_glmnet<-suppressWarnings(train(Survived~.,
                                      data=select(train, -FamilyID2),
                                      trControl=ctrl,
                                      tuneLength=10,
                                      method="glmnet"))

# output to kaggle
lean_glmnet_0311 <- data.frame(PassengerId = test$PassengerId, Survived = ifelse(predict(train_glmnet, test)=='yes', 1, 0))
write.csv(lean_glmnet_0311, file = "Submissions/lean_glmnet_0311.csv", row.names = FALSE)
# this does pretty well too, into the top 10% but not quite as good as cforest

# Let's inspect what these models are predicting
# bind into frame with other features for ggplot
g<-cbind.data.frame(predict(train_cforest, test, type="prob")[,2], 
                    predict(train_glmnet, test, type="prob")[,2],
                    test)
colnames(g)[c(1,2)]<-c("cforest", "glmnet")


# Compare predictions for each individual by the two models
ggplot(g, aes(x=cforest, y=glmnet, color=Pclass, shape=Sex))+
        geom_point(alpha=0.75, size=2)+
        xlab("CForest Pr(Survive)")+
        ylab("Elastic Net Pr(Surive)")+
        geom_hline(aes(yintercept=0.5), linetype="dashed", size=0.5)+
        geom_vline(aes(xintercept=0.5), linetype="dashed", size=0.5)+
        annotate("text", x = 0.25, y = 0.75, label = paste("corr", round(cor(g$cforest, g$glmnet), 3), sep="="))+
        theme_few()

plot_cforest<-ggplot(g, aes(x=as.numeric(PassengerId), y=cforest, color=Pclass, shape=Sex))+
        geom_point(alpha=0.75, size=3)+
        geom_hline(aes(yintercept=0.5))+
        xlab("PassngerId")+
        ylab("Pr(Surive)")+
        ggtitle("Conditional Random Forest")+
        theme_few()

plot_glmnet<-ggplot(g, aes(x=as.numeric(PassengerId), y=glmnet, color=Pclass, shape=Sex))+
        geom_point(alpha=0.75, size=3)+
        geom_hline(aes(yintercept=0.5))+
        xlab("PassngerId")+
        ylab("Pr(Surive)")+
        ggtitle("Elastic Net")+
        theme_few()

grid.arrange(plot_cforest, plot_glmnet, ncol=1)


## Let's run an ensemble
# Let's get a set of models which all treat the data slightly differently. 
# Indivudally these models not do well, but when combined do better than any individual model

# in order to train the ensemble, we will need to split our training set, evaluating on a separate hold out set
set.seed(1999)
index <- createDataPartition(train$Survived, p=0.75, list=FALSE)
t_train <- train[ index,]
v_train<- train[-index,]

# create dfs for some algorithms
t_train_y<-t_train$Survived
t_train_x<-dplyr::select(t_train, -Survived)

# Models for stacking
# KNN, Naive Bayes, CART, Elastic Net, SVMRadial, Ranger, CForest


# set up for tuning
ctrl<-trainControl(method="cv",
                   n=10,
                   classProbs=T,
                   #  summaryFunction=fiveStats,
                   verboseIter=T,
                   allowParallel=T,
                   savePredictions="final",
                   selectionFunction="oneSE")

# KNN
set.seed(1999)
stack_knn<-suppressWarnings(train(Survived~.,
                                     data=dplyr::select(t_train, -FamilyID2),
                                     trControl=ctrl,
                                     tuneLength=10,
                                     method="knn", 
                                  preProcess=c("center", "scale")))

# Elastic Net
set.seed(1999)
stack_glmnet<-suppressWarnings(train(Survived~.,
                                     data=dplyr::select(t_train, -FamilyID2),
                                     trControl=ctrl,
                                     tuneLength=10,
                                     method="glmnet"))

# Boosted Logit
set.seed(1999)
stack_LogitBoost<-suppressWarnings(train(Survived~.,
                                     data=dplyr::select(t_train, -FamilyID2),
                                     trControl=ctrl,
                                     tuneLength=50,
                                     method="LogitBoost"))

# CART
set.seed(1999)
stack_cart<-suppressWarnings(train(Survived~.,
                                     data=dplyr::select(t_train, -FamilyID2),
                                     trControl=ctrl,
                                     tuneLength=10,
                                     method="rpart"))

# boosted trees
set.seed(1999)
stack_gbm<-suppressWarnings(train(y=t_train_y,
                                      x=dplyr::select(t_train_x, -FamilyID),
                                      trControl=ctrl,
                                      tuneLength=5,
                                      method="gbm"))


# ranger
set.seed(1999)
stack_ranger<-suppressWarnings(train(y=t_train_y,
                                 x=dplyr::select(t_train_x, -FamilyID),
                                 trControl=ctrl,
                                 tuneLength=5,
                                 importance="permutation",
                                 method="ranger"))

# Cforest
set.seed(1999)
stack_cforest<-suppressWarnings(train(y=t_train_y,
                                     x=dplyr::select(t_train_x, -FamilyID),
                                     trControl=ctrl,
                                     tuneLength=5,
                                     method="cforest"))

# SVM Radial
set.seed(1999)
stack_svm<-suppressWarnings(train(Survived~.,
                                   data=dplyr::select(t_train, -FamilyID2),
                                   trControl=ctrl,
                                   tuneLength=5,
                                   method="svmRadialWeights",
                                   preProcess=c("center", "scale")))


# gather models into a list
stack_models<-lapply(ls(pattern="stack_"), get)

# Store oos predictions
# then, use these predictions in a glm of the outcome variable in caret

# Grab the outcome from the training CV
cv_obs<-as.matrix((stack_models[[1]]$pred) %>%
                          arrange(rowIndex) %>%
                          dplyr::select(obs))

### grab out of sample predictions from the stacking CV
cv_preds<-as.tbl(foreach(i=1:length(stack_models), .combine=cbind.data.frame) %do% {
        
        bar <- as.tbl(stack_models[[i]]$pred) %>% 
                arrange(rowIndex) %>% 
                dplyr::select(yes)
        
        colnames(bar)<-stack_models[[i]]$method
        bar
        
})


# functions for evaluating performance
# log loss function
myLL<-function(y,p)
        mean(-1 * (y=="yes") * log(p + .Machine$double.eps) - 
                     (y=="no") * log(1-p+.Machine$double.eps))

myRate <- function(p, y, cut){
        
        cut<-cut
        
        TP <- sum(p >= cut & y == "yes")
        TN <- sum(p < cut  & y == "no")
        FP <- sum(p >= cut & y == "no")
        FN <- sum(p < cut  & y == "yes")
        
        results <- vector(mode = "list")
        results$sens <- TP / (TP + FN)
        results$spec <- TP / (TP + FP)
        results$npv <- TN / (TN + FN)
        results$prec <- TP / (TP + FP)
        results$conf <- matrix(c(TP, FN, FP, TN), ncol = 2, nrow = 2, byrow = T)
        results$acc <- (TP + TN) / (TP + TN + FP + FN)
        results$logLoss <- myLL(y = y, p = p)
        results$auc <- as.numeric(pROC::auc(response = as.vector(y), predictor = p))
        results
}

getResults <- function(p, y, cut){
        
        foo <- myRate(p = p, y = y, cut = cut)
        data.frame(logLoss = foo$logLoss,
                   auc = foo$auc,
                   accuracy = foo$acc,
                   sensitivity = foo$sens,
                   specificity = foo$spec,
                   precision = foo$prec)
}

# get results
foo_results<-do.call(rbind, apply(cv_preds, 2, getResults, y=cv_obs, cut=0.5))
cv_results<-as.tbl(cbind.data.frame(data.frame(model=names(cv_preds)), foo_results))

arrange(cv_results, accuracy)


# Examine correlations between models
round(cor(cv_preds), 3)

# quick plot of that
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        # correlation coefficient
        r <- cor(x, y)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste("r= ", txt, sep = "")
        text(0.5, 0.6, txt)
}

# plot
pairs(cv_preds, upper.panel = panel.cor, cex=0.75)



# Ensemble
# feed the predictions into a glm of the actual outcome
# cross validate
train_stack<-data.table(cv_preds, cv_obs)

# GLM
set.seed(1999)
ensemble_glm<-suppressWarnings(train(obs~.,
                                     data=train_stack,
                                     trControl=ctrl,
                                     tuneLength=5,
                                     method="glm"))

# GBM
set.seed(1999)
ensemble_gbm<-suppressWarnings(train(obs~.,
                                     data=train_stack,
                                     trControl=ctrl,
                                     tuneLength=5,
                                     method="gbm"))

models_ensemble<-lapply(ls(pattern="ensemble_"), get)

# predict the holdout set
v_preds<-foreach(i=1:length(stack_models), .combine=cbind) %do% {
        
        foo<-as.matrix(predict.train(stack_models[[i]], v_train, type="prob")[,2])
        colnames(foo)<-stack_models[[i]]$method
        foo
}

rm(foo)

v_ensemble<-foreach(i=1:length(models_ensemble), .combine=cbind) %do% {
        hold<-predict(models_ensemble[[i]], v_preds, type="prob")[,2]
        hold
}
colnames(v_ensemble)<-c("ensemble_glm", "ensemble_gbm")

# bind into predictions with others
v_final<-cbind.data.frame(v_preds, v_ensemble)

# get results with 0.5 threshold
foo_results<-do.call(rbind, apply(v_final, 2, getResults, y=v_train$Survived, cut=0.5))
v_results<-as.tbl(cbind.data.frame(data.frame(model=names(v_final)), foo_results))

arrange(v_results, accuracy)

# examine accuracy over different cutpoints
cuts<-seq(0, 1, 0.02)

# this is really annoying
do.call(rbind, apply(v_final, 2, getResults, y=cv_obs, cut=0.5))


cuts_results<-foreach(i=1:ncol(v_final), .combine=cbind) %do% {
        hold<-foreach(j=1:length(cuts), .combine=rbind) %do% {
                foo<-getResults(v_final[,i], y=v_train$Survived, cut=cuts[j])
                out<-select(foo, accuracy)
                colnames(out)<-c(colnames(v_final[i]))
                out
        }
        hold
}

cuts_accuracy<-cbind(cuts, cuts_results)

# what cutpoint maximizes the accuracy of the Ensembled GBM? Ensemble GLM?
cut_gbm<-data.frame(cuts = cuts_accuracy[which(cuts_accuracy$ensemble_gbm==max(cuts_accuracy$ensemble_gbm)),]$cuts,
                    accuracy=max(cuts_accuracy$ensemble_gbm))

cut_glm<-data.frame(cuts = cuts_accuracy[which(cuts_accuracy$ensemble_glm==max(cuts_accuracy$ensemble_glm)),]$cuts,
                    accuracy=max(cuts_accuracy$ensemble_glm))

g1<-ggplot(cuts_accuracy, aes(x=cuts, y=ensemble_gbm)) +
        geom_line()+
        labs(y="Accuracy", x="Cutpoint")+
        ggtitle("Ensemble - GBM")+
        geom_vline(xintercept=cut_gbm[1,1])+
        theme_bw()

g2<-ggplot(cuts_accuracy, aes(x=cuts, y=ensemble_glm)) +
        geom_line()+
        labs(y="Accuracy", x="Cutpoint")+
        ggtitle("Ensemble - GLM")+
        geom_vline(xintercept=cut_glm[1,1])+
        theme_bw()

grid.arrange(g1, g2, ncol=1)


# refit all models to entirety of training data
# KNN
set.seed(1999)
final_knn<-suppressWarnings(train(Survived~.,
                                  data=dplyr::select(train, -FamilyID2),
                                  trControl=ctrl,
                                  tuneLength=10,
                                  method="knn", 
                                  preProcess=c("center", "scale")))

# Elastic Net
set.seed(1999)
final_glmnet<-suppressWarnings(train(Survived~.,
                                     data=dplyr::select(train, -FamilyID2),
                                     trControl=ctrl,
                                     tuneLength=10,
                                     method="glmnet"))

# Boosted Logit
set.seed(1999)
final_LogitBoost<-suppressWarnings(train(Survived~.,
                                         data=dplyr::select(train, -FamilyID2),
                                         trControl=ctrl,
                                         tuneLength=50,
                                         method="LogitBoost"))

# CART
set.seed(1999)
final_cart<-suppressWarnings(train(Survived~.,
                                   data=dplyr::select(train, -FamilyID2),
                                   trControl=ctrl,
                                   tuneLength=10,
                                   method="rpart"))

# boosted trees
set.seed(1999)
final_gbm<-suppressWarnings(train(y=train_y,
                                  x=dplyr::select(train_x, -FamilyID),
                                  trControl=ctrl,
                                  tuneLength=5,
                                  method="gbm"))


# ranger
set.seed(1999)
final_ranger<-suppressWarnings(train(y=train_y,
                                     x=dplyr::select(train_x, -FamilyID),
                                     trControl=ctrl,
                                     tuneLength=5,
                                     importance="permutation",
                                     method="ranger"))

# Cforest
set.seed(1999)
final_cforest<-suppressWarnings(train(y=train_y,
                                      x=dplyr::select(train_x, -FamilyID),
                                      trControl=ctrl,
                                      tuneLength=5,
                                      method="cforest"))

# SVM Radial
set.seed(1999)
final_svm<-suppressWarnings(train(Survived~.,
                                  data=dplyr::select(train, -FamilyID2),
                                  trControl=ctrl,
                                  tuneLength=5,
                                  method="svmRadialWeights",
                                  preProcess=c("center", "scale")))


# gather models into a list
final_models<-lapply(ls(pattern="final_"), get)

# turn into ensemble
# Grab the outcome from the training CV
final_obs<-as.matrix((final_models[[1]]$pred) %>%
                          arrange(rowIndex) %>%
                          dplyr::select(obs))

### grab out of sample predictions from the stacking CV
final_preds<-as.tbl(foreach(i=1:length(final_models), .combine=cbind.data.frame) %do% {
        
        bar <- as.tbl(final_models[[i]]$pred) %>% 
                arrange(rowIndex) %>% 
                dplyr::select(yes)
        
        colnames(bar)<-final_models[[i]]$method
        bar
        
})

# run ensemble models
final_stack<-data.table(final_preds, final_obs)

# GLM
set.seed(1999)
fensemble_glm<-suppressWarnings(train(obs~.,
                                     data=final_stack,
                                     trControl=ctrl,
                                     tuneLength=5,
                                     method="glm"))

# GBM
set.seed(1999)
fensemble_gbm<-suppressWarnings(train(obs~.,
                                     data=final_stack,
                                     trControl=ctrl,
                                     tuneLength=5,
                                     method="gbm"))

fensemble_models<-lapply(ls(pattern="fensemble_"), get)



# predict the test set
test_preds<-foreach(i=1:length(final_models), .combine=cbind) %do% {
        
        foo<-as.matrix(predict.train(final_models[[i]], test, type="prob")[,2])
        colnames(foo)<-final_models[[i]]$method
        foo
}

test_ensemble<-foreach(i=1:length(fensemble_models), .combine=cbind) %do% {
        hold<-predict(fensemble_models[[i]], test_preds, type="prob")[,2]
        hold
}
colnames(test_ensemble)<-c("ensemble_glm", "ensemble_gbm")


# predict with ensemble
test_ensemble<-as.tbl(foreach(i=1:length(models_ensemble), .combine=cbind.data.frame) %do% {
        hold<-predict.train(models_ensemble[[i]], test_preds, type="prob")[,2]
        hold
})
colnames(test_ensemble)<-c("ensemble_glm", "ensemble_gbm")


# output with cutpoints as determined by holdout set
ensemble_glm_0312<-data.frame(PassengerId = test$PassengerId, Survived = ifelse(test_ensemble$ensemble_glm > cut_glm[1,1], 1, 0))
write.csv(ensemble_glm_0312, file = "Submissions/ensemble_glm_0312.csv", row.names = FALSE)


# output with cutpoints as determined by holdout set
ensemble_gbm_0312<-data.frame(PassengerId = test$PassengerId, Survived = ifelse(test_ensemble$ensemble_gbm > cut_gbm[1,1], 1, 0))
write.csv(ensemble_gbm_0312, file = "Submissions/ensemble_gbm_0312.csv", row.names = FALSE)

# ensembling doing okay, but not quite as well as cforest
# why is that?

# well, it's possible that this is just because we have similar, stable learners;
# ultimately, we would like it if we had models which were not correlated at all


