##Project 3
## Author: Juan Gomez


####################################### Requiring Data ##########################################
require(data.world)

project <- "https://data.world/juandi/f-17-edaproject-3"

df <- data.world::query(
  data.world::qry_sql("SELECT * FROM electiondata"),
  dataset = project
)


##################################### Initilizing Session #######################################

library(class)
library(glmnet)
library(leaps)


##################################### K-Nearest Neighbors 1 #####################################

# Setting training data
train = df$votes > 1000000

## Test 1: Using the white population percentage and unemployment population percentage

#Binding predictors
test1 = cbind(df$white_pop,df$unemployment)

#Running KNN test
knn.pred1 = knn(test1[train,], test1[!train,], df$candidate[train], k=1)

#Confusion matrix and mean
table(knn.pred1,df$candidate[!train])
mean(knn.pred1==df$candidate[!train])


## Test 2: Using the white population percentage and unemployment population percentage
test2 = cbind(df$latino_pop,df$management_occup)

#Running KNN test
knn.pred1 = knn(test2[train,], test2[!train,], df$candidate[train], k=1)

#Confusion matrix and mean
table(knn.pred1,df$candidate[!train])
mean(knn.pred1==df$candidate[!train])


###################################### Model Selection ##########################################

## Redefining data from categorical to numerical to correctly do analysis
# Modifying candidate results to numerical representation
df$candidate[df$candidate == "Trump"] <- 1
df$candidate[df$candidate == "Clinton"] <- 0

# Dropping state cloum
drops <- c("state")
df <- df[ , !(names(df) %in% drops)]


##Best subset regression
#RUnning best subset regression and saving results
regfit.full=suppressWarnings(regsubsets(candidate~.,data=df, nvmax= 22))
reg.summary=summary(regfit.full)

#Plotting results
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
min = which.min(reg.summary$cp)
points(min,reg.summary$cp[min],pch=20,col="red")

#Coefficient Summary
coef(regfit.full,min)

##Best subset regression with forward stepwise selection
#Running best subset regression and saving results
regfit.fwd=suppressWarnings(regsubsets(candidate~.,data=df, nvmax= 22, method = "forward"))
reg.summary=summary(regfit.fwd)

#Plotting results
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
min = which.min(reg.summary$cp)
points(min,reg.summary$cp[min],pch=20,col="red")

#Coefficient Summary
coef(regfit.full,min)


##################################### K-Nearest Neighbors 2 #####################################

#Changing data back to original for proper analysis
df$candidate[df$candidate == 1] <- "Trump"
df$candidate[df$candidate == 0] <- "Clinton"


# Test 3: Using the four best predictors according to best subset regression
test3 = cbind(df$pct_rep,df$less_than_high_school,df$asian_american_pop,df$unemployment)

#Running KNN test
knn.pred3 = knn(test3[train,], test3[!train,], df$candidate[train], k=1)

#Confusion matrix and mean
table(knn.pred3,df$candidate[!train])
mean(knn.pred3==df$candidate[!train])

