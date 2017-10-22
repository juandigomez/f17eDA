## Author: Juan Gomez

require(data.world)

project <- "https://data.world/jlee/f-17-eda-project-2"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM heartdisease"),
  dataset = project
)

# Modyfying data to calculate results with K nearest neighbors
df$num[df$num > 0] <- 1

df$chestpain[df$chestpain == "typical angina"] <- 1
df$chestpain[df$chestpain == "atypical angina"] <- 2
df$chestpain[df$chestpain == "non-anginal pain"] <- 3
df$chestpain[df$chestpain == "asymptomatic"] <- 3

df$sex[df$sex == "male"] <- 1
df$sex[df$sex == "female"] <- 0

df$fbs[df$fbs == "true"] <- 1
df$fbs[df$fbs == "false"] <- 0

# Attaching modified data and initiaing 
attach(df)
library(class)

# Establishing training data
train = age > 50

# Test 1: Using Cholester and sex to determine heart disease
test1 = cbind(chol,sex)

knn.pred1 = knn(test1[train,], test1[!train,], num[train], k=1)
table(knn.pred1,num[!train])
mean(knn.pred1==num[!train])

# Test 2: Using depression (oldpeak) and fasting blood sugar to determine heart disease
test2 = cbind(oldpeak,fbs)

knn.pred2 = knn(test2[train,], test2[!train,], num[train], k=1)
table(knn.pred2,num[!train])
mean(knn.pred2==num[!train])

# Test 3: Using resting blood preassure and chest pains to determine heart disease
test3 = cbind(trestbps,chestpain)

knn.pred3 = knn(test3[train,], test3[!train,], num[train], k=1)
table(knn.pred3,num[!train])
mean(knn.pred3==num[!train])

# CHanging K values for best test case
knn.predk3 = knn(test2[train,], test2[!train,], num[train], k=3)
table(knn.predk3,num[!train])
mean(knn.predk3==num[!train])

knn.predk5 = knn(test2[train,], test2[!train,], num[train], k=5)
table(knn.predk5,num[!train])
mean(knn.predk5==num[!train])

knn.predk7 = knn(test2[train,], test2[!train,], num[train], k=7)
table(knn.predk7,num[!train])
mean(knn.predk7==num[!train])

knn.predk9 = knn(test2[train,], test2[!train,], num[train], k=9)
table(knn.predk9,num[!train])
mean(knn.predk9==num[!train])
