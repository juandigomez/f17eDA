##Author: Ross Miglin
##UTEID: rdm2965
##Cannata's Data Analytics Class


library(class)
require(data.world)
require(MASS)
require(ISLR)

project <- "https://data.world/jlee/f-17-eda-project-2"


df <- data.world::query(
  data.world::qry_sql("SELECT * FROM heartdisease limit 1000"),
  dataset = project
)
df$num[df$num > 0] <- 1
summary(df)
attach(df)

qda.fit = qda(num~age+trestbps+chol+thalach+oldpeak,data=df,family=binomial)
qda.fit
df.282 = subset(df,chol==282)
qda.class = predict(qda.fit, df.282)
table(qda.class$class,df.282$num)
mean(qda.class$class==df.282$num)

