##Author: Ross Miglin
##UTEID: rdm2965
##Cannata's Data Analytics Class


library(class)
require(data.world)

project <- "https://data.world/jlee/f-17-eda-project-2"


df <- data.world::query(
  data.world::qry_sql("SELECT * FROM heartdisease limit 1000"),
  dataset = project
)
df$num[df$num > 0] <- 1
summary(df)
attach(df)

qda(num~age+trestbps+chol+thalach+oldpeak,data=df,family=binomial)
