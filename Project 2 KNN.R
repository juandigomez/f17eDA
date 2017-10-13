## Author: Juan Gomez

require(data.world)

project <- "https://data.world/jlee/f-17-eda-project-2"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM heartdisease"),
  dataset = project
)
df$num[df$num > 0] <- 1

attach(df)
library(class)

xlag = cbind(chol,trestbps)
train = age>50

knn.pred = knn(xlag[train,], xlag[!train,], num[train], k=1)
table(table(knn.pred,num[!train])
mean(knn.pred==num[!train])
