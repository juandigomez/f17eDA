## Author: Juan Gomez

require(data.world)

project <- "https://data.world/juandi/f-17-edaproject-3"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM ElectionData"),
  dataset = project
)
summary(df)
#Important Class library to perfor K-Nearest Neighbors
library(class)

# Setting training data
train = df$votes > 1000000

# Test 1: Using the white population percentage and unemployment population percentage
test1 = cbind(df$white_pop,df$unemployment)

knn.pred1 = knn(test1[train,], test1[!train,], df$candidate[train], k=1)
table(knn.pred1,df$candidate[!train])
mean(knn.pred1==df$candidate[!train])

# Test 2: Using the white population percentage and unemployment population percentage
test2 = cbind(df$latino_pop,df$management_occup)

knn.pred1 = knn(test2[train,], test2[!train,], df$candidate[train], k=1)
table(knn.pred1,df$candidate[!train])
mean(knn.pred1==df$candidate[!train])


