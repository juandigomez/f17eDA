require(data.world)

project <- "https://data.world/juandi/f-17-eda-project-4"
data.world::set_config(cfg_env("DW_API"))
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM electiondata"),
  dataset = project
)

attach(df)
require(tree)
require(randomForest)
require(gbm)

# Setting up data
result = df$candidate

df$candidate[df$candidate == "Trump"] <- 1
df$candidate[df$candidate == "Clinton"] <- 0

drops <- c("state")
df <- df[ , !(names(df) %in% drops)]

df$candidate = as.numeric(df$candidate)
df$candidate = as.numeric(df$votes)

#Setting up data frame for decision tree
df=data.frame(df, result)

# Decision tree
tree.candidate=tree(result~.,df)
summary(tree.candidate)
plot(tree.candidate)
text(tree.candidate,pretty=0)

# Random Forest
set.seed(101)
train=sample(1:nrow(df),20)

rf.boston=randomForest(result~.-candidate,data=df,subset=train)
rf.boston

set.seed(101)
train=sample(1:nrow(df),30)

rf.boston=randomForest(result~.-candidate,data=df,subset=train)
rf.boston

set.seed(101)
train=sample(1:nrow(df),45)

rf.boston=randomForest(result~.-candidate,data=df,subset=train)
rf.boston

#Boosting
boost.election=gbm(result~.,data=df[train,],distribution="gaussian",n.trees=1000,shrinkage=0.001,interaction.depth=2)
summary(boost.election)

boost.election=gbm(result~.,data=df[train,],distribution="gaussian",n.trees=100,shrinkage=1,interaction.depth=3)
summary(boost.election)

boost.election=gbm(result~.,data=df[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.election)

plot(boost.election,i="graduates_degree")
plot(boost.election,i="asian_american_pop")

