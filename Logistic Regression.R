##Author: Patrick Lyons
##UTEID: PAL869
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

glm.fit=glm(num~age+trestbps+chol+thalach+oldpeak,data=df,family=binomial)

summary(glm.fit)

glm.probs = predict(glm.fit,type="response")
glm.probs[1:5]

glm.pred=ifelse(glm.probs>0.5,1,0)
#will create vector of trues and falses; for each element, if it's greater than .5 we call it heart disease
attach(df) #allows us to use variables by name
table(glm.pred,num) #allows us to compare our predictions to the actual data
mean(glm.pred==num) #classification performance: gives us proportion

plot(oldpeak,num,xlab="Oldpeak",ylab="Probability of Heart Disease")
g=glm(num~oldpeak,data=df,family=binomial)
curve(predict(g,data.frame(oldpeak=x),type="resp"),add=TRUE)
points(oldpeak,fitted(g),pch=20)

plot(thalach,num,xlab="Maximum Heartrate Achieved",ylab="Probability of Heart Disease")
g=glm(num~thalach,data=df,family=binomial)
curve(predict(g,data.frame(thalach=x),type="resp"),add=TRUE)
points(thalach,fitted(g),pch=20)
