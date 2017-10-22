project <- "https://data.world/jlee/f-17-eda-project-2"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM heartdisease"),
  dataset = project
)
summary(df)
attach(df)

df$num[df$num > 0] <- 1

## Project 2 Linear Discriminant Analysis 
require(MASS)

boxplot(age~df$num, main="Heart Disease by Age", ylab="Age", xlab="Heart Disease")
mosaicplot(sex~df$num, main="Heart Disease by Gender", shade=FALSE, color=TRUE, xlab="Gender", ylab="Heart Disease")

Heart=lda(df$num~chol+thalach+trestbps+oldpeak,data=df,subset=age<=55)
Heart
train = subset(df,age>55)
lda.pred=predict(Heart,train)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
dflda = data.frame(lda.pred)
ggplot(dflda) + geom_histogram(mapping = aes(x=LD1), color="blue") + facet_wrap(~class)
ggplot(dflda) + geom_boxplot(mapping = aes(x=class, y=LD1), color = "blue") 
table(lda.pred$class,train$num)
mean(lda.pred$class==train$num)