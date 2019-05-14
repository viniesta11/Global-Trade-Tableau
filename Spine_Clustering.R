getwd()
library(cluster)
library(mclust)
health<-read.csv("Dataset_spine.csv")
View(health)
summary(health)
names(health)
str(health)

####Scaling the data#####
health_scaled<-scale(health)
View(health_scaled)
summary(health_scaled)

####Within sum of squares calculation####
wss<-(nrow(health_scaled)-1)*sum(apply(health_scaled,2,var))
for (i in 2:15) {
  wss[i]=sum(kmeans(Health_data.scale,centers = i)$withinss)  
}
set.seed(15)

####Scree plot####
plot(1:15,wss,type = "b",xlab = "No of clusters",ylab = "Within Group Sum of Squares")

####Using kmeans####
Health_Cluster <-kmeans(health_scaled,2)
Health_Cluster
summary(Health_Cluster)
health1<-cbind.data.frame(Health_data,cluster = Health_Cluster$cluster)
View(health1)


####Silhoutte distance calculation####
dis<-daisy(health_scaled)
s_distance<-silhouette(health1$cluster,dis)
s_distance
plot(s_distance)
Health_Cluster$cluster
Health_Cluster$centers
Health_Cluster$totss
Health_Cluster$withinss
Health_Cluster$tot.withinss
Health_Cluster$betweenss
Health_Cluster$size
Health_Cluster$iter
names(Health_Cluster)
View(health1)
names(health1)
names(health1)[13]<-"cluster"
View(health1)
health2<-cbind.data.frame(health1,class = health1$cluster)
View(health2)
health3<-cbind(health,Health_Cluster$cluster)
names(health3)[13]<-"class"
View(health4)
View(health3)
health4<-health3
health4$class[4]
x <- health4$class
c=1
for (val in x) {
  
  if(val == 1)
  {
    health4$class[c] <- "Abnormal"
  }
  else
  {
    health4$class[c] <- "Normal"
  }
  c=c+1
}
View(health4)
health_final<-health4

#####Prediction using Logistic Regression####
summary(health_final)

####Handling the outlier####
boxplot.stats(health_final$pelvic_incidence)
boxplot(health_final$pelvic_incidence)
health_final$pelvic_incidence<-ifelse(health_final$pelvic_incidence>96.65732,96.65732,health_final$pelvic_incidence)

boxplot.stats(health_final$pelvic_tilt)
boxplot(health_final$pelvic_tilt)
health_final$pelvic_tilt<-ifelse(health_final$pelvic_tilt>38.750670,38.750670,health_final$pelvic_tilt)

boxplot.stats(health_final$lumbar_lordosis_angle)
boxplot(health_final$lumbar_lordosis_angle)
health_final$ lumbar_lordosis_angle<-ifelse(health_final$ lumbar_lordosis_angle>100.7442,100.7442,health_final$ lumbar_lordosis_angle)

boxplot.stats(health_final$ sacral_slope)
boxplot(health_final$ sacral_slope)
health_final$ sacral_slope<-ifelse(health_final$ sacral_slope>79.6951,79.6951,health_final$ sacral_slope)

boxplot.stats(health_final$ pelvic_radius)
boxplot(health_final$ pelvic_radius)
health_final$ pelvic_radius<-ifelse(health_final$ pelvic_radius>146.46600,146.46600,health_final$ pelvic_radius)
health_final$ pelvic_radius<-ifelse(health_final$ pelvic_radius<89.30755,89.30755,health_final$ pelvic_radius)

boxplot.stats(health_final$ degree_spondylolisthesis)
boxplot(health_final$ degree_spondylolisthesis)
health_final$ degree_spondylolisthesis<-ifelse(health_final$ degree_spondylolisthesis>100.292107,100.292107,health_final$ degree_spondylolisthesis)

boxplot.stats(health_final$ pelvic_slope)
boxplot(health_final$ pelvic_slope)

boxplot.stats(health_final$ Direct_tilt)
boxplot(health_final$ Direct_tilt)

boxplot.stats(health_final$ thoracic_slope)
boxplot(health_final$ thoracic_slope)

boxplot.stats(health_final$ cervical_tilt)
boxplot(health_final$ cervical_tilt)

boxplot.stats(health_final$ sacrum_angle)
boxplot(health_final$ sacrum_angle)

boxplot.stats(health_final$ scoliosis_slope)
boxplot(health_final$ scoliosis_slope)

str(health_final)
View(health_final)

####Creating Train and Test data####
train_index<-sample(1:nrow(health_final),0.75*nrow(health_final))

train_data<-health_final[train_index,]
test_data<-health_final[-train_index,]

model_health<-glm(as.factor(class)~.,family = "binomial",train_data)
summary(model_health)
step(model_health)
model_health1<-glm(formula = as.factor(class) ~ pelvic_incidence + lumbar_lordosis_angle + 
      degree_spondylolisthesis, family = "binomial", data = train_data)
summary(model_health1)
test_data$predicted_class<-predict(model_health1,newdata=test_data,type="response")
View(test_data)
test_data$predicted_score <- ifelse(test_data$predicted_class > 0.5,"Normal","Abnormal")
View(test_data)
install.packages("plotly")
library(e1071)
library(ROCR)
library(plotROC)
library(plotly)
library(caret)
library(InformationValue)

test_data$class1<-ifelse(test_data$class=="Normal",1,0)
test_data$predicted_score1<-ifelse(test_data$predicted_score=="Normal",1,0)
confusionMatrix(test_data$class1,test_data$predicted_score1)
plotROC(test_data$class1,test_data$predicted_score1)
ks_stat(test_data$class1,test_data$predicted_score1)