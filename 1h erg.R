####################################Exercise 1####################################

print(getwd())
#������ ��� path ���� ������������ ���� �������, ��� ������� ��� ������� GroceriesInitial.csv
setwd("C:/Users/Lina/Desktop/faxat")
print(getwd())

original <- read.csv(file = 'GroceriesInitial.csv')



#���������� ������ ��� ���������������� basket values ��� ������������ ����
original$low_value_basket <- FALSE
original$medium_value_basket <- FALSE
original$high_value_basket <- FALSE

#������� ��������������� ��� basket value
summary(original$basket_value)

#��������� basket value �� ���������� ���� ���� ��� string ��� ������� ���������
original$basket_value <- as.numeric(original$basket_value)

#���������� ����������������� basket value ��� ���� �������
for (row in 1:nrow(original)) {
  if(original$basket_value[row] < 4.0){
    original$low_value_basket[row] <- TRUE
  }
  else if(original$basket_value[row] < 10.0){
    original$medium_value_basket[row] <- TRUE
  }
  else{
    original$high_value_basket[row] <- TRUE
  }
}


#������� ��������� �� ���������� R
#���������� ������ �� ������� ��������� ��� ��� �����������
product_names <- c("citrus fruit", "tropical fruit", "whole milk", "other vegetables", "rolls/buns", "chocolate", "bottled water", "yogurt",
                   "sausage", "root vegetables", "pastry", "soda", "cream")

#��������� ��������� �� dataframe �������� ������, ��� �� �������� ��� ������, ������� �� ��� �������� ���� ��� original dataframe
products <- as.data.frame(t(apply(original[,4:35],1, 
                                  function(x)
                                    (product_names) %in% as.character(unlist(x)))))

#������� �������� ��� ������-���������������
names(products) <- product_names
#��������� ��� ����������� ������ �� ������ ������ ��� original ��� ��� �����������, ��� �������� ��� ������� mydata �� �� �������������� ���� ����� ��� ������ � ������.
mydata <- cbind(original$id, original$recency_days, original$basket_value, original$low_value_basket, original$medium_value_basket, original$high_value_basket,products)

summary(mydata)




####################################Exersise 2####################################

library(arules)
#Apriori ���� ��� �� ��������
apr = apriori(data = mydata[, 7:ncol(mydata)], parameter = list(supp = 0.001, confidence = 0.8))
summary(apr)
inspect(sort(apr, by = 'confidence')[1:20])


#Apriori ��� �������� ��� ���������������� basket_value
apr = apriori(data = mydata[, 4:ncol(mydata)], parameter = list(supp = 0.002, confidence = 1))
inspect(sort(apr, by = 'confidence')[1:20])




####################################Exercise 3####################################

#Kmeans ��� ��  �������������� 2(recency_days) ��� 3(basket_value)
kmeans_res <- kmeans(mydata[, 2:3], centers=5)
kmeans_res

#������������ ��� �������������.
library(factoextra)
library(ggplot2)
fviz_cluster(kmeans_res, data = mydata[, 2:3],axis = c(2,3),  geom = c("point"))
plot(mydata[,2:3], col = kmeans_res$cluster, main="K-Means results")


#���������� ��������������� cluster ��� �������� ��� ���� ��� ������ ���� ������ � �������
mydata$cluster <-as.integer(kmeans_res$cluster) 

#������������ ��� 5 ���������� ��������������� ��� clusters
mydata$Cluster1 <- FALSE
mydata$Cluster2 <- FALSE
mydata$Cluster3 <- FALSE
mydata$Cluster4 <- FALSE
mydata$Cluster5 <- FALSE

#������� �� ��� ���� ��� cluster, energopo;ihsh toy ����������� cluster1 - cluster5 ��� ���� �������
for (row in 1:nrow(mydata)) {
  if(mydata$cluster[row] == 1){
    mydata$Cluster1[row] <- TRUE
  }
  else if(mydata$cluster[row] == 2){
    mydata$Cluster2[row] <- TRUE
  }
  else if(mydata$cluster[row] == 3){
    mydata$Cluster3[row] <- TRUE
  }
  else if(mydata$cluster[row] == 4){
    mydata$Cluster4[row] <- TRUE
  }
  else{
    mydata$Cluster5[row] <- TRUE
  }
}

#�������� �������� ����� ������ cluster
mydata <- subset (mydata, select = -cluster)

#�������� �������������
summary(mydata)




####################################Exercise 4####################################

#������� ��� cluster1
apr_clust1 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = 0.001, confidence = 0.01, minlen = 2), appearance = list(lhs = "Cluster1"))
inspect(sort(apr_clust1, by = 'confidence'))
apr_clust1 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = 0.001, confidence = 0.01, minlen = 2), appearance = list(rhs = "Cluster1"))
inspect(sort(apr_clust1, by = 'confidence'))

#������� ��� cluster2
apr_clust2 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = .001, confidence = 0.11, minlen = 2), appearance = list(lhs = "Cluster2"))
inspect(sort(apr_clust2, by = 'confidence'))
apr_clust2 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = 0.001, confidence = 0.11, minlen = 2), appearance = list(rhs = "Cluster2"))
inspect(sort(apr_clust2, by = 'confidence')[1:20])


#������� ��� cluster3
apr_clust3 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = .001, confidence = 0.11, minlen = 2), appearance = list(lhs = "Cluster3"))
inspect(sort(apr_clust3, by = 'confidence'))
apr_clust3 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = 0.001, confidence = 0.11, minlen = 2), appearance = list(rhs = "Cluster3"))
inspect(sort(apr_clust3, by = 'confidence')[1:20])

#������� ��� cluster4
apr_clust4 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = .001, confidence = 0.11, minlen = 2), appearance = list(lhs = "Cluster4"))
inspect(sort(apr_clust4, by = 'confidence'))
apr_clust4 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = 0.001, confidence = 0.11, minlen = 2), appearance = list(rhs = "Cluster4"))
inspect(sort(apr_clust4, by = 'confidence')[1:20])


#������� ��� cluster5
apr_clust5 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = .001, confidence = 0.11, minlen = 2), appearance = list(lhs = "Cluster5"))
inspect(sort(apr_clust5, by = 'confidence'))
apr_clust5 = apriori(data = mydata[,7:ncol(mydata)], 
                     parameter = list(supp = 0.001, confidence = 0.11, minlen = 2), appearance = list(rhs = "Cluster5"))
inspect(sort(apr_clust5, by = 'confidence')[1:20])





























