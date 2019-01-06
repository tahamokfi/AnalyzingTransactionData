Analyzing Transaction data like a data scientist
================
Taha Mokfi
March 24, 2017

Links to all the other parts
----------------------------

1.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_2.md" target="_blank">Part 2: Network analysis</a>**
2.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_3.md" target="_blank">Part 3: Association rule mining</a>**
3.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_4.md" target="_blank">Part 4: Sequential rules mining</a>**

**You can download the code from <a href="http://bit.ly/2RxMarz" target="_blank">this link</a> and data <a href="http://bit.ly/2VBlzsy" target="_blank">here</a>**

Introduction
------------

After winning the first place in 2017 Big data analytics symposium competition for Siemens wind turbines, we inspired by the power of the machine learning techniques on the transaction dataset. This is the first series of "Analyzing transaction data like a data scientist" articles in which we tried to give you practical examples. For sure the results and interpretations on the different datasets vary by application and data quality. We used two classical methods: clustering and association rule mining and two less known methods: Sequential pattern mining and network analysis for the project.

In this series of R code, we are trying to show how you can analyze your transaction datasets using different unsupervised methods in Machine Learning. First, we will use clustering approaches to cluster our records. The entities in a transaction dataset can be customers or error codes occurred in wind turbines. Both are transactions and each transaction is either a purchase of a specific product or an error code happened in a turbine. This part is all about applying clustering methods on this type of data.

First Approach: Clustering
==========================

Data description
----------------

The data is transaction data of customer invoices from a company. One can assume that this data contains transactions of wind turbine error codes or any other similar cases. You can download the dataset <a href="http://goo.gl/8mjbvz" target="_blank">from here</a>. Data contains 10546 transactions of 392 customers with the following variables:

1.  **InvoiceNo:** Contains the Invoice number of customer which is identical for each visit in each date
2.  **StockCode:** Indicates the product bought by the customer in specific invoice number
3.  **InvoiceDate:** Indicates the date of purchase
4.  **CustomerID:** Which is identical for each customer
5.  **Country:** This indicates the country of buyer

Importing data and do crosstabs
-------------------------------

First, we import the dataset into R and transform date variable to the proper format using following codes.

``` r
#Load the data
dt1 <- read.csv("Final.csv")
dt1$InvoiceDate <- as.POSIXct(dt1$InvoiceDate, format = "%m/%d/%Y %H:%M")
```

We cannot use the transaction data directly for clustering and we have to somehow use crosstab strategy to make it usable for the clustering techniques. If the data would be as follow:

<center>
![Original dataset](D:/job/Portfolio/Rpubs/transactional%20datasets/Table1.PNG)
</center>
You can either crosstab it based on each Invoice ID like this one:

<center>
![Based on Invoice ID](D:/job/Portfolio/Rpubs/transactional%20datasets/Invtable.PNG)
</center>
Or do crosstab on it based on customer Id which would be similar to this one:

<center>
![Based on Invoice ID](D:/job/Portfolio/Rpubs/transactional%20datasets/custable.PNG)
</center>
In the first table, most of the values are either zero or one because in each visit (Invoice) products are identical. But in the second table values are from a wider range. Usually, it depends on you and the project owners to select between these datasets.

Here, we chose the second strategy, do crosstabs and aggregation based on customers. For constructing these tables, you can use `melt` and `dcast` function from `reshape2` library. In melt function, we set `id.vars` to be any variables except the "StockCode" which should be as columns. Finally, using `dcast` we set the rows as "CustomerID" in the dataset.

``` r
#Reshape the dataset
library(reshape2)
mlt1 = melt(dt1,id.vars =  names(dt1)[c(1,3:5)])
dt2 <- dcast(mlt1, CustomerID  ~ value, value.var = c("CustomerID"),fun.aggregate =length)
#Remove first variable containing the CustomerID
dt3 <- dt2[, -1]
```

Then we have to reduce the dimensionality because we have 260 variables. For this purpose, we can use Principal component Analysis (PCA) but if you are using another dataset, make sure that a number of columns (variables) are less than a number of rows (records) otherwise you cannot apply PCA. Moreover, it is a better approach to standardize the dataset before applying PCA. This dataset is highly biased like many other datasets so, many values in "dt3" are zero. You can use normalization or standardization or like us, you can just use binary indicators for each variable (which are products here) in which zero means "not bought" and 1 means "at least bought once". Then we transformed all the variables to numeric and used PCA to reduce the dimensions to 6.

``` r
#Transform dataset
for (i in 1:ncol(dt3)){
  dt3[,i] <- ifelse(dt3[,i]==0,0,1)
}
#Apply PCA
for (i in 1:ncol(dt3)){
  dt3[, i] <- as.numeric(dt3[, i])
}
pcaout <- prcomp(dt3)
pcdt <- pcaout$x[,1:6]
```

Next, we used K-means clustering and Davies-Bouldin index to cluster and find the best number of clusters. The less the Davies-Bouldin index is the better the clusters are. You have to install `clusterCrit` before running this function. This function has four arguments. First is dataset you want to cluster, second is the minimum value of K and the third one is the maximum number of K and finally "rnd" which indicates the random seed for clustering. The output of this function would be Davies-Bouldin index. If your dataset is huge and wants to see the progress of clustering, you might use `print(paste0("K is equal to= ",j))` which is set to comment here.

``` r
#Clustering Function
kmcl <- function(data, kmin, kmax, rnd = 1357){
  library(clusterCrit)
  DB <- c()
  for (i in 1:ncol(data))
  {
    data[, i] <- as.numeric(data[, i])
  }
  for (j in kmin:kmax)
  {
    set.seed(rnd)
    km <- kmeans(data, centers = j)
    data1 <- as.matrix(data)
    # Computing the Davies-Bouldin
    DB[j] <- intCriteria(data1, km$cluster, crit = c("Davies_Bouldin"))
    #print(paste0("K is equal to= ",j))
  }
  return(DB)
}
```

In order to better show the Davies-Bouldin values along with a different number of clusters, we created the following function for plotting the output of clustering function.

``` r
#Davies-Bouldin Plot
dbplot <- function(obj, tit1 = "Null"){
  plot(2:15, unlist(obj), type = "o", col = "black", ylim = c(0, 3), 
       lwd = 2, ylab = "DB Vlaue", xlab = "Number of Clusters", cex.lab = 1.5, 
       cex.axis = 0.75)
  grid(nx = 25, ny = 25, col = "lightgray", lty = "dotted", lwd = par("lwd"), 
       equilogs = TRUE)
  axis(side = 1, at = seq(2, 15, by = 1), cex.axis = 0.75)
  box(col = "black", lwd = 2)
  title(tit1)
}
```

After applying "kmcl" function and searching for the best number of clusters from 2 to 15 clusters, we found 8 clusters in the dataset. Following codes apply 8 clusters on the dataset and show the distribution of customers along with the clusters.

``` r
#Apply clustering for different K
clusdb <- kmcl(pcdt,2,15,rnd = 1336)
#Apply DB plot
dbplot(clusdb,"Davies Bouldin for Clustering")
```

![](Part_-1_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#Apply K-means with 6 clusters
set.seed(1336)
km1 <- kmeans(pcdt,centers = 8)
table(km1$cluster)
```

    ## 
    ##   1   2   3   4   5   6   7   8 
    ##  45   9 252  13  15  31  23   4

In the next step, we created a "rec.vis" which contains the clusters associated with each customer. But this data won't give us insight to our customers (or transactions entities) so we then merge this data with the original dataset and create "dt4" dataset. Now we can use other variables in the dataset to present and interpret the clusters. The only variable here is country. The tb2 and tb1 contain the crosstab tables of country versus clusters.

``` r
#Adding cluster
rec.inv <- cbind(dt2, km1$cluster)[, c(1, length(dt2)+1)]
names(rec.inv) <- c("CustomerID", "invCluster")
#Merging with rest of variables
dt3 <- unique(dt1[,c(4,5)])
dt4 <- merge(dt3,rec.inv,by="CustomerID")
names(dt4)[3] <- c("Cluster")
#Ploting the cluster VS country
tb2 <- as.data.frame.matrix(table(dt4$Country,dt4$Cluster))
tb1 <- as.data.frame(table(dt4$Country,dt4$Cluster))
```

Look at the tb2 table. There are values from different ranges in this table. A good approach is to normalized these values according to each country or each cluster. Here, row1 contains the normalized values (values are a count of customers) based on country and col1 contains values normalized by each cluster. Although most of the customers are placed in one cluster, by visualizing values in tb1 and tb2 we can show some of the existing patterns in the dataset. We used `ggplot2` library for visualizing these values using following codes.

``` r
#Normalization
norm1 <- function(x){
  (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
tb1$col1 <- as.vector(as.matrix(as.data.frame(lapply(tb2, norm1))))
tb1$row1 <- as.vector(as.matrix(t(as.data.frame(lapply(as.data.frame(t(tb2)), norm1)))))
tb1$Var1 <- as.factor(tb1$Var1)
tb1$Var2 <- as.factor(tb1$Var2)
library(ggplot2)
ggplot(tb1, aes(Var2,Var1))+geom_point(aes(alpha=row1,size=row1),colour = "blue")+scale_size_continuous(range = c(0.5,5))+
  labs(x="Invoice Clusters",y="Countries",size="Frequency",alpha="Frequency") +
  ggtitle("Bubble plot of customer clusters and countries - Normalized by country")+
  theme(axis.text.x = element_text(hjust=1,vjust=0.5,size = 12,face = "bold",color = "Black"),
        plot.title = element_text(hjust=0.5,size = 14,face = "bold",color = "Black"),
        axis.title.x =element_text(hjust=0.5,size = 16,face = "bold",color = "Black"),
        axis.title.y =element_text(hjust=0.5,size = 16,face = "bold",color = "Black"),
        legend.title = element_text(hjust=0.5,size = 14,face = "bold",color = "Black"),
        legend.text = element_text(hjust=0.5,size = 10,face = "bold",color = "Black"))
```

![](Part_-1_files/figure-markdown_github/unnamed-chunk-8-1.png)

The bigger the bubbles are the more customers in that spot are. For example, we can say that Singapore, Iceland, and the Channel Islands have more customers which are in cluster one than other countries.

For better illustration of cluster patterns based on countries we can cluster the tb2 table which is shown here:

``` r
View(tb2)
```

|                      |    1|    2|    3|    4|    5|    6|    7|    8|
|:---------------------|----:|----:|----:|----:|----:|----:|----:|----:|
| Australia            |    0|    0|    6|    0|    1|    0|    0|    1|
| Austria              |    0|    0|    6|    0|    0|    1|    1|    0|
| Bahrain              |    0|    0|    1|    0|    0|    0|    0|    0|
| Belgium              |    1|    0|   14|    1|    0|    2|    4|    0|
| Brazil               |    0|    0|    1|    0|    0|    0|    0|    0|
| Canada               |    0|    0|    1|    0|    0|    0|    1|    0|
| Channel Islands      |    4|    0|    5|    0|    0|    0|    0|    0|
| Cyprus               |    0|    0|    5|    0|    0|    1|    1|    0|
| Czech Republic       |    0|    0|    1|    0|    0|    0|    0|    0|
| Denmark              |    0|    0|    7|    0|    0|    1|    0|    0|
| EIRE                 |    0|    0|    0|    1|    0|    0|    0|    2|
| European Community   |    0|    0|    1|    0|    0|    0|    0|    0|
| Finland              |    1|    0|    8|    1|    0|    1|    0|    0|
| France               |    4|    5|   49|    3|    1|   13|    7|    0|
| Germany              |   10|    3|   53|    1|    9|    7|    7|    0|
| Greece               |    0|    0|    4|    0|    0|    0|    0|    0|
| Iceland              |    1|    0|    0|    0|    0|    0|    0|    0|
| Israel               |    0|    0|    2|    0|    1|    0|    0|    0|
| Italy                |    3|    0|    9|    0|    0|    1|    0|    0|
| Japan                |    1|    0|    6|    0|    0|    0|    0|    0|
| Lebanon              |    0|    0|    1|    0|    0|    0|    0|    0|
| Lithuania            |    0|    0|    1|    0|    0|    0|    0|    0|
| Malta                |    0|    0|    2|    0|    0|    0|    0|    0|
| Netherlands          |    0|    0|    4|    0|    0|    2|    0|    1|
| Norway               |    1|    0|    6|    2|    0|    0|    1|    0|
| Poland               |    2|    0|    3|    0|    0|    0|    0|    0|
| Portugal             |    5|    0|   12|    1|    1|    0|    0|    0|
| RSA                  |    0|    0|    1|    0|    0|    0|    0|    0|
| Saudi Arabia         |    0|    0|    1|    0|    0|    0|    0|    0|
| Singapore            |    1|    0|    0|    0|    0|    0|    0|    0|
| Spain                |    5|    0|   16|    3|    0|    1|    0|    0|
| Sweden               |    1|    0|    7|    0|    0|    0|    0|    0|
| Switzerland          |    4|    1|   11|    0|    2|    0|    1|    0|
| United Arab Emirates |    0|    0|    2|    0|    0|    0|    0|    0|
| Unspecified          |    0|    0|    4|    0|    0|    0|    0|    0|
| USA                  |    1|    0|    2|    0|    0|    1|    0|    0|

For this purpose, we used the same function for clustering and this time on the tb2 table we got 6 clusters as our desired number of clusters. So, countries can be grouped into 6 clusters. The interesting thing here is that if you want to aggregate your data based on the countries, which are 36 here, from the first step you will have a matrix of 36 rows and 260 columns. In this matrix, the number of observations is less than a number of columns so that you cannot use PCA directly and without PCA because of the curse of dimensionality you cannot have good clusters of countries based on products. But here thanks to two-level clustering approach we used, you can easily cluster countries based on customers basket patterns.

``` r
#cluster based on countries
clcnt <- kmcl(tb2,2,15,rnd = 1366)
#Plot Davies Bouldin
dbplot(clcnt,"Davies Bouldin for Clustering")
```

![](Part_-1_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
#Apply best K (K=6)
set.seed(1366)
km2 <- kmeans(tb2,centers = 6)
```

You can even show the clusters on the previous plot. In the following code first we obtain the membership of each country and since the country is a factor variable, after sorting them based on the cluster we save the order of countries in `or1` object. Then we used `factor()` function to reorder country variable based on `or1` which contains the order of country by country clusters. We again applied same code for the plot but instead, we used `colour = as.factor(tb1$clus)` which indicates the clusters of countries using different colors.

``` r
contcl.data <- cbind(tb2,km2$cluster)
names(contcl.data)[ncol(contcl.data)] <- "cluster"
contcl.data <- contcl.data[order(contcl.data$cluster),]
or1 <-rownames(contcl.data)
#Sort con
tb1$Var1 <- factor(tb1$Var1 , levels = or1)
tb1$clus <- rep(as.integer(km2$cluster),8)
ggplot(tb1, aes(Var2,Var1))+geom_point(aes(alpha=row1,size=row1,colour = as.factor(tb1$clus)))+scale_size_continuous(range = c(0.5,5))+
  labs(x="Invoice Clusters",y="Countries",size="Frequency",alpha="Frequency") +
  ggtitle("Bubble plot of customer clusters and countries - Normalized by country")+
  theme(axis.text.x = element_text(hjust=1,vjust=0.5,size = 12,face = "bold",color = "Black"),
        plot.title = element_text(hjust=0.5,size = 14,face = "bold",color = "Black"),
        axis.title.x =element_text(hjust=0.5,size = 16,face = "bold",color = "Black"),
        axis.title.y =element_text(hjust=0.5,size = 16,face = "bold",color = "Black"),
        legend.title = element_text(hjust=0.5,size = 14,face = "bold",color = "Black"),
        legend.text = element_text(hjust=0.5,size = 10,face = "bold",color = "Black"))
```

![](Part_-1_files/figure-markdown_github/unnamed-chunk-12-1.png)

Each color indicates a cluster of countries with similar customers in terms of shopping pattern. This is the final goal of clustering which tries to give an abstract view of a huge dataset using a simple visualization technique. **You can download the code from <a href="http://bit.ly/2RxMarz" target="_blank">this link</a> and data <a href="http://bit.ly/2VBlzsy" target="_blank">here</a>**
