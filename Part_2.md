Analyzing Transaction data like a data scientist
================
Taha Mokfi
March 24, 2017

Links to all the other parts
----------------------------

1.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_1.md" target="_blank">Part 1: Cluster analysis</a>**
2.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_3.md" target="_blank">Part 3: Association rule mining</a>**
3.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_4.md" target="_blank">Part 4: Sequential rules mining</a>**

**You can download the code from <a href="http://bit.ly/2ReFBe8" target="_blank">this link</a> and data <a href="http://bit.ly/2VBlzsy" target="_blank">here</a>**

Second Approach: Network analysis
=================================

In this phase, we look at the Transaction data as a network structure. Nodes are events (which here are products or can be errors in turbine failure dataset) and edges indicate whether there is a relationship between events. This relationship in our example can be interpreted as a sequence of purchase. For example looking at the following table:

<center>
![Original dataset](D:/job/Portfolio/Rpubs/transactional%20datasets/Table1.PNG)
</center>
Customer number 12 purchased {A,B,C} in the 1/1/16 13:40 and then in the same day but later that day 1/1/16 18:15 purchased B and C. In terms of transition matrix we can say {A,B,C} are antecedents and {B,C} are precedent so we can say if A -&gt; B, A -&gt; C, B -&gt; B, B -&gt; C, C -&gt; B, and C -&gt; C are the rules. Actually we can say {A,B,C} are the first state and {B,C} are the next state. We can transform all of these rules to a transition matrix as follows:

<center>
![Original dataset](D:/job/Portfolio/Rpubs/transactional%20datasets/trmatrix.PNG)
</center>
Then based on this matrix we can plot the network of products like this one:

<center>
![Original dataset](D:/job/Portfolio/Rpubs/transactional%20datasets/smg1.PNG)
</center>
Let's start with our dataset by importing it into R. More information about data can be found **<a href="http://rpubs.com/tahamokfi/Part1_AnalyzeTransactionData" target="_blank">here</a>**.

``` r
#Load the data
dt1 <- read.csv("Final.csv")
dt1$InvoiceDate <- as.POSIXct(dt1$InvoiceDate, format = "%m/%d/%Y %H:%M")
#Create a list of customer ID (entity ID)
un2 <- unique(dt1$CustomerID)
#Order the data based on entity and date variable (customer and invoice date)
dt2 <- dt1[order(dt1$CustomerID,dt1$InvoiceDate),]
```

Then we used a sophisticated "for loop" using which one can create the transition matrix of any Transaction dataset. This for loops iterates over different customer IDs which are in `un2`. First, it finds unique Invoice dates for each customer and then iterates over different dates and make the first date as the first state and second date as the second state and compute all of the combinations between products using expand.grid and create an edge list and continue these steps until the last date of each customer. At the end, it combines all of the edge lists from the customers so we can use it to create transition matrix and construct network graphs. If you want to see the progression of for loop you might uncomment the "print(paste0..." part in the following code.

``` r
#For loop for creating transition matrix
edg3=c()
for (i in 1:length(un2)){
  un3 <- base::unique(dt2[which(dt2$CustomerID==un2[i]),]$InvoiceDate)
  #print(paste0("Running ",i," of the ",length(un2)))
  edg2=c()
  for (j in 1:length(un3)-1){
    lis1 <- dt2[which(dt2$CustomerID==un2[i] & dt2$InvoiceDate==un3[j]),2]
    lis2 <- dt2[which(dt2$CustomerID==un2[i] & dt2$InvoiceDate==un3[j+1]),2]
    edg1 <- expand.grid(lis1,lis2)
    edg2 <- rbind(edg1,edg2)
  }
  edg3 <- rbind(edg2,edg3)
}
```

Next step, using "igraph" package we created a graph from the transition matrix. Then we computed the betweenness centrality measure. The betweenness is (roughly) defined by the number of shortest paths going through a vertex or an edge. We actually want to answer this question that "when going from a product (StockCode) to another product (StockCode) in a sequence, is there a product which is frequently bought?" Node (which are products here) with higher betweenness centrality would have more control over the network of products. In the other words, most of the products pass through this product. The following code calculates the betweenness for all the nodes and gives you the 10 most central products.

``` r
#Creating graph from the matrix
library(igraph)
g1 <- graph.data.frame(edg3,directed=FALSE)
adj1 <- as_adjacency_matrix(g1,type="both",names=TRUE,sparse=FALSE)
#Afjacency matrix
g1 <- graph.adjacency(as.matrix(adj1),mode="directed",weighted=TRUE)
#Compute the betweeenness using igraph
cen1 <- igraph::betweenness(g1, v = V(g1), directed = TRUE)
#Top 10 betweenness
names(sort(cen1,decreasing = T)[1:10])
```

    ##  [1] "21452" "22487" "22466" "85053" "22567" "84378" "21669" "22427"
    ##  [9] "22776" "84987"

In this step, based on the fact that "there might be some products which have strong relations with each other than with other groups of products" the good practice will be finding the different communities inside the network. So, community detection algorithms can be applied. Since usually graphs for Transaction datasets are weighted and directed in `igraph` package you can use different algorithms such as; spinglass , infomap, walktrap, edge betweenness, and label propagation to find the communities. For this example, we chose `spinglass` algorithm. This step is more like clustering the graph. After clustering, we can investigate the membership of communities. Here the `grclus` contains the products and their associated communities.

``` r
#Clustering the graph using spinglass techniques
set.seed(1234)
cm1 <- cluster_spinglass(g1)
#Members of each community
table(cm1$membership)
```

    ## 
    ##   1   2   3 
    ## 101  84  75

``` r
grclus <- as.data.frame(cbind(cm1$names,cm1$membership))
#Filter the nodes which are in the community 1
ver1 <- as.character(grclus[which(grclus$V2 %in% c(1)),1])
```

We got three different communities in our dataset. Above table shows the distribution of products in each of these three communities. The `grclus` contains the products and their associated communities (or we can say associated clusters instead). Next, we extracted the products which are in the community one and put their names into `ver1` object. Then we calculated the centrality for all of the nodes in this community. Afterward we created `g3` graph which is sub-graph of original graph filtered by 7 most central products of the first community.

``` r
#Compute the centrality for the first community (cluster)
g2 <- induced_subgraph(g1,v = ver1)
cen2 <- igraph::betweenness(g2, v = V(g2), directed = TRUE)
top1 <- names(sort(cen2,decreasing = T)[1:7])
g3 <- adj1[which(rownames(adj1) %in% top1),which(colnames(adj1) %in% top1)]
```

We can visualize the graph of these 7 nodes using either `igraph` or any other packages like `sna` which I like it. As it is shown in the following graph, "22776" connected to almost all of central products except "22195" which is only connected to "22178" and "21164". Using this graph we can have more information about the customer baskets and more insight about affinity analysis among the products.

``` r
library(sna)
net1 <- network(x = g3,directed = T)
set.seed(12345)
gplot(net1,gmode="digraph",displaylabels = T,object.scale=0.009,label.cex = 1.1,edge.col = "Blue")
```

![](Part2_files/figure-markdown_github/unnamed-chunk-6-1.png)

Unlike the previous part (part-1) which was mostly about analyzing Transaction dataset based on customers, this part was about analysis products and their interconnection relations. **You can download the code from <a href="http://bit.ly/2ReFBe8" target="_blank">this link</a> and data <a href="http://bit.ly/2VBlzsy" target="_blank">here</a>**
