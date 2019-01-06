Analyzing Transaction data like a data scientist
================
Mahsa Almaeenejad
March 24, 2017

Links to all the other parts
----------------------------

1.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_1.md" target="_blank">Part 1: Cluster analysis</a>**
2.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_2.md" target="_blank">Part 2: Network analysis</a>**
3.  **<a href="https://github.com/tahamokfi/AnalyzeTransactionData/blob/master/Part_4.md" target="_blank">Part 4: Sequential rules mining</a>**

**You can download the code from <a href="http://bit.ly/2RxSfnH" target="_blank">this link</a> and data <a href="http://bit.ly/2VBlzsy" target="_blank">here</a>**

Third Approach: Association rules mining
========================================

This step is mostly about finding affinity rules in the dataset. We want to say for example "if a customer purchased A then he/she will purchase B" with specific probability. Again, we have to first construct our dataset so that we can apply our desire algorithm on it.

``` r
#Load the data
dt1 <- read.csv("Final.csv")
dt1$InvoiceDate <- as.POSIXct(dt1$InvoiceDate, format = "%m/%d/%Y %H:%M")
```

As we discussed in [part 1](), we can use the strategy of applying crsootab based on Invoice ID or customer ID. For this project, we want to consider Invoice IDs for data construction. So the data would be something similar to this table:

<center>
![Based on Invoice ID](D:/job/Portfolio/Rpubs/Transactional%20datasets/Invtable.PNG)
</center>
Using the following code you can reshape the dataset to the desired format.

``` r
#Reshape the dataset
library(reshape2)
mlt1 = melt(dt1,id.vars =  names(dt1)[c(1,3:5)])
dt2 <- dcast(mlt1, InvoiceNo  ~ value, value.var = c("InvoiceNo"),fun.aggregate = length)
```

Next, we can apply Apriori algorithm on this dataset. First, we transformed dataset to the transaction format and then using `apriori` function and setting support to 5% and confidence to 80%, we can extract association rules. Based on the output of "inspect" function we can say that if customers purchase 23286 and 23287 then he/she will purchase 23285 with probability of 84% and this rule happened for 5% of customers. The greater the lift for a rule the more interesting is that rule. Based on the result of `summary(rul1)` the mean of 99 rules which is found here is around 10.6 which indicates that all of these rules are interesting and do not happen in a random situation.

``` r
#Apply Apriori algorithm
library(arules)
trns1 <- as(split(dt1[,"StockCode"], dt1[,"CustomerID"]), "transactions")
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
rul1 <- apriori(trns1, parameter = list(supp = 0.05, conf = 0.8,minlen=2))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.8    0.1    1 none FALSE            TRUE       5    0.05      2
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 19 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[260 item(s), 392 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [215 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [99 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
summary(rul1)
```

    ## set of 99 rules
    ## 
    ## rule length distribution (lhs + rhs):sizes
    ##  2  3  4 
    ## 35 45 19 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.000   2.000   3.000   2.838   3.000   4.000 
    ## 
    ## summary of quality measures:
    ##     support          confidence          lift            count      
    ##  Min.   :0.05102   Min.   :0.8065   Min.   : 7.006   Min.   :20.00  
    ##  1st Qu.:0.05230   1st Qu.:0.8495   1st Qu.: 9.608   1st Qu.:20.50  
    ##  Median :0.05357   Median :0.9167   Median :10.595   Median :21.00  
    ##  Mean   :0.05939   Mean   :0.9133   Mean   :10.646   Mean   :23.28  
    ##  3rd Qu.:0.06378   3rd Qu.:0.9545   3rd Qu.:11.769   3rd Qu.:25.00  
    ##  Max.   :0.09439   Max.   :1.0000   Max.   :12.776   Max.   :37.00  
    ## 
    ## mining info:
    ##   data ntransactions support confidence
    ##  trns1           392    0.05        0.8

``` r
inspect(head(sort(rul1, by="lift"), 5))
```

    ##     lhs              rhs     support    confidence lift     count
    ## [1] {21500}       => {21499} 0.05612245 0.8800000  12.77630 22   
    ## [2] {21499}       => {21500} 0.05612245 0.8148148  12.77630 22   
    ## [3] {23286,23287} => {23285} 0.05357143 0.8400000  12.66462 21   
    ## [4] {23286}       => {23285} 0.06632653 0.8387097  12.64516 26   
    ## [5] {23285}       => {23286} 0.06632653 1.0000000  12.64516 26

Using `arulesViz` package we can visualize our rules based on their quality measures. By running the following code, you will have a plot of rules in which X is LHS and y is RHS part of your rules. The size of bubbles shows the support of rules and color indicates the lift (The bigger and darker the bubble the higher support and lift are). We made a subset from original rules and created our plots based on these top 10 rules based on lift measure (object subrules1).

``` r
#Plot subset of rules
subrules1 <- head(sort(rul1, by="lift"), 10)
library(arulesViz)
set.seed(1234)
plot(subrules1, method="grouped",  control=list(k=5))
```

![](Part3_files/figure-markdown_github/unnamed-chunk-4-1.png)

The last plot shows the relationship between 15 rules with the highest lift. Again, size and color are indicators of support and lift. Arrows show whether the specific product is in LHS or RHS of the rule.

``` r
#Plot relation between products in terms of rules
set.seed(1234)
plot(subrules1, method="graph")
```

![](Part3_files/figure-markdown_github/unnamed-chunk-5-1.png)

In this part we analysis the affinity among the products using which one can see which products are related to each other. **You can download the code from <a href="http://bit.ly/2RxSfnH" target="_blank">this link</a> and data <a href="http://bit.ly/2VBlzsy" target="_blank">here</a>**
