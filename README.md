coolcat-r
=========

Clustering algorithm Coolcat(Barbará, Couto, Li) implemented on R.

Usage
-----

    library(coolcat)
    data(house.votes)
    cc<-coolcat(house.votes[,-1],k=2,m.replacement=0.2,trace.log=T)
    # Clusters
    cc$clustering
    # Attributes values percentages by cluster
    tableByFactor(cc$data,cc$clustering)
    # Category Utility Function 
    categoryUtilityFunction(cc)
    # Draws a clustering plot and a silhoutte plot. 
    plot(cc)

Todo
----

* Speed-up the algorithm. Is very slow on fitting points and detect bad points
* Use alternative functions to detect bad items.

Licence
-------

GPL-3 
