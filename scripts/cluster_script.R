credit.clust <- data.frame(default = dfcat$default[1:100], housing = dfcat$housing[1:100],
                           loan = dfcat$loan[1:100])

cluster.results <-kmodes(credit.clust, 3, iter.max = 10, weighted = FALSE )