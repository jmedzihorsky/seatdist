#   eastdist internals
#   Juraj Medzihorsky
#   2017-04-25

#   --------------------------
#       largest remainders
#   --------------------------

#   largest remainders
lr <-
function(votes, m, qnm)
    {
        qnm <- tolower(qnm)
        if (!is.character(qnm)) {
            stop('quota must be character\n')
        } else if (qnm %in% c('ha', 'har', 'hare')) {
            d <- sum(votes)/m
            qn <- 'Hare'
        } else if (qnm %in% c('dr', 'dro', 'droop')) {
            d <- trunc(1+sum(votes)/(m+1))
            qn <- 'Droop'
        } else if (qnm %in% c('hb', 'hag', 'hagenbach-bischoff')) {
            d <- sum(votes)/(m+1)
            qn <- 'Hagenbach-Bischoff'
        } else if (qnm %in% c('im', 'imp', 'imperiali')) {
            d <- sum(votes)/(m+2)
            qn <- 'Imperiali'
        } else if (qnm %in% c('adj', 'rei', 'adjusted', 'reinforced',
                              'adjusted imperiali', 'einforced imperiali')) {
            d <- sum(votes)/(m+3)
            qn <- 'Reinforced Imperiali'
        } else {
            stop('quota must be named\n')
        }
        saux <- votes/d
        s0 <- trunc(saux)
        rem <- saux - s0
        mn <- m - sum(s0)
        if (mn>0) {
            tr <- rem[order(rem, decreasing=T)][mn]
            s1 <- ifelse(rem>=tr, 1, 0)
            seats <- s0 + s1 
        } else if (mn==0) {
            seats <- s0
        } else if (mn<0) {
            stop('something went wrong, more seats awarded by the quota than available\n')
        }
        return(list(seats=seats, qn=qn))
    }


#   -----------------------
#       divisor method
#   -----------------------

#   general engine for divisor method
divisormethod <-
function(.votes, .m, .div)
    {
        G <- sapply(.div, function(x) .votes/x)
        threshold <- G[order(G, decreasing=T)][.m]
        W <- (G >= threshold) 
        seats <- rowSums(W)
        return(seats)
    }


#   D'Hondt/Jefferson/Hagenbach-Bischoff
dh <-
function(votes, m)
    {
        div <- 1:m
        return(divisormethod(.votes=votes, .m=m, .div=div))
    }


#   Adams
adams <-
function(votes, m)
    {
        div <- 1:m - 1
        out <- divisormethod(.votes=votes, .m=m, .div=div)
        out[is.na(out)] <- 0
        return(out)
    }


#   Nohlen
nohlen <-
function(votes, m)
    {
        div <- 1:m + 1
        return(divisormethod(.votes=votes, .m=m, .div=div))
    }


#   Sainte-Lague/Webster
sl <-
function(votes, m)
    {
        div <- 2*(1:m) - 1
        return(divisormethod(.votes=votes, .m=m, .div=div))
    }


#   Modified Sainte-Lague
msl <-
function(votes, m)
    {
        div <- c(1, (10*(2:m)-5)/7)
        return(divisormethod(.votes=votes, .m=m, .div=div))
    }


#   Hungarian Sainte-Lague
hungarian <-
function(votes, m)
    {
        div <- c(1.5, 2*(2:m) - 1)
        return(divisormethod(.votes=votes, .m=m, .div=div))
    }


#   Imperiali
imperiali <-
function(votes, m)
    {
        div <- ((1:m)+1)/2
        return(divisormethod(.votes=votes, .m=m, .div=div))
    }


#   Danish
danish <-
function(votes, m)
    {
        div <- 3*(1:m) - 2
        return(divisormethod(.votes=votes, .m=m, .div=div))
    }


#   Huntington-Hill
hh <-
function(votes, m)
    {
        div <- sqrt( (1:m) * ((1:m)-1) )
        return(divisormethod(.votes=votes, .m=m, .div=div))
    }


#   ------------------------------
#       disproportionality aux
#   ------------------------------

#   find pivotal coalition member (for power indexes)
findpivot <-
function(nms, w, thresh)
    {
        l <- length(nms)
        k <- sapply(1:l, function(i) sum(w[nms[1:i]]))
        piv <- nms[which(k>thresh)][1]
        return(piv)
    }

#   Shapley-Shubik power index (for disproportionality indexes)
powind_shsh <-
function(party_weights, threshold=NULL)
    {
        if (is.null(threshold)) {
            threshold <- sum(party_weights)/2
        }
        party_names <- names(party_weights)
        permutations <- combinat::permn(party_names)
        pivots <- sapply(permutations, findpivot, w=party_weights, thresh=threshold)    
        table_pivots <- table(pivots)/length(pivots)
        shsh <- rep(0, length(party_names))
        names(shsh) <- party_names
        shsh[names(table_pivots)] <- table_pivots
        return(shsh)
    }

