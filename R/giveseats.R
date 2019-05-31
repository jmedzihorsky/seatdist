giveseats <-
function(v,
             ns,
             method,
             thresh=0,
             quota=NA)
    {
        if (!is.numeric(thresh)) {
            stop('thresh must be numeric\n')
        }
        nv <- sum(v)
        vn <- v
        if ( (thresh >= 0) & (thresh <= 1) ) {
            cat('thresh treated as a fraction\n')
            vn[v <= nv*thresh] <- 0
        } else if ((thresh>1) & (thresh<=1e2) ) {
            cat('thresh treated as a %\n')
            tn <- thresh/1e2
            vn[v <= nv*tn] <- 0
        } else if (thresh > 100 ) {
            cat('thresh treated as a vote count\n')
            vn[v<=thresh] <- 0 
        } else if (thresh < 0) {
            stop('negative thresh not allowed\n')
        }


        meth <- tolower(method)
        if (meth %in% c("dh", "d'hondt", "dhondt", "je", "jeff", "jefferson",
                        "hb", "hagbisch", "hagenbach") ) {
            o <- dh(votes=vn, m=ns)
            mn <- "D'Hondt/Jefferson/Hagenbach-Bischoff"
        } else if (meth %in% c('sl', 'sainte', 'sainte lague', 'sainte-lague',
                               'we', 'web', 'webster') ) {
            o <- sl(votes=vn, m=ns) 
            mn <- "Sainte-Lague/Webster"
        } else if (meth %in% c('msl', 'mod sainte', 'modified sainte lague') ) {
            o <- msl(votes=vn, m=ns)
            mn <- c('Modified Sainte-Lague')
        } else if (meth %in% c('hu', 'hun', 'hungarian')) {
            o <- hungarian(votes=vn, m=ns)
            mn <- c('Hungarian')
        } else if (meth %in% c('da', 'dan', 'danish') ) {
            o <- danish(votes=vn, m=ns)
            mn <- 'Danish'
        } else if (meth %in% c('im', 'imp', 'imperiali', 'be', 'bel', 'belgian') ) {
            o <- imperiali(votes=vn, m=ns)
            mn <- 'Imperiali/Belgian'
        } else if (meth %in% c('hh', 'huntington', 'huntington-hill',
                               'huntington hill',
                               'ep', 'equal', 'equal proportions') ) {
            o <- hh(votes=vn, m=ns) 
            mn <- 'Huntington-Hill/Equal Proportions'
        } else if (meth %in% c('ad', 'adams', 'sd', 'smallest', 'smallest divisors') ) {
            o <- adams(votes=vn, m=ns) 
            mn <- 'Adams/Smallest Divisors'
        } else if (meth %in% c('no', 'noh', 'nohlen') ) {
            o <- nohlen(votes=vn, m=ns)
            mn <- 'Nohlen'
        } else if (meth %in% c('lr', 'largest', 'remainders', 'largest remainders') ) {
            a <- lr(votes=vn, m=ns, qnm=quota) 
            o <- a$seats
            mn <- paste('Largest Remainders with', a$qn, 'quota')
        }
        
        return(list(method=mn, seats=o))
    }
