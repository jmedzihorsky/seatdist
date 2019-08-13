giveseats <-
function(v,
             ns,
             method,
             thresh=0,
             quota=NA,
             divs=NULL)
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
        } else if (meth %in% c('ne', 'nep', 'nor', 'nepal', 'norway', 'nepalese', 'norwegian') ) {
            o <- nepalese(votes=vn, m=ns)
            mn <- 'Nepalese/Norwegian'
        } else if (meth %in% c('sw', 'swe', 'sweden', 'swedish') ) {
            o <- swedish(votes=vn, m=ns)
            mn <- 'Swedish (new)'
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
        } else if (meth %in% c('pl', 'plur', 'plurality') ) {
            o <- steady(votes=vn, m=ns)
            mn <- 'Plurality'
        } else if (meth %in% c('de', 'dea','dean') ) {
            o <- dean(votes=vn, m=ns)
            mn <- 'Dean'
        } else if (meth %in% c('ts', 'thsch', 'theilschrage', 'theil-schrage') ) {
            o <- theilschrage(votes=vn, m=ns)
            mn <- 'Theil-Schrage'
        } else if (meth %in% c('ag', 'agnew', 'ossipoff') ) {
            o <- agnew(votes=vn, m=ns)
            mn <- 'Agnew'
        } else if (meth %in% c('ich', 'ichimori', 'ichimori 1/3', 'ichimori13')) {
            o <- ichimori13(votes=vn, m=ns)
            mn <- 'Ichimori 1/3'
        } else if (meth %in% c('custom')) {
            o <- customdivisor(votes=vn, m=ns, div=divs)
            mn <- 'user-supplied divisors'                        
        } else if (meth %in% c('lr', 'largest', 'remainders', 'largest remainders') ) {
            a <- lr(votes=vn, m=ns, qnm=quota) 
            o <- a$seats
            mn <- paste('Largest Remainders with', a$qn, 'quota')
        }
        
        if(sum(o)>ns) { warning('more seats awarded than allowed, check for ties') }

        return(list(method=mn, seats=o))
    }
