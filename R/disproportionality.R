disproportionality <-
function(s, 
             v, 
             measure='mixture', 
             ignore_zeros=TRUE,
             k=2,
             eta=2,
             alpha=2,
             thresh=NULL,
             powind='shapley shubik')
    {
        s <- as.vector(s)
        v <- as.vector(v)
        l_s <- length(s)
        l_v <- length(v)
        s_s <- sum(s)
        s_v <- sum(v)
        p_s <- s/s_s
        p_v <- v/s_v
        #   checks
        if (l_s!=l_v) { stop('unequal lenghts of s and v\n') }

        if (is.null(names(v))) {
            names(s) <- names(v) <- names(p_s) <- names(p_v) <- paste('p', 1:l_v, sep='_')
        }

        if (ignore_zeros) {
            fil_zero <- (p_s==0)&(p_v==0)
            p_s <- p_s[!fil_zero]
            p_v <- p_v[!fil_zero]
        }

        m <- measure

        if (m=='dhondt') { # 1                 
            o <- max(p_s/p_v)
            nm <- "D'Hondt"
        } else if (m=='monroe') { # 2
            o <- sqrt( sum((p_s-p_v)^2)/(1+sum(p_v^2)) )
            nm <- 'Monroe'
        } else if (m=='maxdev') { # 3
            o <- max(abs(p_s-p_v))
            nm <- 'Maximum Absolute Deviation'
        } else if (m=='rae') { # 4
            o <- (1/l_s) * sum(abs(p_s-p_v)) 
            nm <- 'Rae'
        } else if (m=='loosemore hanby') { # 5
            o <- (1/2)*sum(abs(p_s-p_v))
            nm <- 'Loosemore-Hanby'
        } else if (m=='grofman') { # 6
            e <- (1/sum(p_v^2))
            o <- (1/e) * sum(abs(p_s-p_v))
            nm <- 'Grofman'
        } else if (m=='lijphart') { # 7
            ord <- order(p_v)
            i1 <- ord[1]
            i2 <- ord[2]
            o <- ( abs(p_s[i1]-p_v[i1]) + abs(p_s[i2]-p_v[i2]) )/2
            nm <- 'Ljiphart'
        } else if (m=='gallagher') { # 8
            o <- sqrt((1/2) * sum((p_s-p_v)^2) )
            nm <- 'Gallagher'
        } else if (m=='kindex') { # 9
            o <- ((1/k) * sum((p_s-p_v)^k) )^(1/k)
            nm <- paste('Generalized Gallagher Index (k-Index) with k =', k)
        } else if (m=='gatev') { # 10
            o <- sqrt((sum((p_s-p_v)^2))/(sum((p_s^2+p_v^2))))
            nm <- 'Gatev'
        } else if (m=='ryabtsev') { # 11
            o <- sqrt((sum((p_s-p_v)^2))/(sum((p_s+p_v)^2)))
            nm <- 'Ryabtsev'
        } else if (m=='szalai') { # 12
            o <- sqrt( sum( ((p_s-p_v)/(p_s+p_v))^2) / l_s )
            nm <- 'Szalai'
        } else if (m=='weighted szalai') { # 25
            o <- sqrt( (1/2) * sum( ((p_s-p_v)^2)/(p_s+p_v) ) )
            nm <- 'Weighted Szalai Index'
        } else if (m=='aleskerov') { # 13
            fil <- ((p_s/p_v) > 1)
            nk <- sum(fil)
            o <- (1/nk) * sum((p_s/p_v)[fil])
            nm <- 'Aleskerov-Platonov'
        } else if (m=='gini') { # 14
            # ... alternative versions possible
            adv <- p_s/p_v
            ord <- order(adv)
            mod <- c(0, cumsum(p_v[ord]))
            obs <- c(0, cumsum(p_s[ord]))
            o <- 2*(sum(mod) - sum(obs))/length(p_v)
            nm <- paste('Gini coefficient of inequality')
        } else if (m=='atkinson') { # 15
            o <- 1 - ( sum ( p_v * (p_s/p_v)^(1-eta) )  )^(1/(1-eta))
            nm <- paste('Atkinson index with eta =', eta)
        } else if (m=='gen entropy') { # 16
            o <- (1/(alpha^2 - alpha)) * (sum(p_v * (p_s/p_v)^alpha ) -1 )
            nm <- paste('Generalized Entropy with alpha =', alpha)
        } else if (m=='sainte lague') { # 17
            o <- sum(((p_s-p_v)^2)/p_v)
            nm <- 'Sainte-Lague'
        } else if (m=='cox shugart') { # 18
            o <- sum((p_s - mean(p_s)) * (p_v - mean(p_v))) / sum((p_v - mean(p_v)) ^ 2)
            nm <- 'Cox & Shugart'
        } else if (m=='farina') { # 19
            o <- acos(sum(p_v*p_s) / (sum(p_v^2) * sum(p_s^2)) ^ (1/2)) * (10/9)
            nm <- 'Farina'
        } else if (m=='ortona') { # 20
            p_s_pp <- p_v # the user can supply whatever they thing is perfect proportionality
            p_s_u <- rep(0, length(p_s))
            p_s_u[which(p_s==max(p_s))[1]] <- sum(p_s)
            o <- sum(abs(p_s - p_s_pp))/sum(abs(p_s_u - p_s_pp))         
            nm <- 'Ortona'
        } else if (m=='fragnelli') { # 21
            # shapley-shubik index
            if (powind=='shapley shubik') {
                phi_v <- powind_shsh(p_v, threshold=thresh)
                phi_s <- powind_shsh(p_s, threshold=thresh)
                npi <- 'Shapley-Shubik power index'
            } else {
                stop('unsupported powind\n')
            }
            # later also Public Goods Index
            o <- sum(abs(phi_v - phi_s))/2        
            nm <- paste("Fragnelli index with", npi)
        } else if (m=='gambarelli biella') { # 22
            # shapley-shubik index
            if (powind=='shapley shubik') {
                phi_v <- powind_shsh(p_v, threshold=thresh)
                phi_s <- powind_shsh(p_s, threshold=thresh)
                npi <- 'Shapley-Shubik power index'
            } else {
                stop('unsupported powind\n')
            }
            # later also Public Goods Index
            o <- max( c( abs(p_v-p_s), abs(phi_v - phi_s) ) )
            nm <- paste("Gambarelli-Biella index with", npi)
        } else if (m=='cosine') { # 23
            o <- 1 - sum(p_s*p_v) / (sqrt(sum(p_s^2))*sqrt(sum(p_v^2)))
            nm <- "Cosine Dissimilarity"
        } else if (m=='mixture') { # 24
            o <- 1 - 1/max(p_s/p_v)
            nm <- "Mixture D'Hondt" 
        } else {
            stop('unsupported measure\n')
        }

        return(list(measure=nm, distance=o)) 
    }
