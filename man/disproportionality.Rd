\name{disproportionality}
\alias{disproportionality}
\alias{powind_shsh}
\alias{findpivot}
\title{
    Apportionment disproportionality
}
\description{
    Function to measure distance from proportionality for allocations of
    indivisibilities such as parliamentary seats.
}
\usage{
disproportionality(s, v, measure = "mixture", ignore_zeros = TRUE, k = 2,
                   eta = 2, alpha = 2, thresh = NULL, powind = "shapley shubik")
}
\arguments{
  \item{s}{numeric, vector of seats (allocated indivisiblities)}
  \item{v}{numeric, vector of votes (claims)}
  \item{measure}{character, name of the disproportionality measure; see Details.}
  \item{ignore_zeros}{logical: should parties with zero seats and votes be ignored?}
  \item{k}{numeric, k value for the Generalized Gallagher index (k-index)}
  \item{eta}{numeric, eta value for the Atkinson index}
  \item{alpha}{numeric, alpha value for the Generalized Entropy}
  \item{thresh}{numeric, threshold for parliamentary majority for the Fragnelli
      and the Gambarelli & Biella indexes}
  \item{powind}{character,  power  index  for  the  Fragnelli  and  the
      Gambarelli  & Biella indexes, defaults to the Shapley-Shubik
      index,\code{"shapley shubik"}. no other power indexes implemented yet.}
}
\details{
    Argument \code{measure} takes the following values

    \code{"dhondt"} for the D'Hondt index \deqn{\delta = \max_i \frac{s_i}{v_i}}
    
    \code{"monroe"} for the Monroe index \deqn{I_M = \sqrt{\frac{\sum_i (s_i-v_i)^2}{1+\sum_i v_i^2} }}
    
    \code{"maxdev"} for the Maximum Absolute Deviation \deqn{I_{MAD} = \max_i \left\{ |s_i-v_i| \right\} } 
    
    \code{"rae"} for the Rae index \deqn{I_{Rae} = \frac{1}{p} \sum_i |s_i-v_i| }
    
    \code{"loosemore hanby"} for the Loosemore & Hanby index  \deqn{I_{LH} = \frac{1}{2} \sum_i |s_i-v_i| }
    
    \code{"grofman"} for the Grofman index \deqn{I_{Grof} = \frac{1}{e} \sum_i |s_i-v_i| ; \; e = \frac{1}{\sum_i v_i^2} }
    
    \code{"lijphart"} for the Lijphart index \deqn{I_{L} = \frac{|s_a-v_a|+|s_b-v_b|}{2} ; \; v_a > v_b > \dots }
    
    \code{"gallagher"} for the Gallagher index \deqn{I_{Gal} = \sqrt{\frac{1}{2} \sum_i (s_i-v_i)^2 } }
    
    \code{"kindex"} for the Generalized Gallagher index aka k-index \deqn{I_{K} = \sqrt[k]{\frac{1}{k} \sum_i (s_i-v_i)^k } }

    \code{"gatev"} for the Gatev index \deqn{I_{Gat} = \sqrt{\frac{\sum_i (s_i-v_i)^2 }{\sum_i (s_i^2 + v_i^2)}} }
    
    \code{"ryabtsev"} for the Ryabtsev index \deqn{I_{Ryb} = \sqrt{\frac{\sum_i (s_i-v_i)^2 }{\sum_i (s_i + v_i)^2}} }
    
    \code{"szalai"} for the Szalai index \deqn{I_{Sz} = \sqrt{\frac{1}{p} \sum_i \left(\frac{s_i-v_i}{s_i+v_i}\right)^2 } }
    
    \code{"weighted szalai"} for the Weighted Szalai index \deqn{I_{WSz} = \sqrt{\frac{1}{2} \sum_i \frac{\left(s_i-v_i\right)^2}{s_i+v_i} } }
    
    \code{"aleskerov"} for the Aleskerov & Platonov index \deqn{I_{AP} = \frac{\sum_i k_i \frac{s_i}{v_i} }{\sum_i k_i }; \; k_i = \mathbf{1} \left(\frac{s_i}{v_i} >1 \right)  }

    \code{"gini"} for the Gini coefficient of inequality

    \code{"atkinson"} for the Atkinson index \deqn{I_{A} = 1 - \left[ \sum_i v_i \left(\frac{s_i}{v_i} \right)^{(1-\eta)} \right]^\frac{1}{1-\eta} }
    
    \code{"gen entropy"} for the Generalized Entropy index \deqn{I_{GE} = \frac{1}{\alpha^2-\alpha} \left[ \sum_i v_i \left(\frac{s_i}{v_i} \right)^\alpha -1 \right] }
    
    \code{"sainte lague"} for the Sainte-Laguë index \deqn{I_{SL} = \sum_i \frac{(s_i-v_i)^2}{v_i} }

    \code{"cox shugart"} for the Cox & Shugart index \deqn{I_{CS} = \frac{\sum_i (s_i-\bar{s})(v_i-\bar{v}) }{ \sum_i (v_i-\bar{v})^2 } }

    \code{"farina"} for the Farina index \deqn{I_{Far} = \arccos \left[ \frac{\sum_i s_i v_i }{ \sqrt{\sum_i s_i^2 \sum_i v_i^2 } } \right] \frac{10}{9} }

    \code{"ortona"} for the Ortona index \deqn{I_{O} = \frac{ \sum_i |s_i-v_i | }{ \sum_i |u_i - v_i| } ; \; u_i = \mathbf{1}(s_i = \max_i s_i ) }
    
    \code{"fragnelli"} for the Fragnelli index \deqn{I_{Frag} = \frac{1}{2} \sum_i |\varphi_i(s) -\varphi_i(v)| ; \; \varphi \mathrm{\; is \; Shapley-Shubik \; index} }

    \code{"gambarelli biella"} for the Gambarelli & Biella index \deqn{I_{GB} = \max_i \{ |s_i-v_i|,\, |\varphi_i(s) -\varphi_i(v)| \}}
  
    \code{"cosine"} for the Cosine Dissimilarity index \deqn{ I_{CD} = 1 - \frac{ \sum_i s_i v_i }{ \sqrt{\sum_i s_i^2} \sqrt{\sum_i v_i^2} } }

    \code{"mixture"} for the Mixture D'Hondt index \deqn{\pi^*_{DH} = 1 - \frac{1}{\max_i s_i/v_i} }

    Argument \code{powind} currently only takes a single value \code{"shapley shubik"} for the Shapley-Shubik index.

}
\value{
    A named list of two items:
    \item{measure}{character, the measure used}
    \item{distance}{numeric, distance from proportionality}
}
\references{
    Chessa, Michela, and Vito Fragnelli. 2012. "A note on 'Measurement of disproportionality in proportional representation systems'".Mathematical and Computer Modelling 55 (3): 1655–1660.

    Gallagher, Michael. 1991. "Proportionality, disproportionality and electoral systems". Electoral Studies 10 (1): 33–51.

    Grilli di Cortona, Pietro, et al. 1999. Evaluation and Optimization of Electoral Systems. SIAM.

    Karpov, Alexander. 2008. 'Measurement of disproportionality in proportional representation systems'. Mathematical and Computer Modelling 48 (9): 1421–1438.

    Loosemore, John, and Victor J Hanby. 1971. "The theoretical limits of maximum distortion: some analytic expressions for electoral systems". British Journal of Political Science 1 (4): 467–477.

    Marcelino, Daniel. 2016. SciencesPo: A tool set for analyzing political behavior data. R package version 1.4.1. http://CRAN.R-project.org/package=SciencesPo.

    Monroe, Burt L. 1994. "Disproportionality and malapportionment: Measuring electoral inequity". Electoral Studies 13 (2): 132–149.

    Rae, Douglas W. 1967. The Political Consequences of Electoral Laws. New Haven: Yale University Press.

    Sainte-Laguë, André. 1910. "La représentation proportionnelle et la méthode des moindres carrés". In Annales scientifiques de l’École Normale Supérieure, 27:529–542.
}
\author{
    Juraj Medzihorsky
}
%% \note{}
%% \seealso{}
\examples{
    seatdist::disproportionality(v=c(60,28,12)*1e3, 
                                 s=c(6,3,1), 
                                 measure="gallagher")
    #   $measure
    #   [1] "Gallagher"
    
    #   $distance
    #   [1] 0.02
}
%% \keyword{ disproportionality }
