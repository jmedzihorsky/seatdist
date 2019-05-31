\name{giveseats}
\alias{giveseats}
\alias{adams}
\alias{danish}
\alias{dh}
\alias{divisormethod}
\alias{hh}
\alias{hungarian}
\alias{imperiali}
\alias{lr}
\alias{msl}
\alias{nohlen}
\alias{sl}
\title{Allocate indivisibilities}
\description{
    Function for proportional allocation of indivisibilities such as parliamentary seats
}
\usage{
    giveseats(v, ns, method, thresh = 0, quota = NA)
}
\arguments{
  \item{v}{numeric, vector of votes (claims)}
  \item{ns}{numeric, number of seats (indivisibilities) to allocate}
  \item{method}{character, name of the allocation algorithm to use (see Details)}
  \item{thresh}{numeric, threshold of exclusion; if in [0,1], treated as a fraction; 
    if in (1, 100), treated as a percent; if larger than 100, treated as a vote count}
  \item{quota}{character, quota for \code{method="largest remainders"}; see Details}
}
\details{
    Argument \code{method} takes the following values

    Divisor methods:
    \code{"dh"} for the D'Hondt method, for which the xth divisor value is x

    \code{"je"} for the Jefferson method which is equivalent to the D'Hondt method

    \code{"hb"} for the Hagenbach-Bischoff method which is equivalent to the D'Hondt method

    \code{"ad"} for the Adams method, for which the xth divisor equals x-1
      
    \code{"sd"} for the Smallest Divisors method, an alias of the Adams method 
  
    \code{"no"} for the Nohlen method, for which the xth divisor is x+1

    \code{"im"} for the Imperiali method, for which the xth divisor is (x+1)/2

    \code{"sl"} for the Sainte-Lague method, for which the xth divsor is 2x-1

    \code{"we"} for the Webster method which is equivalent to the Sainte-Lague method

    \code{"hu"} for the Hungarian Sainte-Lague method, which is identical to the Sainte-Lague method with the
        exception of the 1st divisor which equals to 1.5
    
    \code{"msl"} for the Modified Sainte-Lague method for which the 1st divisor is 1 and all the 
        subsequent divisors are (2x-1)5/7 

    \code{"da"} for the Danish method, for which the xth divisor is 3x-2
    
    \code{"hh"} for the Huntington-Hill method for which the xth divisor is \deqn{\sqrt{x(x-1)}}

    \code{"ep"} for the Equal Proportions method, an alias of the Huntington-Hill method


    Largest remainders method can be called with \code{method="lr"} but requires to set the
    \code{quota} argument to one of

    \code{"ha"} for the Hare quota e/l where e is the size of the number of votes and l the number of seats

    \code{"dr"} for the Droop quota \deqn{\left \lfloor 1 + \frac{e}{l+1} \right \rfloor}

    \code{"hb"} for the Hagenbach-Bischoff quota e/(l+1)

    \code{"im"} for the Imperiali quota e/(l+2)

    \code{"rei"} for the Reinforced Imperiali quota e/(l+3)
 
    Under the largest remainder method it is possible that more than the available number of seats will
    be assigned in the first round (under the Imperiali and Reinforced Imperiali quotas) in which case
    the funtion terminates with an error message.

}
\value{
    A named list of two items:
    \item{method}{character, name of the apportionment method used}
    \item{seats}{numeric vector with seats}
}
\references{
    Grilli di Cortona, Pietro, et al. 1999. Evaluation and Optimization of Electoral Systems. SIAM.

    Marcelino, Daniel. 2016. SciencesPo: A tool set for analyzing political behavior data. R package version 1.4.1. http://CRAN.R-project.org/package=SciencesPo.
}
\author{Juraj Medzihorsky}
%% \note{}
%% \seealso{}
\examples{
    seatdist::giveseats(v=c(A=60, B=28, C=12)*1e3, ns=1e1, 
                        method="lr", quota="hb", thresh=5e-2) 
                          
    #   thresh treated as a fraction
    #   $method
    #   "Largest Remainders with Hagenbach-Bischoff quota"
    
    #   $seats
    #   A B C
    #   6 3 1
}
%% \keyword{ apportionment }


