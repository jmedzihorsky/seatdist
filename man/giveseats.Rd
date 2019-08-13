\name{giveseats}
\alias{giveseats}
\alias{adams}
\alias{agnew}
\alias{customdivisor}
\alias{danish}
\alias{dean}
\alias{dh}
\alias{divisormethod}
\alias{hh}
\alias{hungarian}
\alias{ichimori}
\alias{ichimori13}
\alias{imperiali}
\alias{lr}
\alias{msl}
\alias{nepalese}
\alias{nohlen}
\alias{plurality}
\alias{sl}
\alias{steady}
\alias{swedish}
\alias{theil}
\alias{theil-schrage}
\alias{theilschrage}
\title{Allocate indivisibilities}
\description{
    Function for proportional allocation of indivisibilities such as parliamentary seats
}
\usage{
    giveseats(v, ns, method, thresh = 0, quota = NA, divs = NULL)
}
\arguments{
  \item{v}{numeric, vector of votes (claims)}
  \item{ns}{numeric, number of seats (indivisibilities) to allocate}
  \item{method}{character, name of the allocation algorithm to use (see Details)}
  \item{thresh}{numeric, threshold of exclusion; if in [0,1], treated as a fraction; 
    if in (1, 100), treated as a percent; if larger than 100, treated as a vote coun}
  \item{quota}{character, quota for \code{method="largest remainders"}; see Details}
  \item{divs}{numeric, divisors for \code{method="custom"}, must be non-negative}
}
\details{
    Argument \code{method} takes the following values

    Divisor methods:
    \code{"dh"} for the D'Hondt method, for which the \eqn{x}th divisor value is \eqn{x}

    \code{"je"} for the Jefferson method which is equivalent to the D'Hondt method

    \code{"hb"} for the Hagenbach-Bischoff method which is equivalent to the D'Hondt method

    \code{"ad"} for the Adams method, for which the \eqn{x}th divisor equals \eqn{x-1}
      
    \code{"sd"} for the Smallest Divisors method, an alias of the Adams method 
  
    \code{"no"} for the Nohlen method, for which the \eqn{x}th divisor is \eqn{x+1}

    \code{"im"} for the Imperiali method, for which the \eqn{x}th divisor is \eqn{(x+1)/2}

    \code{"sl"} for the Sainte-Lague method, for which the \eqn{x}th divsor is \eqn{2x-1}

    \code{"we"} for the Webster method which is equivalent to the Sainte-Lague method

    \code{"sw"} for the (new) Swedish Sainte-Lague method, which is identical to the Sainte-Lague method with the
        exception of the 1st divisor which equals to 1.2

    \code{"ne"} for the Nepalese Sainte-Lague method, which is identical to the Sainte-Lague method with the
        exception of the 1st divisor which equals to 1.4

    \code{"nor"} for the Norwegian Sainte-Lague method, which is identical to the Sainte-Lague method with the
        exception of the 1st divisor which equals to 1.4

    \code{"hu"} for the Hungarian Sainte-Lague method, which is identical to the Sainte-Lague method with the
        exception of the 1st divisor which equals to 1.5
    
    \code{"msl"} for the Modified Sainte-Lague method for which the 1st divisor is 1 and all the 
        subsequent divisors are \eqn{(2x-1)5/7} 

    \code{"da"} for the Danish method, for which the \eqn{x}th divisor is \eqn{3x-2}
    
    \code{"hh"} for the Huntington-Hill method for which the \eqn{x}th divisor is \eqn{\sqrt{x(x-1)}}

    \code{"ep"} for the Equal Proportions method, an alias of the Huntington-Hill method

    \code{"pl"} for the Plurality (a.k.a. Steady) method (identic divisors) where the \eqn{x}th divisor is a constant (\eqn{x^0})

    \code{"de"} for the Dean method; the \eqn{x}th divisor is \eqn{x(x-1)/(x-0.5)}

    \code{"ts"} for the Theil-Schrage method (logarithmic mean divisors); the \eqn{x}th divisor is \deqn{\frac{1}{\ln{\frac{x}{x-1}}}}

    \code{"ag"} for the Agnew method (identric mean divisors); a.k.a. Theil, Ossipoff, Entropic; the \eqn{x}th divisor is \deqn{\frac{1}{e} \frac{x^x}{(x-1)^{x-1}}}

    \code{"ich"} for the Ichimori 1/3 method; the \eqn{x}th divisor is \eqn{\sqrt{x^2 + x + 1/3}}

    \code{"custom"} for user-supplied divisors (in argument \code{divs})


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
    \item{method}{character, the name of the apportionment method used}
    \item{seats}{numeric vector with seats}
}
\references{
    Agnew, Robert A. 2008. Optimal Congressional Apportionment. The American Mathematical Monthly 115 (4).

    Grilli di Cortona, Pietro, et al. 1999. Evaluation and Optimization of Electoral Systems. SIAM.

    Ichimori, T., 2010. New apportionment methods and their quota property. JSIAM Letters.

    Marcelino, Daniel. 2016. SciencesPo: A tool set for analyzing political behavior data. R package version 1.4.1. http://CRAN.R-project.org/package=SciencesPo.

    Wada, Junichiro. 2016. "Apportionment behind the veil of uncertainty". The Japanese Economic Review 67 (3): 348–360
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


