# seatdist

`seatdist` is a set of functions for 
- apportioning seats according to votes with a variety of methods
- evaluating seat apportionments with a variety of disproportionalities

It is developed from the 
[`SciencesPo'](https://github.com/danielmarcelino/SciencesPo) 
package, but 
offers more apportionemnt methods and measures,
and corrects some errors.

It is a companion package to
[Medzihorsky, Juraj. (2019). Understanding the D'Hondt Method. _PRX_.](https://doi.org/10.1080/2474736X.2019.1625712)


## Installation

You can install it using `devtools`

`library(devtools)`

`install_github("jmedzihorsky/seatdist")`


## Use

The main functions are

`giveseats()`

and

`disproportionality()`


## Approtionemnt Methods

### Divisor Methods

|Method|Formula|Sequence|
|------|:-----:|--------|
|D'Hondt|x|1,2,3,4,5,...|
|Jefferson|x|1,2,3,4,5,...|
|Hagenbach-Bischoff|x|1,2,3,4,5,...|
|Adams|x-1|0,1,2,3,4,...|
|Smallest Divisors|x-1|0,1,2,3,4|
|Nohlen|x+1|2,3,4,5,6,...|
|Imperiali|(x+1)/2|1,1.5,2,2.5,3,...|
|Sainte-Lague|2x-1|1,3,5,7,9,...|
|Webster|2x-1|1,3,5,7,9,...|
|Hungarian SL|2x-1;x>1|1.5,3,5,7,9,...|
|Modified SL|(2x-1)(5/7);x>1|1,2.14,3.57,5,6.43,...|
|Danish|3x-2|1,4,7,10,13,...|
|Huntington-Hill|sqrt(x(x-1))|0,1.41,2.45,3.46,4.47,...|
|Equal Proportions|sqrt(x(x-1))|0,1.41,2.45,3.46,4.47,...|


### Quota Methods

|Method|Formula|
|------|:-----:|
|Hare| V/S |
|Droop| trunc(1 + V/(S+1)) |
|Hagenbach-Bischoff| V/(S+1) |
|Imperiali| V/(S+2) |
|Reinforced Imperiali| V/(S+3) |


## Disproportionalities

You can find a table with the measures, formulas, and citations 
[here](https://github.com/jmedzihorsky/seatdist/blob/master/seatdist_info.pdf).



