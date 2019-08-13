# seatdist

`seatdist` is a set of functions for 
- apportioning seats according to votes with a variety of methods
- evaluating seat apportionments with a variety of disproportionalities

It is developed from the 
[`SciencesPo'](https://github.com/danielmarcelino/SciencesPo) 
package, but 
offers more apportionemnt methods (21) and measures (31),
and corrects some errors.

It is a companion package to
[Medzihorsky, Juraj. (2019). Rethinking the D'Hondt Method. _PRX_.](https://doi.org/10.1080/2474736X.2019.1625712)


## Installation

You can install it using `devtools`

`library(devtools)`

`install_github("jmedzihorsky/seatdist")`


## Use

The main functions are

`giveseats()`

and

`disproportionality()`


More details in the
[manual](https://github.com/jmedzihorsky/seatdist/blob/master/seatdist-manual.pdf).


## Approtionemnt Methods

### Divisor Methods

|Method|Formula|Sequence|
|------|:-----:|--------|
|Plurality|x^0|1,1,1,1,1,...|
|D'Hondt|x|1,2,3,4,5,...|
|Jefferson|x|1,2,3,4,5,...|
|Hagenbach-Bischoff|x|1,2,3,4,5,...|
|Adams|x-1|0,1,2,3,4,...|
|Smallest Divisors|x-1|0,1,2,3,4|
|Nohlen|x+1|2,3,4,5,6,...|
|Imperiali|(x+1)/2|1,1.5,2,2.5,3,...|
|Sainte-Lague|2x-1|1,3,5,7,9,...|
|Webster|2x-1|1,3,5,7,9,...|
|(new) Swedish SL|2x-1;x>1|1.2,3,5,7,9,...|
|Norwegian SL|2x-1;x>1|1.4,3,5,7,9,...|
|Nepalese SL|2x-1;x>1|1.4,3,5,7,9,...|
|Hungarian SL|2x-1;x>1|1.5,3,5,7,9,...|
|Modified SL|(2x-1)(5/7);x>1|1,2.14,3.57,5,6.43,...|
|Danish|3x-2|1,4,7,10,13,...|
|Huntington-Hill|sqrt(x(x-1))|0,1.41,2.45,3.46,4.47,...|
|Equal Proportions|sqrt(x(x-1))|0,1.41,2.45,3.46,4.47,...|
|Dean|x(x-1)/(x-0.5)|0,1.33,2.4,3.43,4.44,...|
|Theil-Schrage|1/(ln x - ln (x-1))|0,1.44,2.47,3.48,4.48,...|
|Agnew|(1/e) ((x^x) / ((x-1)^(x-1)))|0.37,1.47,2.48,3.49,4.49,...|
|custom| |user supplied|

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



