clear *

pwd

cd "/Users/robertayretonbaileysmith/OneDrive - Aalborg Universitet/AAU/02 PhD/07 Mortgage DK SFC model/04 Data"

log using statalog.txt, text replace

* Open the macro dataset from JST database 

use "JSTdatasetR4.dta"

* Inspect some of the variables

tab year
tab gdp

* Describe an individual variable

describe rgdpmad

* Get a quick summary of the full dataset

summarize

* Get the mean of a variable
mean cpi

* Conditional sum of all other variables if variable
sum if housing_capgain < 0

* Alter a variable on condition
gen cpi_1 = cpi

recode cpi_1 -1 = .

tab cpi_1

sum cpi_1


* If the data for example contained a gender variable

codebook tloans
rename gender female
recode female (1=0)(2=1)
label define fm 1 female 0 male
label values female fm

* Generating a new variable with a label
describe marital
encode marital, gen(marst)
label variable marst "marital status"

codebook marst
gen married = 1
replace married = 0 if marst != 3

* Drop a variable
drop marital

* Keep observations based on conditional
keep if age>=18



