use "/Users/shirokuriwaki/Dropbox/cces_cumulative/data/source/cces/2012_cc.dta", clear

tostring inputstate, gen(state)
rename cdid dist112
rename cdid113 dist113

* look at only these 
keep V101 state dist11* inputzip CurrentHouseName HouseCand*Name HouseCand*Incumbent

* What the code suggests we do
gen house_open = 1
replace house_open = 0 if CurrentHouseName == HouseCand1Name | CurrentHouseName == HouseCand2Name |CurrentHouseName == HouseCand3Name

gen house_open_alt = (HouseCand1Incumbent == "0" & HouseCand2Incumbent == "0")

tab house_open house_open_alt

* What we get then
browse

* Too many open seats
tab house_open

* Zip 10026 is Harlem, which was NY-15 in 112 and NY-13 in 113
browse if inlist(inputzip, 10026, 10027)

* Sample of 400 allegedly "open seats"
set seed 02138
sample 400 if house_open_alt == 1, count
browse
