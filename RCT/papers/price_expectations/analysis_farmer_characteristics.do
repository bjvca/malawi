clear
clear matrix
set more off
capture log close

***
/*******************************************************************************************
Project: Malawi RCT 2022/2023

This do-file generates tables for characterising the surveyed farmers and their households

Last modified: By Leocardia, on 28 June, 2022

**********************************************************************************************/
*setting global path names
clear all
set maxvar 32000
set more off
capture log close
pause on

global path "C:\git\malawi_rct\malawi\RCT"
global rawdata "$path\baseline\data\public"
global outpath "$path\papers\price_expectations\results"
global outfile "farmer_charactristics"


// import in raw data in csv file

import delimited C:\git\malawi_rct\malawi\RCT\baseline\data\public\baseline_data.csv, varnames(1) clear

//trim variables to only (1) farmer_id, (2) variables that define farmemr/household characteristics

keep farmer_id q12-q23 assetsq33-livestockq38i
label var q12 "Respondent is household head"
recode q12 (1=1) (nonmissing = 0)

// sex of household head
des q13 q16
replace q13 = "1" if q13 == "Male"
replace q13 = "2" if q13 == "Female"
replace q16 = "1" if q16 == "Male"
replace q16 = "2" if q16 == "Female"
destring q13 q16, ignore ("n/a") replace
label define sex 1 "Male"  2 "Female"
label values q13 q16 sex
gen sex_hh_head = q13, after (q12)
tab1 sex_hh_head q13
replace sex_hh_head= q16 if q12 == 0
recode sex_hh_head (1=1) (nonmissing = 0)
label var sex_hh_head "Household head is a male"
rename sex_hh_head male_headed_hhs_percentage
drop q13 q16
mean male_headed_hhs_percentage

// age of household head
des q14 q17
replace q14 = . if q14 == 999
destring q17, ignore ("n/a") replace
replace q17 = . if q17 == 999
gen mean_age_of_household_head = q14, after (male_headed_hhs_percentage)
replace mean_age_of_household_head = q17 if q12 == 0
mean mean_age_of_household_head
sum mean_age_of_household_head q14 q17
drop q14 q17

// years of schooling
des q15 q18

replace q15 = . if q15 > 17
destring q18, ignore ("n/a") replace
replace q18 = . if q18 > 17
gen mean_years_of_schooling = q15, after (mean_age_of_household_head)
replace mean_years_of_schooling = q18 if q12 == 0
mean mean_years_of_schooling
sum mean_years_of_schooling q15 q18
drop q15 q18

// household size
replace q19 = . if q19 > 15 // removing 4 outliers 
label var q19 "Household size (number of people)"

// type of roof on the main house
label define roof 1 "Grass Thatch" 2 "Corrugated Iron sheets" 3 "Tiles" 96 "Other material"
label values q20 roof
gen roof_is_grass_thatch = q20, after ( mean_years_of_schooling )
gen roof_is_iron_sheets = q20, after ( roof_is_grass_thatch )
recode roof_is_grass_thatch (1=1) (nonmissing = 0)
recode roof_is_iron_sheets (2=1) (nonmissing = 0)
tab q20
sum roof_is_grass_thatch roof_is_iron_sheets
drop q20

// rooms in the house
label var q21 "rooms_in_the_house"
rename q21 rooms_in_the_house
sum rooms_in_the_house

// Distance to nearest all weather road
label var q22 "Distance (kms) to nearest all weather road"
rename q22 kms_to_nearest_allweather_road
replace kms_to_nearest_allweather_road = . if kms_to_nearest_allweather_road > 99 // removing outliers and 999 
sum kms_to_nearest_allweather_road
mean kms_to_nearest_allweather_road

// Distance to nearest market
label var q23 "Distance (kms) to nearest market"
rename q23 mean_kms_to_nearest_market
replace mean_kms_to_nearest_market = . if mean_kms_to_nearest_market > 99 // removing outliers and 999
sum mean_kms_to_nearest_market
mean mean_kms_to_nearest_market

// household access to assets
replace assetsq33 = "1" if assetsq33 == "Yes"
replace assetsq33 = "2" if assetsq33 == "No"

replace assetsq34 = "1" if assetsq34 == "Yes"
replace assetsq34 = "2" if assetsq34 == "No"

replace assetsq35 = "1" if assetsq35 == "Yes"
replace assetsq35 = "2" if assetsq35 == "No"

replace assetsq36 = "1" if assetsq36 == "Yes"
replace assetsq36 = "2" if assetsq36 == "No"

replace assetsq37 = "1" if assetsq37 == "Yes"
replace assetsq37 = "2" if assetsq37 == "No"


rename assetsq33 access_to_a_bicycle
rename assetsq34 access_to_a_saloon_car
rename assetsq35 access_to_a_pickup_or_lorry
rename assetsq36 access_to_an_ox_cart
rename assetsq37 households_owns_motorbike

destring access_to_a_bicycle - households_owns_motorbike, ignore ("n/a") replace
recode access_to_a_bicycle - households_owns_motorbike (1=1) (nonmissing =0)
sum access_to_a_bicycle- households_owns_motorbike

// household ownership of livestiock
rename livestockq38a Number_bulls_oxen
rename livestockq38b Number_cows_or_heifers
rename livestockq38c Number_calves
rename livestockq38d Number_pigs
rename livestockq38e Number_goats
rename livestockq38f Number_sheep
rename livestockq38g Number_chicken
rename livestockq38h Number_ducks
rename livestockq38i Number_other_livestock

// removing outliers
tab Number_bulls_oxen
replace Number_bulls_oxen = . if Number_bulls_oxen > 8
tab Number_cows_or_heifers
replace Number_cows_or_heifers = . if Number_cows_or_heifers >18
tab Number_calves
replace Number_calves = . if Number_calves > 8
tab Number_pigs
tab Number_goats
replace Number_goats = . if Number_goats > 27
tab Number_sheep
replace Number_sheep=. if Number_sheep > 8
tab Number_chicken
replace Number_chicken = . if Number_chicken > 65
tab Number_ducks
replace Number_ducks = . if Number_ducks >19
tab Number_other_livestock
drop Number_other_livestock

label var male_headed_hhs_percentage "Household head is male"
label var mean_age_of_household_head "Age of household head (years)"
label var mean_years_of_schooling "Years of schooling of household head"
label var roof_is_grass_thatch "Roof of main building is grass thatch"
label var roof_is_iron_sheets "Roof of main building is corrugated iron"
label var access_to_a_bicycle "Household has access to bicycle"
label var access_to_a_saloon_car "Household has access to saloon car"
label var access_to_a_pickup_or_lorry "Household has access to pick-up or lorry access"
label var access_to_an_ox_cart "Household has access to ox-cart"
label var households_owns_motorbike "Household owns a motorbike"
label var Number_bulls_oxen "Number of bulls/oxen/steers owned by household"
label var Number_cows_or_heifers "Number of cows or heifers owned by household"
label var Number_calves "Number of calves owned by household"
label var Number_pigs "Number of pigs owned by household"
label var Number_goats "Number of goats owned by household"
label var Number_sheep "Number of sheep owned by household"
label var Number_chicken "Number of chicken owned by household"
label var Number_ducks "Number of ducks owned by household"
label var rooms_in_the_house "Number of rooms in the house"

global title "respondent and household characteristics"
sumx male_headed_hhs_percentage- Number_ducks, stat(mean sd N) word dec(3)
