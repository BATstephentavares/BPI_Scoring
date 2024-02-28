*********************************************
*		Child National					
*		BAT
* 		Master Data
*		Stephen Tavares
*		1/16/2024
*********************************************

clear all
set more off
frames reset

ssc install "xtgraph"

** 1. Create Working Directory Tags

global stephen_home "/Users/clotze/Library/CloudStorage/OneDrive-BurnoutAnticipationTechnology/Scoring Methods"

global stephen_laptop "/Users/stephentavares/Library/CloudStorage/OneDrive-BurnoutAnticipationTechnology/Scoring Methods"

global data "DataDownloads"

cd "$stephen_home"

** 2. Data Pull and Cleanse Temperature Check Data

/// 2a. Import data into the stata system.

import excel ///
	"$data/CNH_temp_check-20240115 - Copy.xlsx", ///
	clear firstrow

/// 2b. Timestamp issue.

	** Break time stamp into it's constituent parts. 
	
	gen yr = usubstr(timestamp, 1, 4)
	gen mm = usubstr(timestamp, 6, 2)
	gen dt = usubstr(timestamp, 9, 2)
	gen hr = usubstr(timestamp, 12, 2)
	gen min = usubstr(timestamp, 15, 2)
	gen sec = usubstr(timestamp, 18, 2)

	gen stringdate = mm + yr + dt

	destring yr mm dt hr min sec, replace

	drop timestamp

	gen timestamp = mdyhms(mm, dt, yr, hr, min, sec)
	
	** Develop a variable that addresses the day of the observation. 

	gen input_day = date(stringdate,"MYD")
	
	** Center the input day on August 20, 2023.
	
	gen start_date = date("08202320", "MYD")
	
	gen input_day_c = input_day - start_date
	
	** Develop a variable that address the week of the observation.

	gen input_week = week(input_day) 
	
	** Center variable on the week of start_date.
	
	gen start_week = week(start_date)

	gen input_week_c = input_week - start_week
	
	** Create variable describing the time of the day the observation is input.  
	** Morning - 5 AM - 12 PM (0)
	** Afternoon - 12 - 5 PM (1)
	** Evening - 5 - 9 PM (2)
	** Night - 9 PM - 5 AM (3)
	
	gen timeofday = .
	label var timeofday "Time of Day that the Observation Happened"
	
	replace timeofday = 0 if hr >= 5 & hr <12
	replace timeofday = 1 if hr >= 12 & hr <17
	replace timeofday = 2 if hr >= 17 & hr <21
	replace timeofday = 3 if hr >= 21 | hr <5
	
	label define tod 0 "Morning" 1 "Afternoon" 2 "Evening" 3 "Night"
	
	label values timeofday tod
	
	** Drop observations that occured prior to the start date. 
	
	drop if input_day_c < 0 
	
	** Drop variables no longer needed.
	
	drop yr - sec
	drop start_date start_week
	
	format timestamp %td
	
	** Reorder variables. 
	
	rename userId userid
	rename groupList grouplist
	
	order company userid grouplist timestamp stringdate input_day ///
	input_day_c input_week input_week_c timeofday
	
	
/// 2c. Recode variables to numeric. 

	** Recode Temperature. 
	
	encode temperature, gen(temp) 
	
	recode temp (1 = 3)(2 = 2)(3 = 1)(4 = 0)
	
	label define temp 0 "Thriving" 1 "Surviving" 2 "Struggling" ///
	3 "In-Crisis", modify
	
	label values temp temp

	drop temperature
	
	rename temp temperature
	
	** Recode Reason

	tab reason, gen(reason_m)

	drop G - N
	
/// 2d. Reorder the variables and then save data file

	order company userid grouplist timestamp stringdate input_day ///
	input_day_c input_week input_week_c timeofday temperature
	
	
/// 2e. Collapse so the reasons are in one row per time. 

	drop reason 

	collapse (mean) reason* timeofday temperature, by(userid grouplist ///
	input_day_c input_week_c stringdate)
	
	** Replace variables and relabel.
	
	*** Temperature Means.
	
	replace temperature = 3 if temperature >= 2.5
	replace temperature = 2 if temperature >= 1.5 & temperature < 2.5
	replace temperature = 1 if temperature < 1.5 & temperature >= 0.5
	replace temperature = 0 if temperature < 0.5 
	
	label values temperature temp
	label var temperature "Temperature Check Score"
	
	*** Time of Day.
	
	replace timeofday = 3 if timeofday >= 2.5
	replace timeofday = 2 if timeofday >= 1.5 & timeofday < 2.5
	replace timeofday = 1 if timeofday < 1.5 & timeofday >= 0.5
	replace timeofday = 0 if timeofday < 0.5 
	
	label values timeofday tod

	*** Reasons.
		
	forvalues z = 1(1)19{
		
		replace reason_m`z' = 1 if reason_m`z' >0
		
	}
	
	la var reason_m1 "Able to communicate effectively"
	la var reason_m2 "Able to focus"
	la var reason_m3 "Able to take feedback and to adjust to changes of plans"
	la var reason_m4 "Able to take things in stride"
	la var reason_m5 "Activities and relationships you used to enjoy seem less interesting or even stressful"
	la var reason_m6 "Avoiding interaction with coworkers"
	la var reason_m7 "Calm and steady with minor mood fluctuations."
	la var reason_m8 "Consistent performance"
	la var reason_m9 "Easily enraged or aggressive" 
	la var reason_m10 "Exhaustion"
	la var reason_m11 "Inconsistent performance" 
	la var reason_m12 "Increased need for control and difficult adjusting to changes"
	la var reason_m13 "More easily overwhelmed or irritated"
	la var reason_m14 "Muscle tension"
	la var reason_m15 "Nervousness"
	la var reason_m16 "Normal sleep patterns and appetite"
	la var reason_m17 "Panic Attacks"
	la var reason_m18 "Persistent fear"
	la var reason_m19 "Poor performance and difficulty making decisions or concentrating"
	la var reason_m20 "Restless"
	la var reason_m21 "Trouble sleeping or eating"
	
	
	save "${data}/Temperature", replace


** 3. Data Pull and Cleanse Journal Data.

/// 3a. Import data into the stata system.

import excel ///
	"$data/CNH_journal_slider_user_data_expanded_receptiviti-20240116_with_columns - Copy.xlsx", ///
	clear firstrow

	rename userId userid
	rename groupList grouplist
	
/// 3b. Timestamp issue.

	** Break time stamp into it's constituent parts. 
	
	gen yr = usubstr(timestamp, 1, 4)
	gen mm = usubstr(timestamp, 6, 2)
	gen dt = usubstr(timestamp, 9, 2)
	gen hr = usubstr(timestamp, 12, 2)
	gen min = usubstr(timestamp, 15, 2)
	gen sec = usubstr(timestamp, 18, 2)

	gen stringdate = mm + yr + dt

	destring yr mm dt hr min sec, replace

	drop timestamp

	gen timestamp = mdyhms(mm, dt, yr, hr, min, sec)
	
	** Develop a variable that addresses the day of the observation. 

	gen input_day = date(stringdate,"MYD")
	
	** Center the input day on August 20, 2023.
	
	gen start_date = date("08202320", "MYD")
	
	gen input_day_c = input_day - start_date
	
	** Develop a variable that address the week of the observation.

	gen input_week = week(input_day) 
	
	** Center variable on the week of start_date.
	
	gen start_week = week(start_date)

	gen input_week_c = input_week - start_week
	
	** Create variable describing the time of the day the observation is ///
	** input.  
	** Morning - 5 AM - 12 PM (0)
	** Afternoon - 12 - 5 PM (1)
	** Evening - 5 - 9 PM (2)
	** Night - 9 PM - 5 AM (3)
	
	gen timeofday = .
	label var timeofday "Time of Day that the Observation Happened"
	
	replace timeofday = 0 if hr >= 5 & hr <12
	replace timeofday = 1 if hr >= 12 & hr <17
	replace timeofday = 2 if hr >= 17 & hr <21
	replace timeofday = 3 if hr >= 21 | hr <5
	
	label define tod 0 "Morning" 1 "Afternoon" 2 "Evening" 3 "Night"
	
	label values timeofday tod
	
	** Drop observations that occured prior to the start date. 
	
	drop if input_day_c < 0 
	
	** Drop variables no longer needed.
	
	drop yr - sec
	drop start_date start_week
	
	format timestamp %td
	
	** Reorder variables. 
	
	order company userid grouplist timestamp stringdate input_day ///
	input_day_c	input_week input_week_c timeofday
	
	cap drop answers - KP
	drop receptivitiId response_id request_id language version
	
/// 3c. Create the Burn Out Sum Score.

	** Encode the slider values. 
	
	encode overwhelmed, gen(overwhelmed_n)
	encode drained, gen(drained_n)
	encode sluggish, gen(sluggish_n)
	encode detached, gen(detached_n)

	recode overwhelmed_n - detached_n (1 = 3)(2 = 0)(3 = 2)(4 = 1)
	
	label define bo 0 "Never" 1 "Sometimes" 2 "Often" 3 "Always", modify

	label values overwhelmed_n - detached_n bo
	
	drop overwhelmed - detached
	
	rename overwhelmed_n overwhelmed
	rename drained_n drained
	rename sluggish_n sluggish
	rename detached_n detached
	
	** Generate BO Sum Score
	
	gen BO = overwhelmed + drained + sluggish + detached
	
	label var BO "Burnout Sum Score"
	
	** Re-order data set. 
	
	order company userid grouplist timestamp stringdate input_day ///
	input_day_c input_week input_week_c timeofday ///
	drained sluggish detached overwhelmed BO
	
/// 3d. Save data set. 

	save "${data}/BurnoutSliders", replace
	
** 4. Merge data sets together.
	
	merge m:m userid input_day_c using "${data}/Temperature"
	
	** Ceate a panel id number for each individual.
	
	egen panel = group(userid)
	
	** Re-order variables
	
	order userid panel input_week_c input_day_c timeofday temperature BO ///
	overwhelmed drained sluggish detached 
	
	drop _merge
	
	save "${data}/BurnTemp", replace
	
/// 5. import scores

	** 5a. Import scores data frame

	import delimited "$data/scores.csv", clear

	** 5b. Cleanse data to match other BO datasets 

	** Rename variables to be consistent

	rename cognitoidentityid userid
	rename created_time timestamp 
	
	drop response_id language version
	
	gen yr = usubstr(timestamp, 1, 4)
	gen mm = usubstr(timestamp, 6, 2)
	gen dt = usubstr(timestamp, 9, 2)
	gen hr = usubstr(timestamp, 12, 2)
	gen min = usubstr(timestamp, 15, 2)
	gen sec = usubstr(timestamp, 18, 2)

	gen stringdate = mm + yr + dt

	destring yr mm dt hr min sec, replace

	drop timestamp

	gen timestamp = mdyhms(mm, dt, yr, hr, min, sec)
	
	** Develop a variable that addresses the day of the observation. 

	gen input_day = date(stringdate,"MYD")
	
	** Center the input day on August 20, 2023.
	
	gen start_date = date("08202320", "MYD")
	
	gen input_day_c = input_day - start_date
	
	** Develop a variable that address the week of the observation.

	gen input_week = week(input_day) 
	
	** Center variable on the week of start_date.
	
	gen start_week = week(start_date)

	gen input_week_c = input_week - start_week
	
	** Create variable describing the time of the day the observation is ///
	** input.  
	** Morning - 5 AM - 12 PM (0)
	** Afternoon - 12 - 5 PM (1)
	** Evening - 5 - 9 PM (2)
	** Night - 9 PM - 5 AM (3)
	
	gen timeofday = .
	label var timeofday "Time of Day that the Observation Happened"
	
	replace timeofday = 0 if hr >= 5 & hr <12
	replace timeofday = 1 if hr >= 12 & hr <17
	replace timeofday = 2 if hr >= 17 & hr <21
	replace timeofday = 3 if hr >= 21 | hr <5
	
	label define tod 0 "Morning" 1 "Afternoon" 2 "Evening" 3 "Night"
	
	label values timeofday tod
	
	** Drop observations that occured prior to the start date. 
	
	drop if input_day_c < 0 
	
	** Drop variables no longer needed.
	
	drop yr - sec
	drop start_date start_week
	
	format timestamp %td
	
	order userid stringdate timestamp input_day input_day_c input_week ///
			input_week_c timeofday
			
	** Save as dta
	
	save "${data}/LIWCScores", replace

** 6. Merge data sets together.
	
	merge m:m userid input_day_c using "${data}/BurnTemp"

	** Re-order variables
	
	order userid panel input_week_c input_day_c timeofday temperature BO ///
	overwhelmed drained sluggish detached 
	
	save "${data}/Full", replace
	
	keep stringdate input_day_c userid panel timeofday temperature BO overwhelmed drained sluggish detached toxicity_likelihoodtoxicity liwc_extensionlow_empathy  liwccognitive_processes liwcsocial_processes liwcaffective_processes liwcemotional_tone personalityempathetic personalitystress_prone personalityanxiety_prone personalityneuroticism personalityconscientiousness personalityenergetic
	
	save "${data}/forR", replace
