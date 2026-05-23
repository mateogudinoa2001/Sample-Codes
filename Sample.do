/*
Mateo Gudiño Alvarado
Work Performed under Dr. Emilio Gutiérrez and Dr. Kensuke Teshima at ITAM
*/

* NOTE: Relative paths are ommitted here as requested


* Cleanup of homicide databases from INEGI
* Conversion of the .csv files to .dta files is ommitted here.

use "C:\Users\mateo\Dropbox\Kantar\Raw\defunciones_generales_2012.dta", clear

append using "C:\Users\mateo\Dropbox\Kantar\Raw\defunciones_generales_2013.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\defunciones_generales_2014.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\defunciones_generales_2015.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\defunciones_generales_2016.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\conjunto_de_datos_defunciones_generales_2017.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\conjunto_de_datos_defunciones_registradas_2020.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\conjunto_de_datos_defunciones_registradas_2018.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\conjunto_de_datos_defunciones_registradas_2019.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\conjunto_de_datos_defunciones_registradas_2021.dta"
append using "C:\Users\mateo\Dropbox\Kantar\Raw\conjunto_de_datos_defunciones_registradas_2022.dta"

append using "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\conjunto_de_datos_defunciones_registradas_2023_csv\conjunto_de_datos\conjunto_de_datos_defunciones_registradas_2023_csv.dta"
save "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\conjunto_de_datos_defunciones_panel.dta", replace

use "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\conjunto_de_datos_defunciones_panel.dta"

append using "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\defunciones_generales_base_datos_2010_2014_dbf\defunciones_base_datos_2011_dbf\DEFUN11_2.dta"

append using "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\defunciones_generales_base_datos_2010_2014_dbf\defunciones_base_datos_2010_dbf\DEFUN10_2.dta"

keep dia_ocurr mes_ocurr anio_ocur dia_regis mes_regis anio_regis mun_regis ent_regis mun_ocurr ent_ocurr lista_mex gr_lismex

save "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\conjunto_de_datos_defunciones_panel.dta", replace

* Join municipal and state id codes

egen mun_ocurr_temp = group(mun_ocurr)
gen mun_ocurr_string = string(mun_ocurr_temp, "%03.0f")
drop mun_ocurr_temp

egen ent_ocurr_temp = group(ent_ocurr)
gen ent_ocurr_string = string(ent_ocurr_temp, "%02.0f")
drop ent_ocurr_temp

gen ubicageo = ent_ocurr_string + mun_ocurr_string

drop ent_ocurr_string
drop mun_ocurr_string

egen mun_regis_temp = group(mun_regis)
gen mun_regis_string = string(mun_regis_temp, "%03.0f")
drop mun_regis_temp

egen ent_regis_temp = group(ent_regis)
gen ent_regis_string = string(ent_regis_temp, "%02.0f")
drop ent_regis_temp

gen ubicageo_regis = ent_regis_string + mun_regis_string

drop ent_regis_string
drop mun_regis_string

*Filter for homicides


keep if gr_lismex == "E55" | gr_lismex == "55"
drop gr_lismex


* Cleanup of dates as a string (same format as Kantar)

gen diaocurrstring = string(dia_ocurr, "%02.0f")
gen mesocurrstring = string(mes_ocurr, "%02.0f")
gen anioocurstring = string(anio_ocur)
gen diaregisstring = string(dia_regis, "%02.0f")
gen mesregisstring = string(mes_regis, "%02.0f")
gen anioregisstring = string(anio_regis)


* For homicides that have month and year of ocurrence or registration, but not day of ocurrence or registration,
* we input the last day of the month.
replace diaocurrstring = "28" if diaocurrstring == "99" & mesocurrstring == "02"
replace diaocurrstring = "31" if diaocurrstring == "99" & mesocurrstring == "01"
replace diaocurrstring = "31" if diaocurrstring == "99" & mesocurrstring == "03"
replace diaocurrstring = "31" if diaocurrstring == "99" & mesocurrstring == "05"
replace diaocurrstring = "31" if diaocurrstring == "99" & mesocurrstring == "07"
replace diaocurrstring = "31" if diaocurrstring == "99" & mesocurrstring == "09"
replace diaocurrstring = "31" if diaocurrstring == "99" & mesocurrstring == "11"
replace diaocurrstring = "30" if diaocurrstring == "99" & mesocurrstring == "04"
replace diaocurrstring = "30" if diaocurrstring == "99" & mesocurrstring == "06"
replace diaocurrstring = "30" if diaocurrstring == "99" & mesocurrstring == "08"
replace diaocurrstring = "30" if diaocurrstring == "99" & mesocurrstring == "10"
replace diaocurrstring = "30" if diaocurrstring == "99" & mesocurrstring == "12"

replace diaregisstring = "28" if diaregisstring == "99" & mesregisstring == "02"
replace diaregisstring = "31" if diaregisstring == "99" & mesregisstring == "01"
replace diaregisstring = "31" if diaregisstring == "99" & mesregisstring == "03"
replace diaregisstring = "31" if diaregisstring == "99" & mesregisstring == "05"
replace diaregisstring = "31" if diaregisstring == "99" & mesregisstring == "07"
replace diaregisstring = "31" if diaregisstring == "99" & mesregisstring == "09"
replace diaregisstring = "31" if diaregisstring == "99" & mesregisstring == "11"
replace diaregisstring = "30" if diaregisstring == "99" & mesregisstring == "04"
replace diaregisstring = "30" if diaregisstring == "99" & mesregisstring == "06"
replace diaregisstring = "30" if diaregisstring == "99" & mesregisstring == "08"
replace diaregisstring = "30" if diaregisstring == "99" & mesregisstring == "10"
replace diaregisstring = "30" if diaregisstring == "99" & mesregisstring == "12"


* We drop observations without unknown month or year of ocurrence
drop if mesocurrstring == "99"
drop if anioocurstring == "9999"

gen fecha_ocurr = anioocurstring+"-"+mesocurrstring+"-"+diaocurrstring
gen fecha_regis = anioregisstring+"-"+mesregisstring+"-"+diaregisstring
gen date_ocurr = date(fecha_ocurr, "YMD")
gen date_regis = date(fecha_regis, "YMD")



* Dates filter (same time span as Kantar)
*drop if date_ocurr < date("2010-01-01", "YMD") | date_ocurr > date("2015-12-31", "YMD")

save "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\conjunto_de_datos_defunciones_panel_updt2.dta", replace


drop diaocurrstring
drop mesocurrstring
drop anioocurstring
drop diaregisstring
drop mesregisstring
drop anioregisstring

drop ent_regis
drop ent_ocurr
drop mun_regis
drop mun_ocurr
drop lista_mex
drop dia_ocurr
drop mes_ocurr
drop anio_ocur
drop dia_regis
drop mes_regis
drop anio_regis
drop fecha_regis


* First we generate a database with all observations that have day of ocurrence, even without date of registration

gen homicidios = .
bysort ubicageo date_ocurr: gen count_fecha = _N
by ubicageo date_ocurr: replace homicidios = count_fecha if homicidios == .
duplicates drop ubicageo date_ocurr, force
drop count_fecha

save "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\conjunto_de_datos_defunciones_panel_updt1.dta", replace

break






clear

use "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_sincorroborar_todafecha.dta"

replace homicidios = 0 if missing(homicidios)
drop ubicageo_regis
drop fecha_ocurr

save "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_sincorroborar_todafecha.dta", replace




* Now we perform the same procedure only for observations with both date of ocurrence and registration which are no longer than 5 years apart from each other (Kantar's timespan)
clear

use "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\conjunto_de_datos_defunciones_panel_updt2.dta", clear
gen diff_anios = abs(yofd(date_ocurr)-yofd(date_regis))


drop if diff_anios > 5
drop if missing(diff_anios)
drop diff_anios

drop diaocurrstring
drop mesocurrstring
drop anioocurstring
drop diaregisstring
drop mesregisstring
drop anioregisstring

drop ent_regis
drop ent_ocurr
drop mun_regis
drop mun_ocurr
drop lista_mex
drop dia_ocurr
drop mes_ocurr
drop anio_ocur
drop dia_regis
drop mes_regis
drop anio_regis
drop fecha_regis


* We create a column with a homicide counter for the second base

gen homicidios = .
bysort ubicageo date_ocurr: gen count_fecha = _N
by ubicageo date_ocurr: replace homicidios = count_fecha if homicidios == .
duplicates drop ubicageo date_ocurr, force
drop count_fecha


save "C:\Users\mateo\Dropbox\Kantar\Defunciones_INEGI\conjunto_de_datos_defunciones_panel_updt2.dta", replace





*======================================================================================================================================

clear

use "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_corroborados_todafecha.dta"

replace homicidios = 0 if missing(homicidios)
drop ubicageo_regis
drop fecha_ocurr

save "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_corroborados_todafecha.dta", replace


* Cleaning of the household id database with INEGI municipality code.

clear


* =========================================================================================
import delimited "C:\Users\mateo\Dropbox\Kantar\Raw\XW_iddomicilio_location.csv"

keep iddomicilio cve_ent cve_mun

gen ent = string(cve_ent, "%02.0f")
gen mun = string(cve_mun, "%03.0f")
gen ubicageo = ent + mun

drop cve_ent cve_mun ent mun

save "C:\Users\mateo\Dropbox\Kantar\Code\dicc_iddomicilio_mun.dta"
* =============================================================================================================


* Aggregate weekly corroborated homicides

use "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_corroborados_todafecha.dta", clear

gen week_sunday = date_ocurr - dow(date_ocurr) + 6

collapse (sum) homicidios, by(ubicageo week_sunday)

rename homicidios homicidios_corroborados

save "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_corroborados_todafecha.dta", replace

clear


* Aggregate weekly uncorroborated homicides

use "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_sincorroborar_todafecha.dta"

gen week_sunday = date_ocurr - dow(date_ocurr) + 6

collapse (sum) homicidios, by(ubicageo week_sunday)

save "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_sincorroborar_todafecha.dta\homicidios_sincorroborar_semanales.dta", replace

clear


* Aggregate monthly uncorroborated homicides

use "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_corroborados_todafecha.dta", clear

gen mesano = string(month(date_ocurr), "%02.0f") + "-" + string(year(date_ocurr))

collapse (sum) homicidios, by (mesano)

gen fecha = date(mesano, "MY")




* Aggregate weekly corroborated homicides

use "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_corroborados_todafecha.dta"

gen week_sunday = date_ocurr - dow(date_ocurr) + 6

collapse (sum) homicidios, by(ubicageo week_sunday)

rename homicidios homicidios_corroborados

save "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_corroborados_semanales.dta", replace

clear


* Aggregate weekly uncorroborated homicides

use "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_sincorroborar_todafecha.dta", clear
*drop _merge
gen week_sunday = date_ocurr - dow(date_ocurr) + 6

gen mesano = date(string(month(date_ocurr), "%02.0f") + "-" + string(year(date_ocurr)), "MY")
sort mesano 

collapse (sum) homicidios, by(mesano)
sort mesano
gen periodo = string(month(mesano), "%02.0f") + "/" + string(year(mesano))

drop if mesano < 18628


save "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_sincorroborar_todafechaFINAL.dta", replace



* ===================================================================================================================

* A quick cleanup of the 2020 population database

 use "C:\Users\mateo\Dropbox\Kantar\Raw\Población y vivienda\conjunto_de_datos_iter_00CSV20.dta", clear

* Generate ubicageo variable (INEGI geographic location code)
* Join municipal and state id codes

*egen mun_ocurr_temp = group(cve_mun)
gen mun_ocurr_string = string(mun, "%03.0f")
*drop mun_ocurr_temp

*egen ent_ocurr_temp = group(cve_ent)
gen ent_ocurr_string = string(entidad, "%02.0f")
*drop ent_ocurr_temp

gen ubicageo = ent_ocurr_string + mun_ocurr_string

drop ent_ocurr_string
drop mun_ocurr_string

drop entidad
drop mun

keep ubicageo pobtot

* The reason the database is collapsed here is that the aggregation level is at the locality level (which is under municipality,
* so we have to reaggregate at the municipality level)
collapse (sum) pobtot, by(ubicageo)

save "C:\Users\mateo\Dropbox\Kantar\Raw\Población y vivienda\population_clean.dta", replace



* ===================================================================================================================


* Same database for corroborated homicides but aggregated at the monthly level


* Aggregate weekly uncorroborated homicides

use "C:\Users\mateo\Dropbox\Kantar\Code\homicidios_corroborados_todafecha.dta", clear
*drop _merge
*gen week_sunday = date_ocurr - dow(date_ocurr) + 6

gen mesano = date(string(month(week_sunday), "%02.0f") + "-" + string(year(week_sunday)), "MY")
sort mesano 

merge m:1 ubicageo using "C:\Users\mateo\Dropbox\Kantar\Raw\Población y vivienda\population_clean.dta"

collapse (sum) homicidios pobtot, by(mesano)
sort mesano
gen period = string(month(mesano), "%02.0f") + "/" + string(year(mesano))

drop if mesano < 18628
drop if mesano == 23376
* The latter is before jan 1, 2011

gen hom_percap_nacional = (homicidios / pobtot) * 100000
drop if missing(mesano)


* Requested plots:


twoway (scatter hom_percap_nacional mesano), xtitle("Period") ytitle("Monthly Homicide rate") title("Evolution of Monthly Corroborated Homicides per 10,000 People in Mexico") xlabel(18628 "01/2011" 18779 "06/2011" 18993 "01/2012" 19145 "06/2012" 19359 "01/2013" 19510 "06/2013" 19724 "01/2014" 19875 "06/2014" 20089 "01/2015" 20240 "06/2015" 20454 "01/2016" 20605 "06/2016" 20819 "01/2017" 20970 "06/2017" 21184 "01/2018" 21335 "06/2018" 21549 "01/2019" 21700 "06/2019" 21914 "01/2020" 22065 "06/2020" 22279 "01/2021" 22430 "06/2021" 22644 "01/2022" 22795 "06/2022" 23009 "01/2023" 23160 "06/2023" 23374 "12/2023", angle(90)) scale(0.8)
	graph export "Motivacion_PerCapHomicides_Scatter.jpg", replace
	
sort mesano
twoway (scatter homicidios_corroborados mesano), xtitle("Period") ytitle("Monthly Homicides") title("Evolution of Monthly Corroborated Homicides in Mexico") xlabel(18628 "01/2011" 18779 "06/2011" 18993 "01/2012" 19145 "06/2012" 19359 "01/2013" 19510 "06/2013" 19724 "01/2014" 19875 "06/2014" 20089 "01/2015" 20240 "06/2015" 20454 "01/2016" 20605 "06/2016" 20819 "01/2017" 20970 "06/2017" 21184 "01/2018" 21335 "06/2018" 21549 "01/2019" 21700 "06/2019" 21914 "01/2020" 22065 "06/2020" 22279 "01/2021" 22430 "06/2021" 22644 "01/2022" 22795 "06/2022" 23009 "01/2023" 23160 "06/2023" 23374 "12/2023", angle(90)) scale(0.8)
	graph export "Motivacion_Homicides_Scatter.jpg", replace



sort mesano
binscatter hom_percap_nacional mesano, linetype(none) nquantiles(30) xtitle("Homicides per 10,000 inhabitants") ytitle("Mean municipal number of abarrotes stores") title("Evolution of the number of abarrotes stores in Mexico")

	sort mesano
twoway (hist hom_percap_nacional, bin(100)), xtitle("Período") ytitle("Homicidios promedio por percentil") title("Evolución promedio de los homicidios en México a nivel nacional") xlabel(, format(%td)) scale(0.8)
	graph export "Motivacion_Homicidios_Mspline.jpg", replace



npregress kernel hom_percap_nacional mesano
margins, at(mesano = (18993 19024 19053 19084 19114 19145 19175 19206 19237 19267 19298 19328 19359 19390 19418 19449 19479 19510 19540 19571 19602 19632 19663 19693 19724 19755 19783 19814 19844 19875 19905 19936 19967 19997 20028 20058 20089 20120 20148 20179 20209 20240 20270 20301 20332 20362 20393 20423 20454 20485 20514 20545 20575 20606 20636 20667 20698 20728 20759 20789 20820 20851 20879 20910 20940 20971 21001 21032 21063 21093 21124 21154 21185 21216 21244 21275  21366 21397 21428 21458 21489 21519 21550 21581 21609 21640 21670 21701 21731 21762 21793 21823 21854 21884 21915 21946 21975 22006 22036 22067 22097 22128 22159 22189 22220 22250 22281 22312 22340 22371 22401 22432 22462 22493 22524 22554 22585 22615 22646 22677 22705 22736 22766 22797  22827 22858 22889 22919 22950 22980 23011 23042 23070 23101 23131 23162 23192 23223 23254 23284 23315 23345)) vce(bootstrap)
marginsplot, recast(line) recastci(rarea) ciopts(color(*0.5)) xtitle("Período 12-2011 a 12-2023") ytitle("Homicidios promedio por percentil") title("Evolución de los homicidios totales en México a nivel nacional") xlabel(19093 "01-2011" 19123 "." 19153 "." 19183 "04-2011" 19213 "." 19243 "." 19273 "." 19303 "08-2011" 19333 "." 19363 "." 19393 "." 19423 "12-2011" 19453 "." 19483 "." 19513 "." 19543 "04-2012" 19573 "." 19603 "." 19633 "." 19663 "08-2012" 19693 "." 19723 "." 19753 "." 19783 "12-2012" 19813 "." 19843 "." 19873 "." 19903 "04-2013" 19933 "." 19963 "." 19993 "." 20023 "08-2013" 20053 "." 20083 "." 20113 "." 20143 "12-2013" 20173 "." 20203 "." 20233 "." 20263 "04-2014" 20293 "." 20323 "." 20353 "." 20383 "08-2014" 20413 "." 20443 "." 20473 "." 20503 "12-2014" 20533 "." 20563 "." 20593 "." 20623 "04-2015" 20653 "." 20683 "." 20713 "." 20743 "08-2015" 20773 "." 20803 "." 20833 "." 20863 "12-2015" 20893 "." 20923 "." 20953 "." 20983 "04-2016" 21013 "." 21043 "." 21073 "." 21103 "08-2016" 21133 "." 21163 "." 21193 "." 21223 "12-2016" 21253 "." 21283 "." 21313 "." 21343 "04-2017" 21373 "." 21403 "." 21433 "." 21463 "08-2017" 21493 "." 21523 "." 21553 "." 21583 "12-2017" 21613 "." 21643 "." 21673 "." 21703 "04-2018" 21733 "." 21763 "." 21793 "." 21823 "08-2018" 21853 "." 21883 "." 21913 "." 21943 "12-2018" 21973 "." 22003 "." 22033 "." 22063 "04-2019" 22093 "." 22123 "." 22153 "." 22183 "08-2019" 22213 "." 22243 "." 22273 "." 22303 "12-2019" 22333 "." 22363 "." 22393 "." 22423 "04-2020" 22453 "." 22483 "." 22513 "." 22543 "08-2020" 22573 "." 22603 "." 22633 "." 22663 "12-2020" 22693 "." 22723 "." 22753 "." 22783 "04-2021" 22813 "." 22843 "." 22873 "." 22903 "08-2021" 22933 "." 22963 "." 22993 "." 23023 "12-2021" 23053 "." 23083 "." 23113 "." 23143 "04-2022" 23173 "." 23203 "." 23233 "." 23263 "08-2022" 23293 "." 23323 "." 23353 "." 23383 "12-2022" 23413 "." 23443 "." 23473 "." 23503 "04-2023"  23533 "." 23563 "." 23593 "." 23623 "08-2023" 23653 "." 23683 "." 23713 "." 23743 "12-2023", angle(45)) scale(0.7)
	graph export "Motivacion_Homicidios_Npregress.jpg", replace





gen anio = year(mesano)
collapse (sum) homicidios pobtot, by(anio)
gen periodo = "12/" + string(year(anio))
gen hom_percap_nacional = (homicidios/pobtot)*1000

twoway (fpfitci hom_percap_nacional anio) (scatter hom_percap_nacional anio), xtitle("Período 12-2011 a 12-2023") ytitle("Homicidios promedio por percentil") title("Evolución de los homicidios totales en México a nivel nacional") xlabel(2012(1)2023) scale (0.8)
	graph export "Motivacion_Homicidios_Binscatter.jpg", replace
	





