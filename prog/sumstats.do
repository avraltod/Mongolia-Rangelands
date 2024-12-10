// =============================================================================
// Filename:  do09_wrk_20241210.do
// Author:     Avralt-Od Purevjav
// Last Modified:  Avraa
// Date:     2024-12-10
// =============================================================================
// Summary Statistics 
// ========================================================================== //
	* 
	clear all
	set more off
	set excelxlsxlargefile on
	set processors 8
	
	*loc ws "Work" //Home //Server 
	loc ow = c(os)
	di "`ow'"
	if "`ow'"=="Windows" {
		glo wd "D:\iDrive\Dropbox\ShrdFldrs\TASC"
		glo olf "D:\iDrive\Dropbox\Apps\Overleaf\MonGrass"
	}
	if "`ow'"=="MacOSX" {
		glo wd "~/iDrive/Dropbox/ShrdFldrs/TASC"
		glo olf "~/iDrive/Dropbox/Apps/Overleaf/MonGrass"
	}
	
	cd "$wd" 	
	di "Current date: `c(current_date)'; OS: `c(os)'; User: `c(username)'"

	glo YY = year(date(c(current_date), "DMY"))
	glo MM : di %02.0f `= month(date(c(current_date), "DMY"))'
	glo DD : di %02.0f `=day(date(c(current_date), "DMY"))'

	glo date "${YY}${MM}${DD}" //date stamp 
		di $date " $date"

	glo csv "$wd/csv"
	glo xls "$wd/xls"
	glo dta "$wd/dta"
	glo raw "$wd/raw"
	glo fig "$wd/fig"
	glo tab "$wd/tab"
	glo map "$wd/map"
	glo shp "$wd/shp"
	
********************************************************************************
**# Table D.1: Summary Statistics of Vegetation Biomass and Indices
********************************************************************************
* VEGITATION INDICES at soum level (GROUND and LANDSAT and MODIS)  
********************************************************************************	
	use "$dta/wrk/wrk_soum.dta" , clear 
		keep year asid *_na_ave bms

	*Ground based Vegetation Biomass (2001-2020)
	cap la var bms "\, Vegetation Biomass (cwt/ha)"			
		
	*LANDSAT Vegitation indices (1984-2024) 	
	ren landsat_* *  
		cap la var ndvi "\, Normalized Difference Vegetation Index (NDVI)"
		cap la var evi "\, Enhanced Vegetation Index (EVI)"
		cap la var savi "\, Soil Adjusted Vegetation Index (SAVI)"
		cap la var msavi "\, Modified Soil Adjusted Vegetation Index (MSAVI)"
		cap la var nirv "\, Near-Infrared Reflectance of Vegetation (NIRv)"
		cap la var ndwi "\, Normalized Difference Water Index (NDWI)"
			foreach v in ndvi evi savi msavi nirv {
				ren `v' landsat_`v' 
			}
	*MODIS Vegitation indices (2000-2024) 		
	ren modis_* * 
		cap la var ndvi "\, Normalized Difference Vegetation Index (NDVI)"
		cap la var evi "\, Enhanced Vegetation Index (EVI)"
		cap la var savi "\, Soil Adjusted Vegetation Index (SAVI)"
		cap la var msavi "\, Modified Soil Adjusted Vegetation Index (MSAVI)"
		cap la var nirv "\, Near-Infrared Reflectance of Vegetation (NIRv)"
		cap la var ndwi "\, Normalized Difference Water Index (NDWI)"
			foreach v in ndvi evi savi msavi nirv {
				ren `v' modis_`v' 
			}

	glo  texfile "sumstat_vegi"		
	eststo SS: estpost su bms landsat_* modis_*  , d 

	esttab SS  using "$olf/tab/supp/${texfile}.tex", replace substitute(\_ _) ///
			cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) ") ///
			noobs varwidth(30) nomtitle nonumber ///
			 f label booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01) ///
			compress nogaps collabels( ///
				"\multicolumn{@span}{c}{N}" ///
				"\multicolumn{@span}{c}{Mean}" ///
				"\multicolumn{@span}{c}{SD}" ///
				"\multicolumn{@span}{c}{Min}" ///
				"\multicolumn{@span}{c}{Max}" ///
					, lhs("Variable (at Soum-Year level)")) ///
			refcat( ///
				bms "  \addlinespace[1ex]  \it Ground based Vegetation Biomass (2001-2020) " ///
				landsat_ndvi "  \addlinespace[1ex]  \it LANDSAT Vegitation indices (1984-2024) " ///
				modis_ndvi "  \addlinespace[1ex]  \it MODIS Vegitation indices (2000-2024) " ///
				, nolabel) ///
			alignment(D{.}{.}{-1}) eqlabels("a", lhs("Variable (at Soum-Year level)"))

			
********************************************************************************
**# Table D.2: Correlation among Vegetation Biomass and Indices
********************************************************************************
* CORRELATION AMONG VEGITATION INDICES at soum level (GROUND and LANDSAT and MODIS)  
********************************************************************************
	use "$dta/wrk/wrk_soum.dta" , clear 
		keep year asid *_na_ave bms
		
	la var landsat_ndvi "\qquad\quad NDVI"
	la var landsat_evi "\qquad\quad EVI"
	la var landsat_savi "\qquad\quad SAVI"
	la var landsat_msavi "\qquad\quad MSAVI"
	la var landsat_nirv "\qquad\quad NIRv"
	
	la var modis_ndvi "\qquad\quad NDVI"
	la var modis_evi "\qquad\quad EVI"
	la var modis_savi "\qquad\quad SAVI"
	la var modis_msavi "\qquad\quad MSAVI"
	la var modis_nirv "\qquad\quad NIRv"	
	
	la var bms "\qquad Veg. Biomass"

	loc varlist1 "landsat_ndvi landsat_evi landsat_savi landsat_msavi landsat_nirv  modis_ndvi modis_evi modis_savi modis_msavi modis_nirv bms" 
	loc varlist2 "landsat_evi landsat_savi landsat_msavi landsat_nirv modis_ndvi modis_evi modis_savi modis_msavi modis_nirv bms" 
	loc varlist3 "landsat_savi landsat_msavi landsat_nirv modis_ndvi modis_evi modis_savi modis_msavi modis_nirv bms" 
	loc varlist4 "landsat_msavi landsat_nirv modis_ndvi modis_evi modis_savi modis_msavi modis_nirv bms"
	loc varlist5 "landsat_nirv modis_ndvi modis_evi modis_savi modis_msavi modis_nirv bms"
	loc varlist6 "modis_ndvi modis_evi modis_savi modis_msavi modis_nirv bms"
	loc varlist7 "modis_evi modis_savi modis_msavi modis_nirv bms"
	loc varlist8 "modis_savi modis_msavi modis_nirv bms"
	loc varlist9 "modis_msavi modis_nirv bms"
	loc varlist10 "modis_nirv bms"


	foreach i in  1 2 3 4 5 6 7 8 9 10 {
		cap drop CORR_`i'
	eststo CORR_`i' : estpost corr `varlist`i''

	}
	
	glo  texfile "corr_landsat_modis_vegi"	
	esttab CORR*  using "$olf/tab/supp/${texfile}.tex" ///
		, replace substitute(\midrule "") ///
			cells("b(fmt(3) )") ///
			 noobs varwidth(10)  ///
			 f booktabs label star(* 0.10 ** 0.05 *** 0.01) ///
			compress nogaps unstack depvars /// 
			collabels(, none) ///
			mlabels(NDVI EVI SAVI MSAVI NIRv NDVI EVI SAVI MSAVI NIRv) ///
			mgroups(LANDSAT MODIS, pattern(1 0 0 0 0 1 0 0 0 0) ///
				prefix(\multicolumn{@span}{c}{) suffix(})   ///
				span erepeat(\cmidrule(lr){@span}))         ///			
			refcat(landsat_evi "(A) \it LANDSAT " ///
				modis_ndvi "(B) \it MODIS" ///
				bms "(C) \it GROUND" ///
				, nolabel) ///						
				alignment(D{.}{.}{-1}) 
	
********************************************************************************
**# Table D.3: Summary Statistics of Herd size
********************************************************************************
* LIVESTOCK at soum level (census and survey)
********************************************************************************	
	use "$dta/wrk/wrk_soum.dta" , clear 
		keep year asid cen_* svy_* 
		
	loc VAR "total sheep goat cattle horse camel"
	loc LAB "livestocks Sheep Goats Cattle Horses Camels"

	loc i = 0  
	foreach i of numlist 1/6 {
		
		loc vn : word `i' of `VAR'
		loc vl : word `i' of `LAB'

		cap drop cls`i'
		g cls`i' = cen_`vn'*1000
// 			replace cls`i' = 0 if cls`i' == . 
		la var  cls`i' "\qquad `vl'"
		format cls`i' %15.3f		
		
		cap drop sls`i'
		g sls`i' = svy_`vn'*1000
// 			replace sls`i' = 0 if sls`i' == . & year > 2015	
		la var  sls`i' "\qquad `vl'"
		format sls`i' %15.3f	
		
	}	
	la var cls1 "\, Total livestocks, thousands"
	la var sls1 "\, Total livestocks, thousands"

	* in sheep units 
	loc CON "1 0.9 6 7 5"
	loc VAR "sheep goat cattle horse camel"
	loc LAB "Sheep Goats Cattle Horses Camels"

	loc i = 0  
	foreach l in sheep goat cattle horse camel {
	loc i = `i' + 1
	
		loc cn : word `i' of `CON'
		loc vl : word `i' of `LAB'

		cap drop su_cen_`l'
		g su_cen_`l' = cen_`l'*1000*`cn'
// 			replace su_cen_`l' = 0 if cen_`l' == . 		
		la var  su_cen_`l' "\qquad `vl'"
		format su_cen_`l' %15.3f		
		
	}	
	egen su_cen_total = rowtotal(su_cen_*) , missing 
	la var su_cen_total "\, Total livestocks, thousands"
	order su_cen_total, before(su_cen_sheep)

	loc i = 0  
	foreach l in sheep goat cattle horse camel {
	loc i = `i' + 1
	
		loc cn : word `i' of `CON'
		loc vl : word `i' of `LAB'

		cap drop su_svy_`l'
		g su_svy_`l' = svy_`l'*1000*`cn'
// 			replace su_svy_`l' = 0 if svy_`l' == . 	& year > 2015			
		la var  su_svy_`l' "\qquad `vl'"
		format su_svy_`l' %15.3f		
		
	}	
	egen su_svy_total = rowtotal(su_svy_*) , missing 
	la var su_svy_total "\, Total livestocks, thousands"
	order su_svy_total, before(su_svy_sheep)

	glo  texfile "sumstat_herd"		
	eststo SS: estpost su cls? sls? su_cen_* su_svy_* , d 

	esttab SS  using "$olf/tab/supp/${texfile}.tex", replace substitute(\_ _) ///
			cells("count(fmt(0)) mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) ") ///
			noobs varwidth(30) nomtitle nonumber ///
			 f label booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01) ///
			compress nogaps collabels( ///
				"\multicolumn{@span}{c}{N}" ///
				"\multicolumn{@span}{c}{Mean}" ///
				"\multicolumn{@span}{c}{SD}" ///
				"\multicolumn{@span}{c}{Min}" ///
				"\multicolumn{@span}{c}{Max}" ///
					, lhs("Variable (at Soum-Year level)")) ///
			refcat( ///
				cls1 "  \addlinespace[1ex]  \it December census (1970-2023, in heads) " ///
				sls1 "  \addlinespace[1ex]  \it June survey (2016-2024, in heads) " ///
				su_cen_total "  \addlinespace[1ex]  \it December census (1970-2023, in sheep units) " ///
				su_svy_total "  \addlinespace[1ex]  \it June survey (2016-2024, in sheep units) " ///
				, nolabel) ///
			alignment(D{.}{.}{-1}) eqlabels("a", lhs("Variable (at Soum-Year level)"))
	
	
********************************************************************************	
**# Table D.4: Summary Statistics of Herd Density (Weighted by Inverse Distance)	
********************************************************************************
* LIVESTOCK DENSITY at soum level (census and survey)
********************************************************************************	
	use "$dta/wrk/wrk_soum.dta" , clear 
		keep year asid idw1_* 
			ren idw1_* * 
	
	loc VAR "total sheep goat cattle horse camel"
	loc LAB "livestocks Sheep Goats Cattle Horses Camels"

	loc i = 0  
	foreach i of numlist 1/6 {
		loc vn : word `i' of `VAR'
		loc vl : word `i' of `LAB'
		
		cap drop cls`i'
			g cls`i' = cen_`vn'*1000
				la var  cls`i' "\qquad `vl'"
					format cls`i' %15.3f		
		cap drop sls`i'
			g sls`i' = svy_`vn'*1000
				la var  sls`i' "\qquad `vl'"
					format sls`i' %15.3f	
	}	
	la var cls1 "\, Livestock density, thousands"
	la var sls1 "\, Livestock density, thousands"

	* in sheep units 
	loc CON "1 0.9 6 7 5"
	loc VAR "sheep goat cattle horse camel"
	loc LAB "Sheep Goats Cattle Horses Camels"

	loc i = 0  
	foreach l in sheep goat cattle horse camel {
	loc i = `i' + 1
		loc cn : word `i' of `CON'
		loc vl : word `i' of `LAB'

		cap drop su_cen_`l'
		g su_cen_`l' = cen_`l'*1000*`cn'
		la var  su_cen_`l' "\qquad `vl'"
		format su_cen_`l' %15.3f		
		
	}	
	egen su_cen_total = rowtotal(su_cen_*) , missing 
	la var su_cen_total "\, Livestock density, thousands"
	order su_cen_total, before(su_cen_sheep)
	
	loc i = 0  
	foreach l in sheep goat cattle horse camel {
	loc i = `i' + 1
	
		loc cn : word `i' of `CON'
		loc vl : word `i' of `LAB'

		cap drop su_svy_`l'
		g su_svy_`l' = svy_`l'*1000*`cn'
		la var  su_svy_`l' "\qquad `vl'"
		format su_svy_`l' %15.3f		
		
	}	
	egen su_svy_total = rowtotal(su_svy_*) , missing 
	la var su_svy_total "\, Livestock density, thousands"
	order su_svy_total, before(su_svy_sheep)

	
	glo  texfile "sumstat_idw"		
	eststo SS: estpost su cls? sls? su_cen_* su_svy_*  , d 

	esttab SS  using "$olf/tab/supp/${texfile}.tex", replace substitute(\_ _) ///
			cells("count(fmt(0)) mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) ") ///
			noobs varwidth(30) nomtitle nonumber ///
			 f label booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01) ///
			compress nogaps collabels( ///
				"\multicolumn{@span}{c}{N}" ///
				"\multicolumn{@span}{c}{Mean}" ///
				"\multicolumn{@span}{c}{SD}" ///
				"\multicolumn{@span}{c}{Min}" ///
				"\multicolumn{@span}{c}{Max}" ///
					, lhs("Variable (at Soum-Year level)")) ///
			refcat( ///
				cls1 "  \addlinespace[1ex]  \it December census (1970-2023, in heads) " ///
				sls1 "  \addlinespace[1ex]  \it June survey (2016-2024, in heads) " ///
				su_cen_total "  \addlinespace[1ex]  \it December census (1970-2023, in sheep units) " ///
				su_svy_total "  \addlinespace[1ex]  \it June survey (2016-2024, in sheep units) " ///
				, nolabel) ///
			alignment(D{.}{.}{-1}) eqlabels("a", lhs("Variable (at Soum-Year level)"))

********************************************************************************
**# Table D.5-8: Summary Statistics of Weather Variables on Summer Grazing Range
********************************************************************************
********************************************************************************
* WEATHER VARIABLES at soum level (ERA) SGR: AUT/WTR/SPR/SMR  
********************************************************************************
	foreach r in sgr {	
		foreach s in aut wtr spr smr { 	
	
		use "$dta/wrk/wrk_soum.dta" , clear 
			keep year asid `r'_`s'_*  
				ren `r'_`s'_* * 
					drop if year < 1981 

		#delimit;
		glo VAR 
			"
				ws_ave prec tmp_ave sc sd sf sm  
				 gdd*
				 hed*
				 wsd* 
				 wed*
			";
		#delimit cr 
		
		cap la var prec "Precipation, mm, accumulated"
		cap la var ws_ave "Wind speed, m/s, average"
		cap la var tmp_ave "Temperature, $ ^\circ\text{C}$, average"
		cap la var sc "Snow coverage, \%, average"
		cap la var sd "Snow density, kg/m$ ^3$, average"
		cap la var sf "Snow fall, mm, accumulated"
		cap la var sm "Snow melt, mm, accumulated"

		foreach v of varlist $VAR {
			loc lab: variable label `v'
				di "`lab'"
					loc lab =subinstr("`lab'", "Degree days,", "", . )
					loc lab =subinstr("`lab'", "Exposure days,", "", . )
					loc lab =subinstr("`lab'", "Windy days,", "", . )
					loc lab =subinstr("`lab'", "Wind exposure,", "", . )
				loc lab = "\, "+"`lab'"	

				di "`lab'"
				la var `v' "`lab'"
		}	
		
		loc COEFLABS ""
		foreach v of varlist $VAR {
			loc lab: variable label `v'
				*di "`v' " "`lab'"
				loc coef = "`v' "+ char(34)+ "`lab'" +char(34)
				*di `"`coef'"'
				loc COEFLABS `COEFLABS' `coef'
					di `"`COEFLABS'"'
		}	
		
		//fdd 
			loc i = 0 
			loc lb = 50
			foreach ub of numlist 30(5)0 {
				loc `i++'
				cap la var fdd`i' " $ \quad \sum \tau , \quad  -`lb'^\circ \text{C} < \tau \leq-`ub'^\circ \text{C} $ "
				cap la var ced`i' " $ \quad \sum \bm{1}(-`lb'^\circ \text{C} < \tau \leq-`ub'^\circ \text{C}) $ "
				loc lb = `ub'
			}
				cap la var tmp_neg " $ \quad \sum \tau,  \quad  -50^\circ\text{C} < \tau \leq 0^\circ \text{C} $ "
				cap la var tmp_cld " $ \quad \sum \bm{1}(-50^\circ\text{C} < \tau \leq 0^\circ \text{C}) $ "
		//gdd 	
			loc i = 0 
			loc lb = 0
			foreach ub of numlist 5(5)30 50 {
				loc `i++'
				cap la var gdd`i' " $ \quad \sum \tau,  \quad  `lb'^\circ \text{C} < \tau \leq `ub'^\circ \text{C} $ "
				cap la var hed`i' " $ \quad \sum \bm{1}(`lb'^\circ \text{C} < \tau \leq `ub'^\circ \text{C}) $ "
				loc lb = `ub'
			}
			
				cap la var tmp_pos " $ \quad \sum \tau,  \quad  0^\circ \text{C} < \tau \leq 50^\circ \text{C} $ "
				cap la var tmp_wrm " $ \quad \sum \bm{1}(0^\circ \text{C} < \tau \leq 50^\circ \text{C}) $ "			
		//ws  
			loc i = 0 
			loc lb = 0
			foreach ub of numlist 1 5 10 25 {
				loc `i++'				
				cap la var wsd`i' " $ \quad \sum \omega, \quad  `lb'  < \omega \leq `ub'  $ "
				cap la var wed`i' " $ \quad \sum \bm{1}(`lb'  < \omega \leq `ub' ) $ "
				loc lb = `ub'
			}
			
		
		eststo SS: estpost su ${VAR} , d 
		glo  texfile "sumstat_wthr_`r'_`s'"	
		esttab SS  using "$olf/tab/supp/${texfile}.tex", replace substitute(\_ _) ///
				cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) ") ///
				noobs varwidth(30) nomtitle nonumber ///
				 f label booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01) ///
				compress nogaps collabels( ///
					"\multicolumn{@span}{c}{N}" ///
					"\multicolumn{@span}{c}{Mean}" ///
					"\multicolumn{@span}{c}{SD}" ///
					"\multicolumn{@span}{c}{Min}" ///
					"\multicolumn{@span}{c}{Max}" ///
						, lhs("Variable (at Soum-Year level)")) ///
				refcat( ///
					ws_ave "\addlinespace[1ex] \it Weather variables (1981-2024) " ///
					gdd1 "  \addlinespace[1ex]  \it Growing degree days:" ///
					hed1 "  \addlinespace[1ex]  \it Exposure to warm days:" ///
					wsd1 "  \addlinespace[1ex]  \it Wind intensity:" ///
					wed1 "  \addlinespace[1ex]  \it Exposure to windy days:" ///
					, nolabel) ///
				alignment(D{.}{.}{-1}) eqlabels("a", lhs("Variable (at Soum-Year level)"))
		}
	}	
		
********************************************************************************
**# Table D.9-12: Summary Statistics of Weather Variables on Winter Grazing Range
********************************************************************************
********************************************************************************
* WEATHER VARIABLES at soum level (ERA) WGR: AUT/WTR/SPR/SMR  
********************************************************************************

	foreach r in wgr {	
		foreach s in aut wtr spr smr { 	
		
		use "$dta/wrk/wrk_soum.dta" , clear 
			keep year asid `r'_`s'_*  
				ren `r'_`s'_* * 
					drop if year < 1981 
		#delimit;
		glo VAR 
			"
				ws_ave prec tmp_ave sc sd sf sm  
				 fdd*
				 ced*
				 wsd* 
				 wed* 
			";
		#delimit cr 
		
		cap la var prec "Precipation, mm, accumulated"
		cap la var ws_ave "Wind speed, m/s, average"
		cap la var tmp_ave "Temperature, $ ^\circ\text{C}$, average"
		cap la var sc "Snow coverage, \%, average"
		cap la var sd "Snow density, kg/m$ ^3$, average"
		cap la var sf "Snow fall, mm, accumulated"
		cap la var sm "Snow melt, mm, accumulated"

		foreach v of varlist $VAR {
			loc lab: variable label `v'
				di "`lab'"
					loc lab =subinstr("`lab'", "Degree days,", "", . )
					loc lab =subinstr("`lab'", "Exposure days,", "", . )
					loc lab =subinstr("`lab'", "Windy days,", "", . )
					loc lab =subinstr("`lab'", "Wind exposure,", "", . )
				loc lab = "\, "+"`lab'"	

				di "`lab'"
				la var `v' "`lab'"
		}	
		
		loc COEFLABS ""
		foreach v of varlist $VAR {
			loc lab: variable label `v'
				*di "`v' " "`lab'"
				loc coef = "`v' "+ char(34)+ "`lab'" +char(34)
				*di `"`coef'"'
				loc COEFLABS `COEFLABS' `coef'
					di `"`COEFLABS'"'
		}	
		
		//fdd 
			loc i = 0 
			loc lb = 50
			foreach ub of numlist 30(5)0 {
				loc `i++'
				cap la var fdd`i' " $ \quad \sum \tau , \quad  -`lb'^\circ \text{C} < \tau \leq-`ub'^\circ \text{C} $ "
				cap la var ced`i' " $ \quad \sum \bm{1}(-`lb'^\circ \text{C} < \tau \leq-`ub'^\circ \text{C}) $ "
				loc lb = `ub'
			}
				cap la var tmp_neg " $ \quad \sum \tau,  \quad  -50^\circ\text{C} < \tau \leq 0^\circ \text{C} $ "
				cap la var tmp_cld " $ \quad \sum \bm{1}(-50^\circ\text{C} < \tau \leq 0^\circ \text{C}) $ "
		//gdd 	
			loc i = 0 
			loc lb = 0
			foreach ub of numlist 5(5)30 50 {
				loc `i++'
				cap la var gdd`i' " $ \quad \sum \tau,  \quad  `lb'^\circ \text{C} < \tau \leq `ub'^\circ \text{C} $ "
				cap la var hed`i' " $ \quad \sum \bm{1}(`lb'^\circ \text{C} < \tau \leq `ub'^\circ \text{C}) $ "
				loc lb = `ub'
			}
				cap la var tmp_pos " $ \quad \sum \tau,  \quad  0^\circ \text{C} < \tau \leq 50^\circ \text{C} $ "
				cap la var tmp_wrm " $ \quad \sum \bm{1}(0^\circ \text{C} < \tau \leq 50^\circ \text{C}) $ "			
		//ws  
			loc i = 0 
			loc lb = 0
			foreach ub of numlist 1 5 10 25 {
				loc `i++'				
				cap la var wsd`i' " $ \quad \sum \omega, \quad  `lb'  < \omega \leq `ub'  $ "
				cap la var wed`i' " $ \quad \sum \bm{1}(`lb'  < \omega \leq `ub' ) $ "
				loc lb = `ub'
			}
			
		eststo SS: estpost su ${VAR}  , d 
		glo  texfile "sumstat_wthr_`r'_`s'"	
		quietly esttab SS  using "$olf/tab/supp/${texfile}.tex", replace substitute(\_ _) ///
				cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) ") ///
				noobs varwidth(30) nomtitle nonumber ///
				 f label booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01) ///
				compress nogaps collabels( ///
					"\multicolumn{@span}{c}{N}" ///
					"\multicolumn{@span}{c}{Mean}" ///
					"\multicolumn{@span}{c}{SD}" ///
					"\multicolumn{@span}{c}{Min}" ///
					"\multicolumn{@span}{c}{Max}" ///
						, lhs("Variable (at Soum-Year level)")) ///
				refcat( ///
					ws_ave "\addlinespace[1ex] \it Weather variables(1981-2024) " ///
					fdd1 "  \addlinespace[1ex]  \it Freezing degree days:" ///
					ced1 "  \addlinespace[1ex]  \it Exposure to cold days:" ///
					wsd1 "  \addlinespace[1ex]  \it Wind intensity:" ///
					wed1 "  \addlinespace[1ex]  \it Exposure to windy days:" ///
					, nolabel) ///
				alignment(D{.}{.}{-1}) eqlabels("a", lhs("Variable (at Soum-Year level)"))
		}
	}	

********************************************************************************
**# Summary Statistics of the Sample used in the First Stage, Col. 6 of Table E.1
* Table D.13 and D.14 
********************************************************************************			
* SUMSTAT OF OLS, FS, SS 	
********************************************************************************			
	use "$dta/wrk/wrk_soum.dta", clear 
		keep year asid su_*_total idw* *na_ave sgr_* wgr_* eco* 

		xtset asid year 

		* vegetation index (NDVI)
		cap drop lndvi 
			g lndvi = log(landsat_ndvi_na_ave)
				la var lndvi "$ \quad \ln  B^{summer}_{st, SGR} $"
		cap drop lndvilag  
			g lndvilag = L.lndvi 
				la var lndvilag "$ \quad  \ln B^{summer}_{s,t-1, SGR} $"
		* june survey in sheep units (t, calender year )
		cap drop survey 
			g survey = log(su_svy_total)
				la var survey "$ \quad  \ln HS ^{June}_{st} $"
		* december census in sheep units (t+1, non calender year)
		cap drop census 
		g census = log(su_cen_total)  
		la var census "$ \quad  \ln  HS ^{December}_{st} $"
			
		*SGR weather variables 
		#delimit;
		loc HED 
			`" 
			"GDD(0^\circ\text{C} ,5^\circ\text{C}]" 
			"GDD(5^\circ\text{C},10^\circ\text{C}]" 
			"GDD(10^\circ\text{C},15^\circ\text{C}]" 
			"GDD(15^\circ\text{C},20^\circ\text{C}]" 
			"GDD(20^\circ\text{C},25^\circ\text{C}]" 
			"GDD(20^\circ\text{C},30^\circ\text{C}]" 
			"GDD(>30^\circ\text{C})"  
			"'; 	
		#delimit cr 
			* SGR , only HED 
			foreach ssn in smr aut wtr spr {
			* GDDs  
				foreach i of numlist 1/7 {
					loc h : word `i' of `HED'
						la var sgr_`ssn'_hed`i' "\quad $ `h' $ "
				}
				
			replace sgr_`ssn'_hed5 = sgr_`ssn'_hed5 + sgr_`ssn'_hed6 + sgr_`ssn'_hed7 
				la var sgr_`ssn'_hed5 "\quad $ GDD(>20^\circ\text{C}) $ "
					cap drop sgr_`ssn'_hed1 sgr_`ssn'_hed2 sgr_`ssn'_hed6 sgr_`ssn'_hed7
					
			* total precipitation (m)
				replace sgr_`ssn'_prec = (sgr_`ssn'_prec) / 1000 
					la var sgr_`ssn'_prec " \quad Precipitation (m, acc.) "

			* ave. wind speed (m/s)		
				la var sgr_`ssn'_ws_ave " \quad Wind speed (m/s, ave.) "
			}	
			
			
		*SGR WTHR 
		glo SGRAUT "sgr_aut_hed* sgr_aut_prec sgr_aut_ws_ave"
		glo SGRWTR "sgr_wtr_hed* sgr_wtr_prec sgr_wtr_ws_ave"
		glo SGRSPR "sgr_spr_hed* sgr_spr_prec sgr_spr_ws_ave"
		glo SGRSMR "sgr_smr_hed* sgr_smr_prec sgr_smr_ws_ave"
		
		*WGR weather variables (excluded IVs)
		#delimit; 
		loc CED 
		`" 
			"FDD(<-30^\circ\text{C})" 
			"FDD(-30^\circ\text{C},-25^\circ\text{C}]" 
			"FDD(-25^\circ\text{C},-20^\circ\text{C}]" 
			"FDD(-20^\circ\text{C},-15^\circ\text{C}]" 
			"FDD(-15^\circ\text{C},-10^\circ\text{C}]" 
			"FDD(-10^\circ\text{C},-5^\circ\text{C}]" 
			"FDD(-5^\circ\text{C}, 0^\circ\text{C}]"  
		"'; 
		loc WED 
		`" 
			"WD(0,1]" 
			"WD(1,5]" 
			"WD(5,10]" 
			"WD(>10)" 
		"'; 
		#delimit cr 
		
		*summer W on WGR 
			* HED (past summer, t-1) 
			replace wgr_smr_hed5 = wgr_smr_hed5 + wgr_smr_hed6 + wgr_smr_hed7 
				la var wgr_smr_hed5 "\quad $ GDD(>20^\circ\text{C}) $ "
					cap drop wgr_smr_hed1 wgr_smr_hed2 wgr_smr_hed3 ///
						wgr_smr_he4 wgr_smr_hed6 wgr_smr_hed7 
				g lag_wgr_smr_hed5 = L.wgr_smr_hed5 
					la var lag_wgr_smr_hed5 " \quad $ GDD(>20^\circ\text{C}) $ "
						cap drop wgr_smr_hed5 
					
			* total precipitation (m), (past summer, t-1)
				replace wgr_smr_prec = (wgr_smr_prec) / 1000 
					la var wgr_smr_prec " \quad Rainfall (m, acc.) "		
				g lag_wgr_smr_prec = L.wgr_smr_prec 
					la var lag_wgr_smr_prec " \quad Rainfall (m, acc.) "
						cap drop wgr_smr_prec 
					
		*autumn W on WGR 
			* CED 
			replace wgr_aut_ced3 = wgr_aut_ced3 + wgr_aut_ced2 + wgr_aut_ced1 
				la var wgr_aut_ced3 "\quad $ FDD (<-20^\circ\text{C}) $ "
					cap drop wgr_aut_ced1 wgr_aut_ced2 wgr_aut_ced4 ///
						wgr_aut_ced5 wgr_aut_ced6 wgr_aut_ced7
						
		*winter W on WGR 
			* CED 
			replace wgr_wtr_ced3 = wgr_wtr_ced3 + wgr_wtr_ced2 + wgr_wtr_ced1 
				la var wgr_wtr_ced3 "\quad $ FDD (<-20^\circ\text{C}) $ "
					cap drop wgr_wtr_ced1 wgr_wtr_ced2 wgr_wtr_ced4 ///
						wgr_wtr_ced5 wgr_wtr_ced6 wgr_wtr_ced7
			* WED 
			replace wgr_wtr_wed3 = wgr_wtr_wed3 + wgr_wtr_wed4 
					la var wgr_wtr_wed3 "\quad Windy days $ (>5m/s)$ "
						cap drop wgr_wtr_wed1 wgr_wtr_wed2 wgr_wtr_wed4
			* snow density (kg/m3)
				la var wgr_wtr_sd " \quad Snow density, $ kg/m^3 $ "						
						
		*spring W on WGR 
			* CED 
			replace wgr_spr_ced3 = wgr_spr_ced3 + wgr_spr_ced2 + wgr_spr_ced1 
				la var wgr_spr_ced3 "\quad $ FDD (<-20^\circ\text{C}) $ "
					cap drop wgr_spr_ced1 wgr_spr_ced2 wgr_spr_ced4 ///
						wgr_spr_ced5 wgr_spr_ced6 wgr_spr_ced7				
				
		*WGR WTHR 
		glo WGRSMR "lag_wgr_smr_prec lag_wgr_smr_hed5"
		glo WGRAUT "wgr_aut_ced3"
		glo WGRWTR "wgr_wtr_sd wgr_wtr_ced3 wgr_wtr_wed3"
		glo WGRSPR "wgr_spr_ced3"

********************************************************************************
**# Table D.13: SS. of the Sample used in the First Stage, Col. 6 of Table E.1
********************************************************************************
*------------------------------------------------------------------------------*
* FS 
*------------------------------------------------------------------------------*
			
	glo y survey 
	glo x census 
	
	loc varlist $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} ${SGRAUT}

	reghdfe $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ///
		${SGRSPR} ${SGRWTR} ${SGRAUT}  ///
			, absorb(i.year i.asid i.eco#i.year) vce(cluster asid)
	cap drop herdsizehat
		predict herdsizehat, xb 	
	
	eststo SS: estpost su `varlist' if e(sample) , d 
	glo  texfile "sumstat_fs"	
	esttab SS  using "$olf/tab/supp/${texfile}.tex", replace substitute(\_ _) ///
			cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) ") ///
			noobs varwidth(30) nomtitle nonumber ///
			 f label booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01) ///
			 drop(lndvilag) ///
			compress nogaps collabels( ///
				"\multicolumn{@span}{c}{N}" ///
				"\multicolumn{@span}{c}{Mean}" ///
				"\multicolumn{@span}{c}{SD}" ///
				"\multicolumn{@span}{c}{Min}" ///
				"\multicolumn{@span}{c}{Max}" ///
					, lhs("Variable (at Soum-Year level)")) ///
			refcat( ///
						survey "\it Dependent variable: \\ \it  Herdsize in June, survey" ///
						census "\\ \it SAE variable: \\ \it  Herdsize in December, census" ///
						sgr_aut_hed3 "\it  Fall weather, SGR" ///
						sgr_spr_hed3 "\\ \it Included IVs: \\ \midrule  \it  Spring weather, SGR" ///
						sgr_wtr_hed3 "\it  Winter weather, SGR" ///
						sgr_smr_hed3 "\it  Summer weather, SGR" ///
						lag_wgr_smr_prec "\\ \it Excluded IVs: \\  \midrule \it  Past summer weather, WGR" ///
						wgr_aut_prec "\it  Fall weather, WGR" ///
						wgr_spr_wed3 "\it  Spring weather, WGR" ///
						wgr_wtr_sd "\it  Winter weather, WGR" ///
				, nolabel) ///
			alignment(D{.}{.}{-1}) ///
				eqlabels("a", lhs("Variable (at Soum-Year level)"))

********************************************************************************
**# Table D.14: SS. of the Sample used in the Second Stage, Col. 6 of Table E.4
********************************************************************************	
*------------------------------------------------------------------------------*
* Second Step: Livestock on Weather 
*------------------------------------------------------------------------------*	
	
	cap drop hshat 
	g hshat = . 
	la var hshat "\quad $\ln \widehat{HS}^{June}_{st}$"
	
	glo y lndvi  
	glo x hshat 

	loc varlist $y $x lndvilag  ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	replace hshat = herdsizehat
	
	reghdfe `varlist' , absorb(i.year i.asid i.eco#i.year) vce(cluster asid)
	
	eststo SS: estpost su `varlist' if e(sample) , d 
	glo  texfile "sumstat_ss"	
	esttab SS  using "$olf/tab/supp/${texfile}.tex", replace substitute(\_ _) ///
			cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) ") ///
			noobs varwidth(30) nomtitle nonumber ///
			 f label booktabs nomtitles star(* 0.10 ** 0.05 *** 0.01) ///
			 drop(lndvilag) ///
			compress nogaps collabels( ///
				"\multicolumn{@span}{c}{N}" ///
				"\multicolumn{@span}{c}{Mean}" ///
				"\multicolumn{@span}{c}{SD}" ///
				"\multicolumn{@span}{c}{Min}" ///
				"\multicolumn{@span}{c}{Max}" ///
					, lhs("Variable (at Soum-Year level)")) ///
			refcat( ///
						lndvi "\it Dependent variable: \\ \it  Summer NDVI, SGR" ///
						hshat "\\ \it Independent variables: \\ \midrule \it  Predicted herdsize in June" ///
						sgr_aut_hed3 "\\ \it  Fall weather, SGR" ///
						sgr_spr_hed3 "\\ \it  Spring weather, SGR" ///
						sgr_wtr_hed3 "\\ \it  Winter weather, SGR" ///
						sgr_smr_hed3 "\\ \it  Summer weather, SGR" ///
				, nolabel) ///
			alignment(D{.}{.}{-1}) ///
				eqlabels("a", lhs("Variable (at Soum-Year level)"))

	
	
	

	