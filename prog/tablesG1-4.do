// =============================================================================
// Filename:  do10_reg5_ld_20241210.do
// Author:     Avralt-Od Purevjav
// Last Modified:  Avraa
// Date:     2024-12-10
// =============================================================================
// Long Differences Estimates (Annex G)
// Estimated Marginal Effects of Climate and Herd Size
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
*TABLE G.1-4: Estimated Marginal Effects of Climate and Herd Size 
********************************************************************************	
	use "$dta/wrk/wrk_soum.dta" , clear 
		keep year asid su_*_total idw* *na_ave sgr_* wgr_* eco* aid sid 
		
		xtset asid year 

		* vegetation index (NDVI)
		cap drop lndvi 
			g lndvi = log(landsat_ndvi_na_ave)
				la var lndvi "$ \ln  B^{summer}_{st, SGR} $"
		cap drop lndvilag  
			g lndvilag = L.lndvi 
				la var lndvilag "$ \ln B^{summer}_{s,t-1, SGR} $"
		* june survey in sheep units (t, calender year )
		cap drop survey 
			g survey = log(su_svy_total)
				la var survey "$ \ln HS ^{June}_{st} $"
		* december census in sheep units (t+1, non calender year)
		cap drop census 
		g census = log(su_cen_total)  
		la var census "$ \ln  HS ^{December}_{st} $"
		
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

	save "$dta/wrk/wrk_soum_ld.dta", replace 
	
*------------------------------------------------------------------------------*
**# Bootstrapping 
*------------------------------------------------------------------------------*
	
	cap program drop BSLD 
	program define BSLD, rclass
	syntax [, diff(integer 1) tfe(integer 2) fst(integer 3) ]
	* bootstrapping 
	
	use "$dta/wrk/wrk_soum_ld.dta" , clear 
	tempfile boot 
	save `boot', replace 
	
	xtset asid year 
	
	*SGR WTHR 
	glo SGRAUT "sgr_aut_hed* sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed* sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed* sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed* sgr_smr_prec sgr_smr_ws_ave"
	
	*WGR WTHR IVs
	glo WGRSMR "lag_wgr_smr_prec lag_wgr_smr_hed5"
	glo WGRAUT "wgr_aut_ced3"
	glo WGRWTR "wgr_wtr_sd wgr_wtr_ced3 wgr_wtr_wed3"
	glo WGRSPR "wgr_spr_ced3"
	
	** FS 
	glo y survey 
	glo x census 
	
	loc varlist1 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} 
	loc varlist2 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}   
	loc varlist3 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}
	loc varlist4 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR}
	loc varlist5 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} 
	
	use `boot', clear 
		keep if year > 2015
			keep year asid 
	
	* 2024-2016: 9 years 
	bsample round(1*`fst') , strata(asid)
	g boot = 1 
	
	merge m:1 year asid using `boot', assert(2 3) keep(2 3) nogen 
	replace boot = 0 if boot == . 
	
	bys asid year: g n = _n 
	
	foreach i in  1 2 3 4 5 6  {
		if `i' == 1 | `i' == 2  {			
				reghdfe `varlist`i'' if boot , vce(cluster asid)  
			}
		if `i' > 2   {
				reghdfe `varlist`i'' if boot  ///
					, absorb(i.year i.asid i.eco#i.year) ///
						vce(cluster asid) 
			}			
	cap drop herdsizehat`i'
		predict herdsizehat`i', xb 
	}
		
	*dropping duplicate years 	
	drop if n > 1 
	keep if year > 1984
	
	tempfile hshat  
	save `hshat'
	
	* long difference 
	if `diff' == 20 {
		loc sy = 1987
		loc ey = 2002
		loc tp = 16
	}
	if `diff' == 10 {
		loc sy = 1987
		loc ey = 2012
		loc tp = 26
	}
		
	foreach a of numlist `sy'/`ey' {
		use `hshat' , clear   

		*SGR WTHR 
		glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
		glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
		glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
		glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
		
		di "$SGRAUT"
		di "$SGRWTR"
		di "$SGRSPR"
		di "$SGRSMR"
	
		loc d `diff' 
		loc b = `a'+ `d'
			di "`a' , `b', `d'"
			
		cap drop period 
		g period = . 
		replace period = 1 if year >= `a'- 2 & year <= `a' + 2
		replace period = 2 if year >= `b'- 2 & year <= `b' + 2
			tab year period , m 
			drop if period == . 

		glo VARS lndvi $SGRAUT $SGRWTR $SGRSPR $SGRSMR herdsizehat1 herdsizehat2 herdsizehat3 herdsizehat4 herdsizehat5  herdsizehat6
			su 	$VARS if period ~=. 
		
		collapse (mean) $VARS , by(period asid eco aid)
			su $VARS
		
		reshape wide $VARS , i(asid eco) j(period)
			foreach v of glo VARS {
				di "`v'"
					su `v'?
				cap drop d_`v'
					g d_`v' = `v'2 - `v'1 
						su d_`v'
							cap drop `v'?
			}

		ren d_* * 	
	
	g year = `a'
	tempfile p`a'
	save `p`a'', replace 

	}

	clear 
	foreach a of numlist `sy'/`ey' {
		append using `p`a''
	}
	
	bsample round(1*`tp') , strata(asid)
	g boot2 = 1 	

	cap drop hshat 
	g hshat = . 
	la var hshat "$ \Delta \ln \widehat{HS}^{June}_{st}$"
	
	glo y lndvi  
	glo x hshat 
	
	loc varlist1 $y $x     
	loc varlist2 $y $x  ${SGRSMR}  
	loc varlist3 $y $x  ${SGRSMR}  
	loc varlist4 $y $x  ${SGRSMR} ${SGRSPR} 
	loc varlist5 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	
	if `tfe' == 1 {
		loc TFE "i.year"
	}
	
	if `tfe' == 2 {
		loc TFE " "
	}	
	
	foreach i in  1 2 3 4 5 6 {
		replace hshat = herdsizehat`i'
			if `i' == 1 {		
				reghdfe `varlist`i''  , vce(cluster asid) 
					return scalar b`i'1 = _b[hshat]
					return scalar b`i'2 = .
					return scalar b`i'3 = . 
					return scalar b`i'4 = .
					return scalar b`i'5 = .
					return scalar b`i'6 = .
				}
			if `i' == 2 {		
				reghdfe `varlist`i''  , vce(cluster asid)  
					return scalar b`i'1 = _b[hshat]
					return scalar b`i'2 = _b[sgr_smr_hed3]
					return scalar b`i'3 = _b[sgr_smr_hed4]
					return scalar b`i'4 = _b[sgr_smr_hed5]
					return scalar b`i'5 = _b[sgr_smr_prec]
					return scalar b`i'6 = _b[sgr_smr_ws_ave]					
				}				
			if `i' > 2 {
				reghdfe `varlist`i''   ///
					, absorb( `TFE' i.asid ) ///
						vce(cluster asid) 
					return scalar b`i'1 = _b[hshat]
					return scalar b`i'2 = _b[sgr_smr_hed3]
					return scalar b`i'3 = _b[sgr_smr_hed4]
					return scalar b`i'4 = _b[sgr_smr_hed5]
					return scalar b`i'5 = _b[sgr_smr_prec]
					return scalar b`i'6 = _b[sgr_smr_ws_ave]											
				}
		}
	end 
		
*------------------------------------------------------------------------------*
* 20-year diff with time FEs 
// 	simulate  ///
// 	 hshat1 = r(b11) hshat2 = r(b21) hshat3 = r(b31) hshat4 = r(b41) hshat5 = r(b51) hshat6 = r(b61) ///
// 	 sgr_smr_hed31 = r(b12) sgr_smr_hed32 = r(b22) sgr_smr_hed33 = r(b32) sgr_smr_hed34 = r(b42) sgr_smr_hed35 = r(b52) sgr_smr_hed36 = r(b62) ///
// 	 sgr_smr_hed41 = r(b13) sgr_smr_hed42 = r(b23) sgr_smr_hed43 = r(b33) sgr_smr_hed44 = r(b43) sgr_smr_hed45 = r(b53) sgr_smr_hed46 = r(b63) ///
// 	 sgr_smr_hed51 = r(b14) sgr_smr_hed52 = r(b24) sgr_smr_hed53 = r(b34) sgr_smr_hed54 = r(b44) sgr_smr_hed55 = r(b54) sgr_smr_hed56 = r(b64) ///
// 	 sgr_smr_prec1 = r(b15) sgr_smr_prec2 = r(b25) sgr_smr_prec3 = r(b35) sgr_smr_prec4 = r(b45) sgr_smr_prec5 = r(b55) sgr_smr_prec6 = r(b65) ///
// 	 sgr_smr_ws_ave1 = r(b16) sgr_smr_ws_ave2 = r(b26) sgr_smr_ws_ave3 = r(b36) sgr_smr_ws_ave4 = r(b46) sgr_smr_ws_ave5 = r(b56) sgr_smr_ws_ave6 = r(b66) ///
// 			, reps(10000) /* nodots nolegend */ seed(0123456789) ///
// 			saving("$dta/wrk/boot_ld20.dta" , replace): BSLD , diff(20) tfe(1) fst(9)
*------------------------------------------------------------------------------*
	use "$dta/wrk/boot_ld20.dta" , clear 
		g id=_n
		glo var "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
		reshape long ${var} ///
				, i(id) j(col)
		loc M ""
		loc SD ""
		foreach v of varlist $var {
			loc M " `M' m_`v' = `v' "
			loc SD " `SD' sd_`v' = `v' "
		}		
		collapse (mean) `M'  (sd) `SD' , by(col)	
		table () (col), stat(mean m_* ) stat(mean sd_* ) nototals nformat(%9.4f)
	save "$dta/wrk/sd_ld20.dta", replace
*------------------------------------------------------------------------------*
* 20-year diff without time FEs 
// 	simulate  ///
// 	 hshat1 = r(b11) hshat2 = r(b21) hshat3 = r(b31) hshat4 = r(b41) hshat5 = r(b51) hshat6 = r(b61) ///
// 	 sgr_smr_hed31 = r(b12) sgr_smr_hed32 = r(b22) sgr_smr_hed33 = r(b32) sgr_smr_hed34 = r(b42) sgr_smr_hed35 = r(b52) sgr_smr_hed36 = r(b62) ///
// 	 sgr_smr_hed41 = r(b13) sgr_smr_hed42 = r(b23) sgr_smr_hed43 = r(b33) sgr_smr_hed44 = r(b43) sgr_smr_hed45 = r(b53) sgr_smr_hed46 = r(b63) ///
// 	 sgr_smr_hed51 = r(b14) sgr_smr_hed52 = r(b24) sgr_smr_hed53 = r(b34) sgr_smr_hed54 = r(b44) sgr_smr_hed55 = r(b54) sgr_smr_hed56 = r(b64) ///
// 	 sgr_smr_prec1 = r(b15) sgr_smr_prec2 = r(b25) sgr_smr_prec3 = r(b35) sgr_smr_prec4 = r(b45) sgr_smr_prec5 = r(b55) sgr_smr_prec6 = r(b65) ///
// 	 sgr_smr_ws_ave1 = r(b16) sgr_smr_ws_ave2 = r(b26) sgr_smr_ws_ave3 = r(b36) sgr_smr_ws_ave4 = r(b46) sgr_smr_ws_ave5 = r(b56) sgr_smr_ws_ave6 = r(b66) ///
// 			, reps(10000) /* nodots nolegend */ seed(0123456789) ///
// 			saving("$dta/wrk/boot_ld20_noTFE.dta" , replace): BSLD , diff(20) tfe(2) fst(9)
*------------------------------------------------------------------------------*
	use "$dta/wrk/boot_ld20_noTFE.dta" , clear 
		g id=_n
		glo var "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
		reshape long ${var} ///
				, i(id) j(col)
		loc M ""
		loc SD ""
		foreach v of varlist $var {
			loc M " `M' m_`v' = `v' "
			loc SD " `SD' sd_`v' = `v' "
		}		
		collapse (mean) `M'  (sd) `SD' , by(col)	
		table () (col), stat(mean m_* ) stat(mean sd_* ) nototals nformat(%9.4f)
	save "$dta/wrk/sd_ld20_noTFE.dta", replace
*------------------------------------------------------------------------------*
* 10-year diff with time FEs 
// 	simulate  ///
// 	 hshat1 = r(b11) hshat2 = r(b21) hshat3 = r(b31) hshat4 = r(b41) hshat5 = r(b51) hshat6 = r(b61) ///
// 	 sgr_smr_hed31 = r(b12) sgr_smr_hed32 = r(b22) sgr_smr_hed33 = r(b32) sgr_smr_hed34 = r(b42) sgr_smr_hed35 = r(b52) sgr_smr_hed36 = r(b62) ///
// 	 sgr_smr_hed41 = r(b13) sgr_smr_hed42 = r(b23) sgr_smr_hed43 = r(b33) sgr_smr_hed44 = r(b43) sgr_smr_hed45 = r(b53) sgr_smr_hed46 = r(b63) ///
// 	 sgr_smr_hed51 = r(b14) sgr_smr_hed52 = r(b24) sgr_smr_hed53 = r(b34) sgr_smr_hed54 = r(b44) sgr_smr_hed55 = r(b54) sgr_smr_hed56 = r(b64) ///
// 	 sgr_smr_prec1 = r(b15) sgr_smr_prec2 = r(b25) sgr_smr_prec3 = r(b35) sgr_smr_prec4 = r(b45) sgr_smr_prec5 = r(b55) sgr_smr_prec6 = r(b65) ///
// 	 sgr_smr_ws_ave1 = r(b16) sgr_smr_ws_ave2 = r(b26) sgr_smr_ws_ave3 = r(b36) sgr_smr_ws_ave4 = r(b46) sgr_smr_ws_ave5 = r(b56) sgr_smr_ws_ave6 = r(b66) ///
// 			, reps(10000) /* nodots nolegend */ seed(0123456789) ///
// 			saving("$dta/wrk/boot_ld10.dta" , replace): BSLD , diff(10) tfe(1) fst(9)
*------------------------------------------------------------------------------*
	use "$dta/wrk/boot_ld10.dta" , clear 
		g id=_n
		glo var "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
		reshape long ${var} ///
				, i(id) j(col)
		loc M ""
		loc SD ""
		foreach v of varlist $var {
			loc M " `M' m_`v' = `v' "
			loc SD " `SD' sd_`v' = `v' "
		}		
		collapse (mean) `M'  (sd) `SD' , by(col)	
		table () (col), stat(mean m_* ) stat(mean sd_* ) nototals nformat(%9.4f)
	save "$dta/wrk/sd_ld10.dta", replace
*------------------------------------------------------------------------------*
* 10-year diff without time FEs 
// 	simulate  ///
// 	 hshat1 = r(b11) hshat2 = r(b21) hshat3 = r(b31) hshat4 = r(b41) hshat5 = r(b51) hshat6 = r(b61) ///
// 	 sgr_smr_hed31 = r(b12) sgr_smr_hed32 = r(b22) sgr_smr_hed33 = r(b32) sgr_smr_hed34 = r(b42) sgr_smr_hed35 = r(b52) sgr_smr_hed36 = r(b62) ///
// 	 sgr_smr_hed41 = r(b13) sgr_smr_hed42 = r(b23) sgr_smr_hed43 = r(b33) sgr_smr_hed44 = r(b43) sgr_smr_hed45 = r(b53) sgr_smr_hed46 = r(b63) ///
// 	 sgr_smr_hed51 = r(b14) sgr_smr_hed52 = r(b24) sgr_smr_hed53 = r(b34) sgr_smr_hed54 = r(b44) sgr_smr_hed55 = r(b54) sgr_smr_hed56 = r(b64) ///
// 	 sgr_smr_prec1 = r(b15) sgr_smr_prec2 = r(b25) sgr_smr_prec3 = r(b35) sgr_smr_prec4 = r(b45) sgr_smr_prec5 = r(b55) sgr_smr_prec6 = r(b65) ///
// 	 sgr_smr_ws_ave1 = r(b16) sgr_smr_ws_ave2 = r(b26) sgr_smr_ws_ave3 = r(b36) sgr_smr_ws_ave4 = r(b46) sgr_smr_ws_ave5 = r(b56) sgr_smr_ws_ave6 = r(b66) ///
// 			, reps(10000) /* nodots nolegend */ seed(0123456789) ///
// 			saving("$dta/wrk/boot_ld10_noTFE.dta" , replace): BSLD , diff(10) tfe(2) fst(9)
*------------------------------------------------------------------------------*
	use "$dta/wrk/boot_ld10_noTFE.dta" , clear 
		g id=_n
		glo var "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
		reshape long ${var} ///
				, i(id) j(col)
		loc M ""
		loc SD ""
		foreach v of varlist $var {
			loc M " `M' m_`v' = `v' "
			loc SD " `SD' sd_`v' = `v' "
		}		
		collapse (mean) `M'  (sd) `SD' , by(col)	
		table () (col), stat(mean m_* ) stat(mean sd_* ) nototals nformat(%9.4f)
	save "$dta/wrk/sd_ld10_noTFE.dta", replace
*------------------------------------------------------------------------------*

********************************************************************************
*------------------------------------------------------------------------------*
* First step: The Long Difference Approach
*------------------------------------------------------------------------------*
	use "$dta/wrk/wrk_soum_ld.dta", clear 

	glo y survey 
	glo x census 
	
	glo WGRSMR "lag_wgr_smr_prec lag_wgr_smr_hed5"
	glo WGRAUT "wgr_aut_ced3"
	glo WGRWTR "wgr_wtr_sd wgr_wtr_ced3 wgr_wtr_wed3"
	glo WGRSPR "wgr_spr_ced3"
	
	glo SGRAUT "sgr_aut_hed* sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed* sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed* sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed* sgr_smr_prec sgr_smr_ws_ave"
	
	
	loc varlist1 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} 
	loc varlist2 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}   
	loc varlist3 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}
	loc varlist4 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR}
	loc varlist5 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	
	local T "FS"
	foreach i in  1 2 3 4 5 6  {
		if `i' == 1 | `i' == 2  {
			eststo `T'_`i': reghdfe `varlist`i'' ///
				, vce(cluster asid)  
				test ${WGRSMR}  ${WGRAUT} ${WGRSPR} ${WGRWTR} 
					return list 
				estadd scalar Ftest = `r(F)' : `T'_`i'
				estadd loc tFE "No" : `T'_`i'
				estadd loc sFE "No" : `T'_`i'
				estadd loc eyFE "No" : `T'_`i'
				estadd loc SPRW "No" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'
				}
		if `i' == 3   {
			eststo `T'_`i': reghdfe `varlist`i'' ///
				, absorb(i.year i.asid i.eco#i.year) ///
					vce(cluster asid) 
				test ${WGRSMR}  ${WGRAUT} ${WGRSPR} ${WGRWTR} 
					return list 
				estadd scalar Ftest = `r(F)' : `T'_`i'
				estadd loc tFE "Yes" : `T'_`i'
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc eyFE "Yes" : `T'_`i'
				estadd loc SPRW "No" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'
				}			
		if `i' == 4   {
			eststo `T'_`i': reghdfe `varlist`i'' ///
				, absorb(i.year i.asid i.eco#i.year) ///
					vce(cluster asid) 
				test ${WGRSMR}  ${WGRAUT} ${WGRSPR} ${WGRWTR} 
					return list 
				estadd scalar Ftest = `r(F)' : `T'_`i'
				estadd loc tFE "Yes" : `T'_`i'
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc eyFE "Yes" : `T'_`i'
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc SWF "YNN" : `T'_`i'
				}
		if `i' == 5   {
			eststo `T'_`i': reghdfe `varlist`i'' ///
				, absorb(i.year i.asid i.eco#i.year) ///
					vce(cluster asid) 
				test ${WGRSMR}  ${WGRAUT} ${WGRSPR} ${WGRWTR} 
					return list 
				estadd scalar Ftest = `r(F)' : `T'_`i'
				estadd loc tFE "Yes" : `T'_`i'
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc eyFE "Yes" : `T'_`i'
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "Yes" : `T'_`i'
				estadd loc SWF "YYN" : `T'_`i'
				}
		if `i' == 6  {
			eststo `T'_`i': reghdfe `varlist`i'' ///
				, absorb(i.year i.asid i.eco#i.year) ///
					vce(cluster asid) nocons 
				test ${WGRSMR}  ${WGRAUT} ${WGRSPR} ${WGRWTR} 
					return list 
				estadd scalar Ftest = `r(F)' : `T'_`i'
				estadd loc tFE "Yes" : `T'_`i'
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc eyFE "Yes" : `T'_`i'
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "Yes" : `T'_`i'
				estadd loc WTRW "Yes" : `T'_`i'
				estadd loc SWF "YYY" : `T'_`i'
				}			
		cap drop herdsizehat`i'
			predict herdsizehat`i', xb 
		}

	save "$dta/wrk/wrk_soum_long.dta", replace  
********************************************************************************			
	
			
*------------------------------------------------------------------------------*
* 20-year differencing of Predicted HS 
*------------------------------------------------------------------------------*

	foreach a of numlist 1987/2002 {
		
		use "$dta/wrk/wrk_soum_long.dta" , clear   

			*SGR WTHR 
			glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
			glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
			glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
			glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
			
			di "$SGRAUT"
			di "$SGRWTR"
			di "$SGRSPR"
			di "$SGRSMR"
		
			loc d 20 
			loc b = `a'+ `d'
				di "`a' , `b', `d'"
				
			cap drop period 
			g period = . 
			replace period = 1 if year >= `a'- 2 & year <= `a' + 2
			replace period = 2 if year >= `b'- 2 & year <= `b' + 2
				tab year period , m 
					drop if period == . 
				
			glo VARS lndvi $SGRAUT $SGRWTR $SGRSPR $SGRSMR herdsizehat1 herdsizehat2 herdsizehat3 herdsizehat4 herdsizehat5  herdsizehat6
				su 	$VARS if period ~=. 
			
			collapse (mean) $VARS , by(period asid eco aid)
				su $VARS
			
			reshape wide $VARS , i(asid eco) j(period)
			
				foreach v of glo VARS {
					di "`v'"
						su `v'?
					cap drop d_`v'
						g d_`v' = `v'2 - `v'1 
							su d_`v'
								cap drop `v'?
				}

			ren d_* * 	
		
			g year = `a'
			tempfile p`a'
			save `p`a'', replace 

			}
			
	clear 
	foreach a of numlist 1987/2002 {
		append using `p`a''
	}
	
		la var lndvi "$ \Delta \ln  B^{summer}_{st, SGR} $"
		* SGR 
		loc HED `" "GDD(0^\circ\text{C} ,5^\circ\text{C}]" "GDD(5^\circ\text{C},10^\circ\text{C}]" "GDD(10^\circ\text{C},15^\circ\text{C}]" "GDD(15^\circ\text{C},20^\circ\text{C}]" "GDD(>20^\circ\text{C})"  "'		
		* SGR , only HED 
		foreach ssn in smr aut wtr spr {
		* GDDs  
			foreach i of numlist 3/5 {
				loc h : word `i' of `HED'
					la var sgr_`ssn'_hed`i' "\quad $ \Delta `h' $ "
			}
		* total precipitation (m)
				la var sgr_`ssn'_prec " \quad $ \Delta \text{Precipitation (m, acc.)}$  "
		* ave. wind speed (m/s)		
			la var sgr_`ssn'_ws_ave " \quad $ \Delta \text{Wind speed (m/s, ave.)}$  "
		}
		
	save "$dta/wrk/wrk_soum_panel_20.dta" , replace    
*------------------------------------------------------------------------------*

********************************************************************************
********************************************************************************
**# Table G.1: Second Stage: A Panel of 20-year Differences
********************************************************************************
*------------------------------------------------------------------------------*
* Second Step: Panel of 20-year differences with year FEs 
*------------------------------------------------------------------------------*
{
	use "$dta/wrk/wrk_soum_panel_20.dta" , clear     

	cap drop hshat 
	g hshat = . 
	la var hshat "$ \Delta \ln \widehat{HS}^{June}_{st}$"
	
	glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
			
	glo y lndvi  
	glo x hshat 

	loc varlist1 $y $x     
	loc varlist2 $y $x  ${SGRSMR}  
	loc varlist3 $y $x  ${SGRSMR}  
	loc varlist4 $y $x  ${SGRSMR} ${SGRSPR} 
	loc varlist5 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	
	cap drop yr*
	tab year, g(yr)
	
	loc YFE ""
	foreach y of numlist 1/16 {
		loc YFE "`YFE' yr`y'"
		di "`YFE'"
	}	
	
	cap drop cym*
	tab asid, g(cym)

	loc SFE ""
	foreach s of numlist 1/339 {
		loc SFE "`SFE' cym`s'"
		di "`SFE'"
	}
	
	cap drop ecoyear 
	egen ecoyear = group(eco year)
	cap drop ecoyr*
	tab ecoyear, g(ecoyr)	
	
	loc EYFE ""
	foreach e of numlist 1/80 {
		loc EYFE "`EYFE' ecoyr`e'"
		di "`EYFE'"
	}
	
	glo sumvar "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
	*bringing bootstrapped SE 
	preserve 
		use "$dta/wrk/bse/sd_ld20.dta", clear 
			matrix S = J(6,6,.)
			matrix rownames S = se se se se se se
			matrix colnames S = ${sumvar} 					
			matrix list S 
			
			foreach i of numlist 1/6 {
			loc j = 0 
				foreach v in ${sumvar}  {
					loc `j++'
					su sd_`v' if col == `i'
					matrix S[`i',`j'] = r(mean)
				} 
			}
			matrix list S 
	restore 
					
	local T "PD"
	foreach i in  1 2 3 4 5 6 {
		replace hshat = herdsizehat`i'
		if `i' == 1 {
				cap drop _mysample 
				eststo `T'_`i': reg `varlist`i''  , vce(cluster asid)
				
					matrix P = J(1,1,.)
					matrix rownames P = pvalue
					matrix colnames P = hshat									
						loc j = 0 
						foreach v of varlist hshat   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..1]
					matrix PV = P[1, 1..1]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
										
					estadd loc SPRW "No" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "No" : `T'_`i'
					estadd loc tFE "No" : `T'_`i'
					estadd loc sFE "No" : `T'_`i'
					estadd loc eyFE "No" : `T'_`i'
					estadd loc SWF "NNN" : `T'_`i'

					estadd scalar SH1=0  : `T'_`i'
					estadd scalar SH2=100  : `T'_`i'
					estadd scalar SH3=0  : `T'_`i'
			}
			
		if `i' == 2 {

				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					
				
				estadd loc SPRW "No" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc tFE "No" : `T'_`i'
				estadd loc sFE "No" : `T'_`i'
				estadd loc eyFE "No" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave ///
						)
					
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 

					di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
					ereturn list 
					
				estadd scalar SH1=0 : `T'_`i'
				estadd scalar SH2=`e(SH1)'  : `T'_`i'
				estadd scalar SH3=`e(SH2)'  : `T'_`i'

			}
		 	
		if `i' == 3 {
	
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `YFE' , vce(cluster asid)

					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}    {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc tFE "Yes" : `T'_`i'
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc eyFE "Yes" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'

				shapley2 , stat(r2) force ///
					group( ///
						`SFE'  ///
						`YFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave  ///
						sgr_spr_hed3 ///
						sgr_spr_hed4 ///
						sgr_spr_hed5 ///
						sgr_spr_prec ///
						sgr_spr_ws_ave ///
						)
		
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
			}
		if `i' == 4 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `YFE' , vce(cluster asid)

					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}    {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YNN" : `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						`SFE'  ///
						`YFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4   ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave  ///
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///			
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave ///
						) 
					 
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
			}
		if `i' == 5 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `YFE', vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}    {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "Yes" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YYN" : `T'_`i'

				shapley2, stat(r2) ///
					group( ///
						`SFE'  ///
						`YFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave   ///							
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave   ///
						sgr_aut_hed3  ///
						sgr_aut_hed4  ///
						sgr_aut_hed5  ///
						sgr_aut_prec  ///
						sgr_aut_ws_ave ///
						) 			 
					
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
			}
		if `i' == 6 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `YFE', vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}    {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "Yes" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YYY" : `T'_`i'

				shapley2, stat(r2) ///
					group( ///
						`SFE'  ///
						`YFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave   ///							
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave   ///
						sgr_aut_hed3  ///
						sgr_aut_hed4  ///
						sgr_aut_hed5  ///
						sgr_aut_prec  ///
						sgr_aut_ws_ave ///
						) 			 
					
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 
					di "`SH1' `SH2' `SH3'"
					ereturn list 
				estadd scalar SH1=`e(SH1)'  : `T'_`i'
				estadd scalar SH2=`e(SH2)'  : `T'_`i'
				estadd scalar SH3=`e(SH3)'  : `T'_`i'
			}
	}

	local tex "pd20_1987_2020_bse"
	 esttab PD_* using "$olf/tab/supp/`tex'.tex", replace substitute(\_ _) ///
			cells(b(fmt(3) star ) SD(par fmt(3) pvalue(PV)) ) /// //se(par fmt(3))
			star(* 0.10 ** 0.05 *** 0.01) ///
			label compress nogaps nodepvars collabels(none) nomtitles ///
			booktabs alignment(D{.}{.}{-1}) fragment ///
			mgroups("(a) Dependent variable: $ \Delta \ln B^{summer}_{st, SGR} $" , pattern(1 0 0 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			stats(sFE SWF N r2 HH SH2 SH3 SH1  , ///
				fmt( 0 0 0 2 0 2 2 2 ) ///
			labels( ///
				"Soum FEs, Period FEs"  ///
				"Spring, Winter, Fall, SGR"  ///			
			"$ N $" "$ R^2 $ " ///
			"\\ \it Shapley-Owen decomposition: \\ \midrule \quad \% of $ R^2$" ///
				"\qquad Herdsize" ///
				"\qquad Climate" ///
				"\qquad FEs" ///
			) ///
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
			) ) ///
			keep(hshat sgr_smr_*  ) ///
			order(hshat sgr_* wgr_* ) ///
			interaction( $ \times $) ///
			indicate( ///
			, label(\multicolumn{1}{c}{Y} \multicolumn{1}{c}{N})) ///
			refcat( ///
				sgr_smr_hed3 "\it Summer weather, SGR" ///
				sgr_aut_hed3 "\it Fall weather, SGR" ///
				sgr_spr_hed3 "\it  Spring weather, SGR" ///
				sgr_wtr_hed3 "\it  Winter weather, SGR" ///
				, nolabel) 	
			
	}
********************************************************************************
**# Table G.2: Second Stage: A Panel of 20-year Differences without Period FEs
********************************************************************************
*------------------------------------------------------------------------------*
* Second Step: Panel of 20-year differences without year FEs 
*------------------------------------------------------------------------------*
{	
	use "$dta/wrk/wrk_soum_panel_20.dta" , clear     
	
	cap drop hshat 
	g hshat = . 
	la var hshat "$ \Delta \ln \widehat{HS}^{June}_{st}$"
	
	glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
	
	cap drop hshat 
	g hshat = . 
	la var hshat "$ \Delta \ln \widehat{HS}^{June}_{st}$"
	
	glo y lndvi  
	glo x hshat 
	
	loc varlist1 $y $x     
	loc varlist2 $y $x  ${SGRSMR}  
	loc varlist3 $y $x  ${SGRSMR}  
	loc varlist4 $y $x  ${SGRSMR} ${SGRSPR} 
	loc varlist5 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}

	cap drop yr*
	tab year, g(yr)
	
	loc YFE ""
	foreach y of numlist 1/16 {
		loc YFE "`YFE' yr`y'"
		di "`YFE'"
	}	
	
	cap drop cym*
	tab asid, g(cym)

	loc SFE ""
	foreach s of numlist 1/339 {
		loc SFE "`SFE' cym`s'"
		di "`SFE'"
	}
	
	cap drop ecoyear 
	egen ecoyear = group(eco year)
	cap drop ecoyr*
	tab ecoyear, g(ecoyr)	
	
	loc EYFE ""
	foreach e of numlist 1/80 {
		loc EYFE "`EYFE' ecoyr`e'"
		di "`EYFE'"
	}

	glo sumvar "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
	*bringing bootstrapped SE 
	preserve 
		use "$dta/wrk/bse/sd_ld20_noTFE.dta", clear 
		matrix S = J(6,6,.)
		matrix rownames S = se se se se se se
		matrix colnames S = ${sumvar} 					
		matrix list S 
		
		foreach i of numlist 1/6 {
		loc j = 0 
			foreach v in ${sumvar}  {
				loc `j++'
				su sd_`v' if col == `i'
				matrix S[`i',`j'] = r(mean)
			} 
		}
		matrix list S 
	restore
	
	local T "PD"
	foreach i in  1 2 3 4 5 6 {
		replace hshat = herdsizehat`i'
		if `i' == 1 {
				cap drop _mysample 
				eststo `T'_`i': reg `varlist`i''  , vce(cluster asid)
					matrix P = J(1,1,.)
					matrix rownames P = pvalue
					matrix colnames P = hshat									
						loc j = 0 
						foreach v of varlist hshat   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..1]
					matrix PV = P[1, 1..1]
					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
				
					estadd loc SPRW "No" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "No" : `T'_`i'
					estadd loc tFE "No" : `T'_`i'
					estadd loc sFE "No" : `T'_`i'
					estadd loc eyFE "No" : `T'_`i'
					estadd loc SWF "NNN" : `T'_`i'
		
					estadd scalar SH1=0  : `T'_`i'
					estadd scalar SH2=100  : `T'_`i'
					estadd scalar SH3=0  : `T'_`i'
			}
		if `i' == 2 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
				
					estadd loc SPRW "No" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "No" : `T'_`i'
					estadd loc tFE "No" : `T'_`i'
					estadd loc sFE "No" : `T'_`i'
					estadd loc eyFE "No" : `T'_`i'
					estadd loc SWF "NNN" : `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave ///
						)
					
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=0 : `T'_`i'
					estadd scalar SH2=`e(SH1)'  : `T'_`i'
					estadd scalar SH3=`e(SH2)'  : `T'_`i'
			}
		if `i' == 3 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE'  , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
							
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "No" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "NNN" : `T'_`i'

				shapley2 , stat(r2) force ///
					group( ///
						`SFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave  ///
						sgr_spr_hed3 ///
						sgr_spr_hed4 ///
						sgr_spr_hed5 ///
						sgr_spr_prec ///
						sgr_spr_ws_ave ///
						)
		
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
	 
			}
		if `i' == 4 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'				
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YNN" : `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						`SFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4   ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave  ///
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///			
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave ///
						) 
					 
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'

			}
		if `i' == 5 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'				
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "Yes" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YYN" : `T'_`i'

				shapley2, stat(r2) ///
					group( ///
						`SFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave   ///							
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave   ///
						sgr_aut_hed3  ///
						sgr_aut_hed4  ///
						sgr_aut_hed5  ///
						sgr_aut_prec  ///
						sgr_aut_ws_ave ///
						) 			 
					
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
				
			}
		if `i' == 6 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'				
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "Yes" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YYY" : `T'_`i'

				shapley2, stat(r2) ///
					group( ///
						`SFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave   ///							
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave   ///
						sgr_aut_hed3  ///
						sgr_aut_hed4  ///
						sgr_aut_hed5  ///
						sgr_aut_prec  ///
						sgr_aut_ws_ave ///
						) 			 
					
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
				
			}
	}
	
	local tex "pd20_1987_2020_woYFE_bse"
	 esttab PD_* using "$olf/tab/supp/`tex'.tex", replace substitute(\_ _) ///
			cells(b(fmt(3) star ) SD(par fmt(3) pvalue(PV)) ) /// //se(par fmt(3))
			star(* 0.10 ** 0.05 *** 0.01) ///
			label compress nogaps nodepvars collabels(none) nomtitles ///
			booktabs alignment(D{.}{.}{-1}) fragment ///
			mgroups("(a) Dependent variable: $ \Delta \ln B^{summer}_{st, SGR} $" , pattern(1 0 0 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			stats(sFE SWF N r2 HH SH2 SH3 SH1  , ///
				fmt( 0 0 0 2 0 2 2 2 ) ///
			labels( ///
				"Soum FEs"  ///
				"Spring, Winter, Fall, SGR"  ///			
			"$ N $" "$ R^2 $ " ///
			"\\ \it Shapley-Owen decomposition: \\ \midrule \quad \% of $ R^2$" ///
				"\qquad Herdsize" ///
				"\qquad Climate" ///
				"\qquad FEs" ///
			) ///
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
			) ) ///
			keep(hshat sgr_smr_*  ) ///
			order(hshat sgr_* wgr_* ) ///
			interaction( $ \times $) ///
			indicate( ///
			, label(\multicolumn{1}{c}{Y} \multicolumn{1}{c}{N})) ///
			refcat( ///
				sgr_smr_hed3 "\it Summer weather, SGR" ///
				sgr_aut_hed3 "\it Fall weather, SGR" ///
				sgr_spr_hed3 "\it  Spring weather, SGR" ///
				sgr_wtr_hed3 "\it  Winter weather, SGR" ///
				, nolabel) 		
			
}

*------------------------------------------------------------------------------*
* 10-year differencing of Predicted HS 
*------------------------------------------------------------------------------*

	foreach a of numlist 1987/2012 {
		
	use "$dta/wrk/wrk_soum_long.dta" , clear   


		*SGR WTHR 
		glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
		glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
		glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
		glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
		
		di "$SGRAUT"
		di "$SGRWTR"
		di "$SGRSPR"
		di "$SGRSMR"
	
		loc d 10 
		loc b = `a'+ `d'
			di "`a' , `b', `d'"
			
		cap drop period 
		g period = . 
		replace period = 1 if year >= `a'- 2 & year <= `a' + 2
		replace period = 2 if year >= `b'- 2 & year <= `b' + 2
		
		tab year period 
		
		glo VARS lndvi $SGRAUT $SGRWTR $SGRSPR $SGRSMR herdsizehat1 herdsizehat2 herdsizehat3 herdsizehat4 herdsizehat5 herdsizehat6
		su 	$VARS if period ~=. 

		drop if period == . 
		
		collapse (mean) $VARS , by(period asid eco aid)
				
		su $VARS
		
		reshape wide $VARS , i(asid eco) j(period)
		
		
		
		foreach v of glo VARS {
			di "`v'"
				su `v'?
			cap drop d_`v'
				g d_`v' = `v'2 - `v'1 
					su d_`v'
						cap drop `v'?
		}

		ren d_* * 	
	
	g year = `a'
	tempfile p`a'
	save `p`a'', replace 

	}
	
	clear 
	foreach a of numlist 1987/2012 {
		append using `p`a''
	}
	
		la var lndvi "$ \Delta \ln  B^{summer}_{st, SGR} $"
		* SGR 
		loc HED `" "GDD(0^\circ\text{C} ,5^\circ\text{C}]" "GDD(5^\circ\text{C},10^\circ\text{C}]" "GDD(10^\circ\text{C},15^\circ\text{C}]" "GDD(15^\circ\text{C},20^\circ\text{C}]" "GDD(>20^\circ\text{C})"  "'		
		* SGR , only HED 
		foreach ssn in smr aut wtr spr {
		* GDDs  
			foreach i of numlist 3/5 {
				loc h : word `i' of `HED'
					la var sgr_`ssn'_hed`i' "\quad $ \Delta `h' $ "
			}
		* total precipitation (m)
				la var sgr_`ssn'_prec " \quad $ \Delta \text{Precipitation (m, acc.)}$  "
		* ave. wind speed (m/s)		
			la var sgr_`ssn'_ws_ave " \quad $ \Delta \text{Wind speed (m/s, ave.)}$  "
		}
		
	save "$dta/wrk/wrk_soum_panel_10.dta" , replace    
 
********************************************************************************
**# Table G.3: Second Stage: A Panel of 10-year Differences
******************************************************************************** 
*------------------------------------------------------------------------------*
* Second Step: Panel of 10-year differences with year FES 
*------------------------------------------------------------------------------*	
{	
	
	use "$dta/wrk/wrk_soum_panel_10.dta" , clear     

	cap drop hshat 
	g hshat = . 
	la var hshat "$ \Delta \ln \widehat{HS}^{June}_{st}$"
	
	glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
			
	glo y lndvi  
	glo x hshat 

	loc varlist1 $y $x     
	loc varlist2 $y $x  ${SGRSMR}  
	loc varlist3 $y $x  ${SGRSMR}  
	loc varlist4 $y $x  ${SGRSMR} ${SGRSPR} 
	loc varlist5 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	
	cap drop yr*
	tab year, g(yr)
	
	loc YFE ""
	foreach y of numlist 1/26 {
		loc YFE "`YFE' yr`y'"
		di "`YFE'"
	}	
	
	cap drop cym*
	tab asid, g(cym)

	loc SFE ""
	foreach s of numlist 1/339 {
		loc SFE "`SFE' cym`s'"
		di "`SFE'"
	}
	
	cap drop ecoyear 
	egen ecoyear = group(eco year)
	cap drop ecoyr*
	tab ecoyear, g(ecoyr)	
	
	loc EYFE ""
	foreach e of numlist 1/130 {
		loc EYFE "`EYFE' ecoyr`e'"
		di "`EYFE'"
	}
	
	glo sumvar "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
	*bringing bootstrapped SE 
	preserve 
		use "$dta/wrk/bse/sd_ld10.dta", clear 
		matrix S = J(6,6,.)
		matrix rownames S = se se se se se se
		matrix colnames S = ${sumvar} 					
		matrix list S 
			foreach i of numlist 1/6 {
			loc j = 0 
				foreach v in ${sumvar}  {
					loc `j++'
					su sd_`v' if col == `i'
					matrix S[`i',`j'] = r(mean)
				} 
			}
		matrix list S 
	restore 
					
	local T "PD"
	foreach i in  1 2 3 4 5 6 {
		replace hshat = herdsizehat`i'
		if `i' == 1 {
				cap drop _mysample 
				eststo `T'_`i': reg `varlist`i''  , vce(cluster asid)
				
					matrix P = J(1,1,.)
					matrix rownames P = pvalue
					matrix colnames P = hshat									
						loc j = 0 
						foreach v of varlist hshat   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..1]
					matrix PV = P[1, 1..1]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
										
					estadd loc SPRW "No" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "No" : `T'_`i'
					estadd loc tFE "No" : `T'_`i'
					estadd loc sFE "No" : `T'_`i'
					estadd loc eyFE "No" : `T'_`i'
					estadd loc SWF "NNN" : `T'_`i'

					estadd scalar SH1=0  : `T'_`i'
					estadd scalar SH2=100  : `T'_`i'
					estadd scalar SH3=0  : `T'_`i'
			}
			
		if `i' == 2 {

				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					
				
				estadd loc SPRW "No" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc tFE "No" : `T'_`i'
				estadd loc sFE "No" : `T'_`i'
				estadd loc eyFE "No" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave ///
						)
					
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 

					di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
					ereturn list 
					
				estadd scalar SH1=0 : `T'_`i'
				estadd scalar SH2=`e(SH1)'  : `T'_`i'
				estadd scalar SH3=`e(SH2)'  : `T'_`i'

			}
		 	
		if `i' == 3 {
	
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `YFE' , vce(cluster asid)

					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}    {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc tFE "Yes" : `T'_`i'
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc eyFE "Yes" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'

				shapley2 , stat(r2) force ///
					group( ///
						`SFE'  ///
						`YFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave  ///
						sgr_spr_hed3 ///
						sgr_spr_hed4 ///
						sgr_spr_hed5 ///
						sgr_spr_prec ///
						sgr_spr_ws_ave ///
						)
		
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
			}
		if `i' == 4 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `YFE' , vce(cluster asid)

					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}    {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YNN" : `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						`SFE'  ///
						`YFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4   ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave  ///
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///			
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave ///
						) 
					 
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
			}
		if `i' == 5 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `YFE', vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}    {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "Yes" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YYN" : `T'_`i'

				shapley2, stat(r2) ///
					group( ///
						`SFE'  ///
						`YFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave   ///							
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave   ///
						sgr_aut_hed3  ///
						sgr_aut_hed4  ///
						sgr_aut_hed5  ///
						sgr_aut_prec  ///
						sgr_aut_ws_ave ///
						) 			 
					
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
			}
		if `i' == 6 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `YFE', vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}    {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "Yes" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YYY" : `T'_`i'

				shapley2, stat(r2) ///
					group( ///
						`SFE'  ///
						`YFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave   ///							
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave   ///
						sgr_aut_hed3  ///
						sgr_aut_hed4  ///
						sgr_aut_hed5  ///
						sgr_aut_prec  ///
						sgr_aut_ws_ave ///
						) 			 
					
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 
					di "`SH1' `SH2' `SH3'"
					ereturn list 
				estadd scalar SH1=`e(SH1)'  : `T'_`i'
				estadd scalar SH2=`e(SH2)'  : `T'_`i'
				estadd scalar SH3=`e(SH3)'  : `T'_`i'
			}
	}

	local tex "pd10_1987_2020_bse"
	 esttab PD_* using "$olf/tab/supp/`tex'.tex", replace substitute(\_ _) ///
			cells(b(fmt(3) star ) SD(par fmt(3) pvalue(PV)) ) /// //se(par fmt(3))
			star(* 0.10 ** 0.05 *** 0.01) ///
			label compress nogaps nodepvars collabels(none) nomtitles ///
			booktabs alignment(D{.}{.}{-1}) fragment ///
			mgroups("(a) Dependent variable: $ \Delta \ln B^{summer}_{st, SGR} $" , pattern(1 0 0 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			stats(sFE SWF N r2 HH SH2 SH3 SH1  , ///
				fmt( 0 0 0 2 0 2 2 2 ) ///
			labels( ///
				"Soum FEs, Period FEs"  ///
				"Spring, Winter, Fall, SGR"  ///			
			"$ N $" "$ R^2 $ " ///
			"\\ \it Shapley-Owen decomposition: \\ \midrule \quad \% of $ R^2$" ///
				"\qquad Herdsize" ///
				"\qquad Climate" ///
				"\qquad FEs" ///
			) ///
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
			) ) ///
			keep(hshat sgr_smr_*  ) ///
			order(hshat sgr_* wgr_* ) ///
			interaction( $ \times $) ///
			indicate( ///
			, label(\multicolumn{1}{c}{Y} \multicolumn{1}{c}{N})) ///
			refcat( ///
				sgr_smr_hed3 "\it Summer weather, SGR" ///
				sgr_aut_hed3 "\it Fall weather, SGR" ///
				sgr_spr_hed3 "\it  Spring weather, SGR" ///
				sgr_wtr_hed3 "\it  Winter weather, SGR" ///
				, nolabel) 	
}			

********************************************************************************
**# Table G.4: Second Stage: A Panel of 10-year Differences without Period FEs
******************************************************************************** 
*------------------------------------------------------------------------------*
* Second Step: Panel of 10-year differences without year FES 
*------------------------------------------------------------------------------*	
{
	use "$dta/wrk/wrk_soum_panel_10.dta" , clear     
			
	cap drop hshat 
	g hshat = . 
	la var hshat "$ \Delta \ln \widehat{HS}^{June}_{st}$"
	
	glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
			
	glo y lndvi  
	glo x hshat 

	loc varlist1 $y $x     
	loc varlist2 $y $x  ${SGRSMR}  
	loc varlist3 $y $x  ${SGRSMR}  
	loc varlist4 $y $x  ${SGRSMR} ${SGRSPR} 
	loc varlist5 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x  ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	
	cap drop yr*
	tab year, g(yr)
	
	loc YFE ""
	foreach y of numlist 1/26 {
		loc YFE "`YFE' yr`y'"
		di "`YFE'"
	}	
	
	cap drop cym*
	tab asid, g(cym)

	loc SFE ""
	foreach s of numlist 1/339 {
		loc SFE "`SFE' cym`s'"
		di "`SFE'"
	}
	
	cap drop ecoyear 
	egen ecoyear = group(eco year)
	cap drop ecoyr*
	tab ecoyear, g(ecoyr)	
	
	loc EYFE ""
	foreach e of numlist 1/130 {
		loc EYFE "`EYFE' ecoyr`e'"
		di "`EYFE'"
	}
	
	glo sumvar "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"
	
	*bringing bootstrapped SE 
	preserve 
		use "$dta/wrk/bse/sd_ld10_noTFE.dta", clear 
		matrix S = J(6,6,.)
		matrix rownames S = se se se se se se
		matrix colnames S = ${sumvar} 					
		matrix list S 
		
		foreach i of numlist 1/6 {
		loc j = 0 
			foreach v in ${sumvar}  {
				loc `j++'
				su sd_`v' if col == `i'
				matrix S[`i',`j'] = r(mean)
			} 
		}
		matrix list S 
	restore
					
	local T "PD"
	foreach i in  1 2 3 4 5 6 {
		replace hshat = herdsizehat`i'
		if `i' == 1 {
				cap drop _mysample 
				eststo `T'_`i': reg `varlist`i''  , vce(cluster asid)
					matrix P = J(1,1,.)
					matrix rownames P = pvalue
					matrix colnames P = hshat									
						loc j = 0 
						foreach v of varlist hshat   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..1]
					matrix PV = P[1, 1..1]
					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
				
					estadd loc SPRW "No" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "No" : `T'_`i'
					estadd loc tFE "No" : `T'_`i'
					estadd loc sFE "No" : `T'_`i'
					estadd loc eyFE "No" : `T'_`i'
					estadd loc SWF "NNN" : `T'_`i'
		
					estadd scalar SH1=0  : `T'_`i'
					estadd scalar SH2=100  : `T'_`i'
					estadd scalar SH3=0  : `T'_`i'
			}
		if `i' == 2 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
				
					estadd loc SPRW "No" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "No" : `T'_`i'
					estadd loc tFE "No" : `T'_`i'
					estadd loc sFE "No" : `T'_`i'
					estadd loc eyFE "No" : `T'_`i'
					estadd loc SWF "NNN" : `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave ///
						)
					
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=0 : `T'_`i'
					estadd scalar SH2=`e(SH1)'  : `T'_`i'
					estadd scalar SH3=`e(SH2)'  : `T'_`i'
			}
		if `i' == 3 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE'  , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
							
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "No" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "NNN" : `T'_`i'

				shapley2 , stat(r2) force ///
					group( ///
						`SFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave  ///
						sgr_spr_hed3 ///
						sgr_spr_hed4 ///
						sgr_spr_hed5 ///
						sgr_spr_prec ///
						sgr_spr_ws_ave ///
						)
		
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
	 
			}
		if `i' == 4 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'				
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "No" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YNN" : `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						`SFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4   ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave  ///
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///			
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave ///
						) 
					 
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'

			}
		if `i' == 5 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'				
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "Yes" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YYN" : `T'_`i'

				shapley2, stat(r2) ///
					group( ///
						`SFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave   ///							
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave   ///
						sgr_aut_hed3  ///
						sgr_aut_hed4  ///
						sgr_aut_hed5  ///
						sgr_aut_prec  ///
						sgr_aut_ws_ave ///
						) 			 
					
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
				
			}
		if `i' == 6 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' , vce(cluster asid)
				
					matrix P = J(1,6,.)
					matrix rownames P = pvalue
					matrix colnames P = ${sumvar} 									
						loc j = 0 
						foreach v of varlist ${sumvar}   {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
					matrix SD = S[`i', 1..6]
					matrix PV = P[1, 1..6]

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'				
				
					estadd loc SPRW "Yes" : `T'_`i'
					estadd loc AUTW "Yes" : `T'_`i'
					estadd loc WTRW "Yes" : `T'_`i'
					estadd loc tFE "Yes" : `T'_`i'
					estadd loc sFE "Yes" : `T'_`i'
					estadd loc eyFE "Yes" : `T'_`i'
					estadd loc SWF "YYY" : `T'_`i'

				shapley2, stat(r2) ///
					group( ///
						`SFE' , ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave   ///							
						sgr_spr_hed3  ///
						sgr_spr_hed4  ///
						sgr_spr_hed5  ///
						sgr_spr_prec  ///
						sgr_spr_ws_ave  ///
						sgr_wtr_hed3  ///
						sgr_wtr_hed4  ///
						sgr_wtr_hed5  ///
						sgr_wtr_prec  ///
						sgr_wtr_ws_ave   ///
						sgr_aut_hed3  ///
						sgr_aut_hed4  ///
						sgr_aut_hed5  ///
						sgr_aut_prec  ///
						sgr_aut_ws_ave ///
						) 			 
					
					estadd scalar SH1 = e(shapley_rel)[1,1]*100 
					estadd scalar SH2 = e(shapley_rel)[2,1]*100 
					estadd scalar SH3 = e(shapley_rel)[3,1]*100 
						di "`SH1' `SH2' `SH3'"
						ereturn list 
					estadd scalar SH1=`e(SH1)'  : `T'_`i'
					estadd scalar SH2=`e(SH2)'  : `T'_`i'
					estadd scalar SH3=`e(SH3)'  : `T'_`i'
				
			}
	}
		
	local tex "pd10_1987_2020_woYFE_bse"
	 esttab PD_* using "$olf/tab/supp/`tex'.tex", replace substitute(\_ _) ///
			cells(b(fmt(3) star ) SD(par fmt(3) pvalue(PV)) ) /// //se(par fmt(3))
			star(* 0.10 ** 0.05 *** 0.01) ///
			label compress nogaps nodepvars collabels(none) nomtitles ///
			booktabs alignment(D{.}{.}{-1}) fragment ///
			mgroups("(a) Dependent variable: $ \Delta \ln B^{summer}_{st, SGR} $" , pattern(1 0 0 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			stats(sFE SWF N r2 HH SH2 SH3 SH1  , ///
				fmt( 0 0 0 2 0 2 2 2 ) ///
			labels( ///
				"Soum FEs"  ///
				"Spring, Winter, Fall, SGR"  ///			
			"$ N $" "$ R^2 $ " ///
			"\\ \it Shapley-Owen decomposition: \\ \midrule \quad \% of $ R^2$" ///
				"\qquad Herdsize" ///
				"\qquad Climate" ///
				"\qquad FEs" ///
			) ///
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
			) ) ///
			keep(hshat sgr_smr_*  ) ///
			order(hshat sgr_* wgr_* ) ///
			interaction( $ \times $) ///
			indicate( ///
			, label(\multicolumn{1}{c}{Y} \multicolumn{1}{c}{N})) ///
			refcat( ///
				sgr_smr_hed3 "\it Summer weather, SGR" ///
				sgr_aut_hed3 "\it Fall weather, SGR" ///
				sgr_spr_hed3 "\it  Spring weather, SGR" ///
				sgr_wtr_hed3 "\it  Winter weather, SGR" ///
				, nolabel) 	
}
		
		