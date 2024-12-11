// =============================================================================
// Filename:  do10_reg7_hs_gdd_20241210.do
// Author:     Avralt-Od Purevjav
// Last Modified:  Avraa
// Date:     2024-12-10
// =============================================================================
// Estimated Marginal Effects with Climate-Herd Size Interactions (Annex H)
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

	use "$dta/wrk/wrk_soum.dta" , clear 
		keep year asid su_*_total idw* *na_ave sgr_* wgr_* eco* 
		
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
			
		* interactions 
		cap drop survey_sgr_smr_hed3
		g survey_sgr_smr_hed3 = survey * sgr_smr_hed3
		la var survey_sgr_smr_hed3 "\quad $ \times \quad GDD(10^\circ\text{C},15^\circ\text{C}]$ "	
		
		cap drop survey_sgr_smr_hed4
		g survey_sgr_smr_hed4 = survey * sgr_smr_hed4
		la var survey_sgr_smr_hed4 "\quad $ \times \quad GDD(15^\circ\text{C},20^\circ\text{C}]$ "
			
		cap drop survey_sgr_smr_hed5
		g survey_sgr_smr_hed5 = survey * sgr_smr_hed5
		la var survey_sgr_smr_hed5 "\quad $ \times \quad GDD(>20^\circ\text{C})$ "
					
		cap drop survey_sgr_smr_prec
		g survey_sgr_smr_prec = survey * sgr_smr_prec
		la var survey_sgr_smr_prec "\quad $ \times $ \quad Precipitation (m, acc.) "
				
		cap drop survey_sgr_smr_ws_ave
		g survey_sgr_smr_ws_ave = survey * sgr_smr_ws_ave
		la var survey_sgr_smr_ws_ave "\quad $ \times $ \quad Wind speed (m/s, ave.) "
	
			
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

	save "$dta/wrk/wrk_soum_hs_gdd.dta", replace 
	
*------------------------------------------------------------------------------*
**# bootstrapping  
*------------------------------------------------------------------------------*
	
	cap program drop BSHSGDD 
	program define BSHSGDD, rclass
	syntax [, fst(integer 1) sst(integer 2)]
	
		* bootstrapping 
		use "$dta/wrk/wrk_soum_hs_gdd.dta" , clear 
		tempfile boot 
		save `boot', replace 
		
		*SGR WTHR 
		glo SGRAUT "sgr_aut_hed* sgr_aut_prec sgr_aut_ws_ave"
		glo SGRWTR "sgr_wtr_hed* sgr_wtr_prec sgr_wtr_ws_ave"
		glo SGRSPR "sgr_spr_hed* sgr_spr_prec sgr_spr_ws_ave"
		glo SGRSMR "sgr_smr_hed* sgr_smr_prec sgr_smr_ws_ave"
		
		xtset asid year 
		
		*WGR WTHR IVs 
		glo WGRSMR "lag_wgr_smr_prec lag_wgr_smr_hed5"
		glo WGRAUT "wgr_aut_ced3"
		glo WGRWTR "wgr_wtr_sd wgr_wtr_ced3 wgr_wtr_wed3"
		glo WGRSPR "wgr_spr_ced3"
		
		** FS 
		glo y survey 
		glo x census 
	
		loc varlist1 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} 
		loc varlist2 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}   
		loc varlist3 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}
		loc varlist4 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR}
		loc varlist5 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} 
		loc varlist6 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	
		use `boot', clear 
		
		keep if year > 2015
		keep year asid 
				
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
			
		drop if n > 1 
		keep if year > 1984
		
		bsample round(1*`sst') , strata(asid)
		g boot2 = 1 	
		
		cap drop hshat 
		g hshat = . 
		cap drop hshat_hed3
		g hshat_hed3 = .		
		cap drop hshat_hed4
		g hshat_hed4 = .			
		cap drop hshat_hed5
		g hshat_hed5 = .					
		cap drop hshat_prec
		g hshat_prec = .				
		cap drop hshat_ws
		g hshat_ws = .
		
		glo y lndvi  
		glo x hshat 
		glo i hshat_hed? hshat_prec hshat_ws
		
		loc varlist1 $y $x  
		loc varlist2 $y $x $i ${SGRSMR}  
		loc varlist3 $y $x $i ${SGRSMR}
		loc varlist4 $y $x $i ${SGRSMR} ${SGRSPR}  
		loc varlist5 $y $x $i ${SGRSMR} ${SGRSPR} ${SGRWTR} 
		loc varlist6 $y $x $i ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	
		foreach i in  1 2 3 4 5 6 {
			replace hshat = herdsizehat`i'
			replace hshat_hed3  = herdsizehat`i' * sgr_smr_hed3	
			replace hshat_hed4  = herdsizehat`i' * sgr_smr_hed4
			replace hshat_hed5  = herdsizehat`i' * sgr_smr_hed5
			replace hshat_prec = herdsizehat`i' * sgr_smr_prec 
			replace hshat_ws = herdsizehat`i' * sgr_smr_ws_ave  
	
			if `i' == 1 {		
				reghdfe `varlist`i''  , vce(cluster asid) 
					return scalar b`i'1 = _b[hshat]
					return scalar b`i'2 = .
					return scalar b`i'3 = .
					return scalar b`i'4 = .
					return scalar b`i'5 = .
					return scalar b`i'6 = .
					return scalar b`i'7 = .
					return scalar b`i'8 = .
					return scalar b`i'9 = .
					return scalar b`i'10 = .
					return scalar b`i'11 = .
				}
			if `i' == 2 {		
				reghdfe `varlist`i''  , vce(cluster asid)  
					return scalar b`i'1 = _b[hshat]
					return scalar b`i'2 = _b[sgr_smr_hed3]
					return scalar b`i'3 = _b[sgr_smr_hed4]
					return scalar b`i'4 = _b[sgr_smr_hed5]
					return scalar b`i'5 = _b[sgr_smr_prec]
					return scalar b`i'6 = _b[sgr_smr_ws_ave]
					return scalar b`i'7 = _b[hshat_hed3]
					return scalar b`i'8 = _b[hshat_hed4]
					return scalar b`i'9 = _b[hshat_hed5]
					return scalar b`i'10 = _b[hshat_prec]
					return scalar b`i'11 = _b[hshat_ws]				
				}				
			if `i' > 2 {
				reghdfe `varlist`i''   ///
					, absorb(i.year i.asid i.eco#i.year) ///
						vce(cluster asid) 
				return scalar b`i'1 = _b[hshat]
				return scalar b`i'2 = _b[sgr_smr_hed3]
				return scalar b`i'3 = _b[sgr_smr_hed4]
				return scalar b`i'4 = _b[sgr_smr_hed5]
				return scalar b`i'5 = _b[sgr_smr_prec]
				return scalar b`i'6 = _b[sgr_smr_ws_ave]
				return scalar b`i'7 = _b[hshat_hed3]
				return scalar b`i'8 = _b[hshat_hed4]
				return scalar b`i'9 = _b[hshat_hed5]
				return scalar b`i'10 = _b[hshat_prec]
				return scalar b`i'11 = _b[hshat_ws]								
				}
		}
	end

*------------------------------------------------------------------------------*
	simulate  ///
		hshat1 = r(b11) hshat2 = r(b21) hshat3 = r(b31) hshat4 = r(b41) hshat5 = r(b51) hshat6 = r(b61) ///
		sgr_smr_hed31 = r(b12) sgr_smr_hed32 = r(b22) sgr_smr_hed33 = r(b32) sgr_smr_hed34 = r(b42) sgr_smr_hed35 = r(b52) sgr_smr_hed36 = r(b62) ///
		sgr_smr_hed41 = r(b13) sgr_smr_hed42 = r(b23) sgr_smr_hed43 = r(b33) sgr_smr_hed44 = r(b43) sgr_smr_hed45 = r(b53) sgr_smr_hed46 = r(b63) ///
		sgr_smr_hed51 = r(b14) sgr_smr_hed52 = r(b24) sgr_smr_hed53 = r(b34) sgr_smr_hed54 = r(b44) sgr_smr_hed55 = r(b54) sgr_smr_hed56 = r(b64) ///
		sgr_smr_prec1 = r(b15) sgr_smr_prec2 = r(b25) sgr_smr_prec3 = r(b35) sgr_smr_prec4 = r(b45) sgr_smr_prec5 = r(b55) sgr_smr_prec6 = r(b65) ///
		sgr_smr_ws_ave1 = r(b16) sgr_smr_ws_ave2 = r(b26) sgr_smr_ws_ave3 = r(b36) sgr_smr_ws_ave4 = r(b46) sgr_smr_ws_ave5 = r(b56) sgr_smr_ws_ave6 = r(b66) ///
		hshat_hed31 = r(b17) hshat_hed32 = r(b27) hshat_hed33 = r(b37) hshat_hed34 = r(b47) hshat_hed35 = r(b57) hshat_hed36 = r(b67) ///
		hshat_hed41 = r(b18) hshat_hed42 = r(b28) hshat_hed43 = r(b38) hshat_hed44 = r(b48) hshat_hed45 = r(b58) hshat_hed46 = r(b68) ///
		hshat_hed51 = r(b19) hshat_hed52 = r(b29) hshat_hed53 = r(b39) hshat_hed54 = r(b49) hshat_hed55 = r(b59) hshat_hed56 = r(b69) ///
		hshat_prec1 = r(b110) hshat_prec2 = r(b210) hshat_prec3 = r(b310) hshat_prec4 = r(b410) hshat_prec5 = r(b510) hshat_prec6 = r(b610) ///
		hshat_ws1 = r(b111) hshat_ws2 = r(b211) hshat_ws3 = r(b311) hshat_ws4 = r(b411) hshat_ws5 = r(b511) hshat_ws6 = r(b611) ///
			, reps(10000) /* nodots nolegend */ seed(0123456789) ///
			saving("$dta/wrk/boot_hs_gdd.dta" , replace): BSHSGDD , fst(9) sst(40) 
*------------------------------------------------------------------------------*
	use "$dta/wrk/boot_hs_gdd.dta" , clear 
		g id=_n
		glo var "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave hshat_hed3 hshat_hed4 hshat_hed5 hshat_prec hshat_ws"
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
	save "$dta/wrk/sd_hs_gdd.dta", replace
*------------------------------------------------------------------------------*

********************************************************************************
**# Table H.1: OLS: Regressing log of soum-level SGR-based average NDVI on the log of June
* herdsize, SGR-based summer weather variables in levels (temperature bins, precipitation,
* wind), the interaction of the herdsize and summer weather variables, and other covariates.
********************************************************************************
*------------------------------------------------------------------------------*
* OLS 
*------------------------------------------------------------------------------*
	use  "$dta/wrk/wrk_soum_hs_gdd.dta", replace 
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
	
	glo y lndvi 
	glo x survey 
	glo i survey_sgr_smr_hed? survey_sgr_smr_prec survey_sgr_smr_ws_ave
	
	loc varlist1 $y $x   
	loc varlist2 $y $x $i ${SGRSMR}  
	loc varlist3 $y $x $i ${SGRSMR} 
	loc varlist4 $y $x $i ${SGRSMR} ${SGRSPR}
	loc varlist5 $y $x $i ${SGRSMR} ${SGRSPR} ${SGRWTR}
	loc varlist6 $y $x $i ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}

	local T "OLS"
	foreach i in 1 2 3 4 5 6 {
		if `i' < 3 {
			eststo `T'_`i': reghdfe `varlist`i''  ///
				, vce( cluster asid) residuals
					
				estadd loc tFE "No" : `T'_`i'
				estadd loc sFE "No" : `T'_`i'
				estadd loc eyFE "No" : `T'_`i'
		}
		if `i' >= 3 {	
			eststo `T'_`i': reghdfe `varlist`i''  ///
				, absorb(i.year i.asid i.eco#i.year, savefe) ///
					vce( cluster asid) residuals	
					
				estadd loc tFE "Yes" : `T'_`i'
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc eyFE "Yes" : `T'_`i'
		}
		if `i' == 1 {
				estadd loc SMRW "No" : `T'_`i'
				estadd loc SPRW "No" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc SFW "NNN" : `T'_`i'
			}
		if `i' == 2  {
				estadd loc SMRW "Yes" : `T'_`i'
				estadd loc SPRW "No" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc SFW "NNN" : `T'_`i'
			}			
		if `i' == 3   {
				estadd loc SMRW "Yes" : `T'_`i'
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "No" : `T'_`i'
				estadd loc SFW "NNN" : `T'_`i'
			}
		if `i' == 4   {
				estadd loc SMRW "Yes" : `T'_`i'
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "No" : `T'_`i'
				estadd loc WTRW "Yes" : `T'_`i'
				estadd loc SFW "YNN" : `T'_`i'
			}
		if `i' == 5  {
				estadd loc SMRW "Yes" : `T'_`i'
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "Yes" : `T'_`i'
				estadd loc WTRW "Yes" : `T'_`i'
				estadd loc SFW "YYN" : `T'_`i'
			}
		if `i' == 6 {
				estadd loc SMRW "Yes" : `T'_`i'
				estadd loc SPRW "Yes" : `T'_`i'
				estadd loc AUTW "Yes" : `T'_`i'
				estadd loc WTRW "Yes" : `T'_`i'
				estadd loc SFW "YYY" : `T'_`i'
			}			
	}
	
	local tex "ols_ndvi_gdd"
	quietly esttab OLS_* using "$olf/tab/supp/`tex'.tex", replace substitute(\_ _) ///
			cells(b(fmt(3) star) se(par fmt(3)) )  /// //se(par fmt(3))
			star(* 0.10 ** 0.05 *** 0.01) ///
			label compress nogaps nodepvars collabels(none) nomtitles ///
			booktabs alignment(D{.}{.}{-1}) fragment ///
			mgroups("(a) Dependent variable: $ \ln B^{summer}_{st, SGR} $ " , pattern(1 0 0 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			stats( sFE SMRW SFW N r2, fmt( 0 0 0 0 2 ) ///
				labels( "Soum, Eco.-Year FEs" ///
				"Spring, Winter, Fall, SGR" ///
				"Other summer weather, SGR" ///
						"$ N $" "$ R^2 $ ") ///
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
				) ///
			) ///
			keep($x $i  sgr_smr_*  ) ///
			order($x sgr_* $i  ) ///
			interaction( $ \times $) ///
			indicate( ///
			, label(\multicolumn{1}{c}{Y} \multicolumn{1}{c}{N})) ///
						refcat( ///
							sgr_aut_hed3 "\it  Fall weather, SGR" ///
							sgr_spr_hed3 "\it  Spring weather, SGR" ///
							sgr_wtr_hed3 "\it  Winter weather, SGR" ///
							sgr_smr_hed3 "\it  Summer weather, SGR" ///
							wgr_aut_ced3 "\it  Fall weather, WGR" ///
							wgr_spr_ced3 "\it  Spring weather, WGR" ///
							wgr_wtr_ced3 "\it  Winter weather, WGR" ///
							survey_sgr_smr_hed3 "$ \ln HS ^{June}_{st} \times $ \it  Summer weather" ///							
							, nolabel) 	

*------------------------------------------------------------------------------*
* FS 
*------------------------------------------------------------------------------*
	use  "$dta/wrk/wrk_soum_hs_gdd.dta", replace 
	xtset asid year 
	
	glo y survey 
	glo x census 
	
	loc varlist1 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} 
	loc varlist2 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}   
	loc varlist3 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}
	loc varlist4 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR}
	loc varlist5 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} ${SGRAUT}

	local T "FS"
	foreach i in  1 2 3 4 5 6  {
		if `i' == 1 {
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
		if `i' == 2 {
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
		if `i' == 3 {
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
		if `i' == 4 {
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
		if `i' == 5 {
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
		if `i' == 6 {
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
	
	
********************************************************************************
**# Table H.2: Second Stage: Results from equation (C.2) regressing log of soum-level SGR-
* based average NDVI on the log of predicted June herdsize, SGR-based summer weather
* variables in levels (temperature bins, precipitation, wind), the interaction of the predicted
* herdsize and summer weather variables, and other covariates.
********************************************************************************
*------------------------------------------------------------------------------*
* Second Step: Shapley 4 without a lag of log of NDVI 
*------------------------------------------------------------------------------*	
	
	cap drop hshat 
	g hshat = . 
	la var hshat "$ \ln \widehat{HS}^{June}_{st}$"

	cap drop hshat_hed3
	g hshat_hed3 = .
	la var hshat_hed3 "\quad $ \times \quad GDD(10^\circ\text{C},15^\circ\text{C}]$ "	
	
	cap drop hshat_hed4
	g hshat_hed4 = .
	la var hshat_hed4 "\quad $ \times \quad GDD(15^\circ\text{C},20^\circ\text{C}]$ "
		
	cap drop hshat_hed5
	g hshat_hed5 = .
	la var hshat_hed5 "\quad $ \times \quad GDD(>20^\circ\text{C})$ "
				
	cap drop hshat_prec
	g hshat_prec = .
	la var hshat_prec "\quad $ \times $ \quad Precipitation (m, acc.) "
			
	cap drop hshat_ws
	g hshat_ws = .
	la var hshat_ws "\quad $ \times $ \quad Wind speed (m/s, ave.) "

	glo y lndvi  
	glo x hshat 
	glo i hshat_hed? hshat_prec hshat_ws
	
	loc varlist1 $y $x  
	loc varlist2 $y $x $i ${SGRSMR}  
	loc varlist3 $y $x $i ${SGRSMR}
	loc varlist4 $y $x $i ${SGRSMR} ${SGRSPR}  
	loc varlist5 $y $x $i ${SGRSMR} ${SGRSPR} ${SGRWTR} 
	loc varlist6 $y $x $i ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}

	cap drop yr* 
		tab year, g(yr)
	loc YFE ""
	foreach y of numlist 1/55 {
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
	foreach e of numlist 1/275 {
		loc EYFE "`EYFE' ecoyr`e'"
		di "`EYFE'"
	}

	glo sumvar "hshat sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave hshat_hed3 hshat_hed4 hshat_hed5 hshat_prec hshat_ws"
	*bringing bootstrapping
	preserve 
		use "$dta/wrk/sd_hs_gdd.dta", clear 
			matrix S = J(6,11,.)
			matrix rownames S = se 
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
					
	local T "SLS"
	foreach i in  1 2 3 4 5 6 {
		replace hshat = herdsizehat`i'
		replace hshat_hed3  = herdsizehat`i' * sgr_smr_hed3	
		replace hshat_hed4  = herdsizehat`i' * sgr_smr_hed4
		replace hshat_hed5  = herdsizehat`i' * sgr_smr_hed5
		replace hshat_prec = herdsizehat`i' * sgr_smr_prec 
		replace hshat_ws = herdsizehat`i' * sgr_smr_ws_ave  
		if `i' == 1 {
				cap drop _mysample 
				eststo `T'_`i': reg `varlist`i''  , vce(cluster asid)

				estadd loc sFE "No" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'
				estadd loc SMRW "No" : `T'_`i'

					matrix P = J(1,11,.)
					matrix rownames P = pvalue
					matrix colnames P = $sumvar									
						loc j = 0 
						foreach v of varlist $sumvar	  {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
										
					if `i' == 1 {
					matrix SD = S[`i', 1..1 ]
					matrix PV = P[1, 1..1 ]
					
					}
					else {
					matrix SD = S[`i', 1..11]
					matrix PV = P[1, 1..11
					}
					
					matrix list SD 
					
					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'

					di "`SH1' `SH2' `SH3' `SH4'"
					ereturn list 
					
				estadd scalar SH1=100  : `T'_`i'
				estadd scalar SH2=0  : `T'_`i'
				estadd scalar SH3=0  : `T'_`i'
				estadd scalar SH4=0 : `T'_`i'
				estadd scalar FF=. : `T'_`i'
				estadd scalar PP=. : `T'_`i'

				
			}		
		if `i' == 2 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' , vce(cluster asid)

				estadd loc sFE "No" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'
				estadd loc SMRW "Yes" : `T'_`i'
					test hshat_hed3 hshat_hed4 hshat_hed5 hshat_prec hshat_ws  
						return list 
				estadd scalar FF = `r(F)' : `T'_`i'
				estadd scalar PP = `r(p)' : `T'_`i'
				
					matrix P = J(1,11,.)
					matrix rownames P = pvalue
					matrix colnames P = $sumvar									
						loc j = 0 
						foreach v of varlist $sumvar	  {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
										
					if `i' == 1 {
					matrix SD = S[`i', 1..6 ]
					matrix PV = P[1, 1..6 ]
					
					}
					else {
					matrix SD = S[`i', 1..11]
					matrix PV = P[1, 1..11 ]
					}
					
					matrix list SD 

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					
				shapley2, stat(r2) force ///
					group( ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave,  ///
						hshat_hed3 ///
						hshat_hed4 ///
						hshat_hed5 ///
						hshat_prec ///
						hshat_ws ///
						)
					
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 
					di "`SH1' `SH2' `SH3'"
					ereturn list 
				estadd scalar SH1=`e(SH1)'  : `T'_`i'
				estadd scalar SH2=`e(SH2)'  : `T'_`i'
				estadd scalar SH3=`e(SH3)'  : `T'_`i'
				estadd scalar SH4=0 : `T'_`i'
			}
		if `i' == 3 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `EYFE' , vce(cluster asid)
				
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc SWF "NNN" : `T'_`i'
				estadd loc SMRW "Yes" : `T'_`i'
					test hshat_hed3 hshat_hed4 hshat_hed5 hshat_prec hshat_ws  
						return list 
				estadd scalar FF = `r(F)' : `T'_`i'
				estadd scalar PP = `r(p)' : `T'_`i'
				
					matrix P = J(1,11,.)
					matrix rownames P = pvalue
					matrix colnames P = $sumvar									
						loc j = 0 
						foreach v of varlist $sumvar	  {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
										
					if `i' == 1 {
					matrix SD = S[`i', 1..6 ]
					matrix PV = P[1, 1..6 ]
					
					}
					else {
					matrix SD = S[`i', 1..11]
					matrix PV = P[1, 1..11 ]
					}
					
					matrix list SD 

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
						hshat , ///
						sgr_smr_hed5  ///
						sgr_smr_hed3  ///
						sgr_smr_hed4  ///
						sgr_smr_prec  ///
						sgr_smr_ws_ave,  ///
						hshat_hed3 ///
						hshat_hed4 ///
						hshat_hed5 ///
						hshat_prec ///
						hshat_ws ,  ///
						`SFE'  ///
						`EYFE' ///						
						)
					
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 
				estadd scalar SH4 = e(shapley_rel)[4,1]*100 
					di "`SH1' `SH2' `SH3' `SH4'"
					ereturn list 
				estadd scalar SH1=`e(SH1)'  : `T'_`i'
				estadd scalar SH2=`e(SH2)'  : `T'_`i'
				estadd scalar SH3=`e(SH3)'  : `T'_`i'
				estadd scalar SH4=`e(SH4) ' : `T'_`i'
			}
		if `i' == 4 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `EYFE' , vce(cluster asid)
				
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc SWF "YNN" : `T'_`i'
				estadd loc SMRW "Yes" : `T'_`i'
					test hshat_hed3 hshat_hed4 hshat_hed5 hshat_prec hshat_ws  
						return list 
				estadd scalar FF = `r(F)' : `T'_`i'
				estadd scalar PP = `r(p)' : `T'_`i'
				
					matrix P = J(1,11,.)
					matrix rownames P = pvalue
					matrix colnames P = $sumvar									
						loc j = 0 
						foreach v of varlist $sumvar	  {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
										
					if `i' == 1 {
					matrix SD = S[`i', 1..6 ]
					matrix PV = P[1, 1..6 ]
					
					}
					else {
					matrix SD = S[`i', 1..11]
					matrix PV = P[1, 1..11 ]
					}
					
					matrix list SD 

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					
				shapley2, stat(r2) force ///
					group( ///
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
							sgr_spr_ws_ave , ///						
						hshat_hed3 ///
						hshat_hed4 ///
						hshat_hed5 ///
						hshat_prec ///
						hshat_ws ,  ///
						`SFE'  ///
						`EYFE' ///						
						)
					
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 
				estadd scalar SH4 = e(shapley_rel)[4,1]*100 
					di "`SH1' `SH2' `SH3' `SH4' `SH5' `SH6' `SH7'  `SH8' `SH9' `SH10' "
					ereturn list 
				estadd scalar SH1=`e(SH1)'  : `T'_`i'
				estadd scalar SH2=`e(SH2)'  : `T'_`i'
				estadd scalar SH3=`e(SH3)'  : `T'_`i'
				estadd scalar SH4=`e(SH4) ' : `T'_`i'
			}
		if `i' == 5 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `EYFE' , vce(cluster asid)
				
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc SWF "YYN" : `T'_`i'
				estadd loc SMRW "Yes" : `T'_`i'
					test hshat_hed3 hshat_hed4 hshat_hed5 hshat_prec hshat_ws  
						return list 
				estadd scalar FF = `r(F)' : `T'_`i'
				estadd scalar PP = `r(p)' : `T'_`i'
					matrix P = J(1,11,.)
					matrix rownames P = pvalue
					matrix colnames P = $sumvar									
						loc j = 0 
						foreach v of varlist $sumvar	  {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
										
					if `i' == 1 {
					matrix SD = S[`i', 1..6 ]
					matrix PV = P[1, 1..6 ]
					
					}
					else {
					matrix SD = S[`i', 1..11]
					matrix PV = P[1, 1..11 ]
					}
					
					matrix list SD 

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'

				shapley2, stat(r2) force ///
					group( ///
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
							sgr_spr_ws_ave  ///			
								sgr_wtr_hed3  ///
								sgr_wtr_hed4  ///
								sgr_wtr_hed5  ///
								sgr_wtr_prec  ///
								sgr_wtr_ws_ave , ///					
						hshat_hed3 ///
						hshat_hed4 ///
						hshat_hed5 ///
						hshat_prec ///
						hshat_ws ,  ///
						`SFE'  ///
						`EYFE' ///						
						)
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 
				estadd scalar SH4 = e(shapley_rel)[4,1]*100 
					di "`SH1' `SH2' `SH3' `SH4'"
					ereturn list 
				estadd scalar SH1=`e(SH1)'  : `T'_`i'
				estadd scalar SH2=`e(SH2)'  : `T'_`i'
				estadd scalar SH3=`e(SH3)'  : `T'_`i'
				estadd scalar SH4=`e(SH4) ' : `T'_`i'
			}
		if `i' == 6 {
				cap drop _mysample 
				eststo `T'_`i' : reg `varlist`i'' `SFE' `EYFE' , vce(cluster asid)
				
				estadd loc sFE "Yes" : `T'_`i'
				estadd loc SWF "YYY" : `T'_`i'
				estadd loc SMRW "Yes" : `T'_`i'
					test hshat_hed3 hshat_hed4 hshat_hed5 hshat_prec hshat_ws  
						return list 
				estadd scalar FF = `r(F)' : `T'_`i'
				estadd scalar PP = `r(p)' : `T'_`i'
					matrix P = J(1,11,.)
					matrix rownames P = pvalue
					matrix colnames P = $sumvar									
						loc j = 0 
						foreach v of varlist $sumvar	  {
							loc `j++'
							cap matrix P[1,`j'] = (2 * ttail(e(df_r), abs( _b[`v']/S[`i',`j'] ) ) ) 
						} 				
					matrix list P
										
					if `i' == 1 {
					matrix SD = S[`i', 1..6 ]
					matrix PV = P[1, 1..6 ]
					
					}
					else {
					matrix SD = S[`i', 1..11]
					matrix PV = P[1, 1..11 ]
					}
					
					matrix list SD 

					estadd matrix SD: `T'_`i'
					estadd matrix PV: `T'_`i'
					

				shapley2, stat(r2) force ///
					group( ///
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
									sgr_aut_ws_ave , ///					
						hshat_hed3 ///
						hshat_hed4 ///
						hshat_hed5 ///
						hshat_prec ///
						hshat_ws ,  ///
						`SFE'  ///
						`EYFE' ///						
						)
				estadd scalar SH1 = e(shapley_rel)[1,1]*100 
				estadd scalar SH2 = e(shapley_rel)[2,1]*100 
				estadd scalar SH3 = e(shapley_rel)[3,1]*100 
				estadd scalar SH4 = e(shapley_rel)[4,1]*100 
					di "`SH1' `SH2' `SH3' `SH4'"
					ereturn list 
				estadd scalar SH1=`e(SH1)'  : `T'_`i'
				estadd scalar SH2=`e(SH2)'  : `T'_`i'
				estadd scalar SH3=`e(SH3)'  : `T'_`i'
				estadd scalar SH4=`e(SH4) ' : `T'_`i'
			}
	}

	local tex "sls_ndvi_shapley4_wLag_gdd_bse"
	 esttab SLS_* using "$olf/tab/supp/`tex'.tex", replace substitute(\_ _) ///
			cells( b(fmt(3) star) SD( par fmt(3) pvalue(PV)) )  /// //se(par fmt(3))
			star(* 0.10 ** 0.05 *** 0.01) ///
			label compress nogaps nodepvars collabels(none) nomtitles ///
			booktabs alignment(D{.}{.}{-1}) fragment ///
			mgroups("(a) Dependent variable: $ \ln B^{summer}_{st, SGR} $" , pattern(1 0 0 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
			stats(sFE SMRW SWF N r2 HH FF PP, ///
				fmt( 0 0 0 0 2 0 2 2) ///
			labels( ///
				"Soum, Eco.-Year FEs"  ///
				"Other summer weather, SGR"  ///
				"Spring, Winter, Fall, SGR"  ///			
			"$ N $" "$ R^2 $ "  ///
			"\\ \it Summer weather $ \times $ Herdsize: \\ \midrule \quad " ///
				"\qquad F-test" ///
				"\qquad P-value" ///
			) ///
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}"  "\multicolumn{1}{c}{@}" ///
				) ///
			) ///
			keep(hshat sgr_smr_* hshat_*   ) ///
			order(hshat sgr_smr_* hshat_*) ///
			interaction( $ \times $) ///
			indicate( ///
			, label(\multicolumn{1}{c}{Y} \multicolumn{1}{c}{N})) ///
			refcat( ///
				sgr_smr_hed3 "\it Summer weather, SGR" ///
				sgr_aut_hed3 "\it Fall weather, SGR" ///
				sgr_spr_hed3 "\it  Spring weather, SGR" ///
				sgr_wtr_hed3 "\it  Winter weather, SGR" ///
				hshat_hed3 "$ \ln \widehat{HS}^{June}_{st} \times $ \it  Summer weather" ///											
				, nolabel) 	
			
			

