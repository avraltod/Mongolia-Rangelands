// =============================================================================
// Filename:  do08_fig_20241210.do
// Author:     Avralt-Od Purevjav
// Last Modified:  Avraa
// Date:     2024-12-10
// =============================================================================
// Figures 
// ========================================================================== //
	* 
	clear all
	set more off
	set excelxlsxlargefile on
	set processors 8
	set maxvar 50000 
	
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
	glo gph "$wd/gph"

	// confirming if the user-defined commands are installed 
	qui foreach prog in unique shp2dta spmap mergepoly geoinpoly geo2xy geonear ///
		bimap joyplot circlebar treemap circlepack {
		cap which `prog'
		if _rc ssc install `prog' , replace all
	}

	graph set window fontface "ArialNarrow"
	graph set window fontfacesans "ArialNarrow"
	graph set window fontfaceserif "ArialNarrow"
	graph set window fontfacemono "ArialNarrow"

// 	graph set window fontface "LM Roman 10"
// 	graph set window fontfacesans "LM Roman 10"
// 	graph set window fontfaceserif "LM Roman 10"
// 	graph set window fontfacemono "LM Roman 10"


********************************************************************************
**# Figure 1: Livestock Population Dynamics in Mongolia
********************************************************************************
* LIVESTOCK at country level 
********************************************************************************	
	{
	use "$dta/wrk/wrk_soum.dta" , clear 
		keep year asid cen_* 
			ren cen_* * 
				drop if year == 2024
				
		* at national level 		
		collapse (sum) sheep goat cattle horse camel  , by(year)
		 
	glo SIZE "ysize(6) xsize(9)"
	glo GREG "graphregion(style(none) m(0) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=0 b=0 l=2 r=2) fc(white) lc(black) lp(solid) ls(p1) lw(thin))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(black)"
	glo YT2 "$YT1"
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) "
	glo YL2 "$YL1 format(%9.1f)"
	glo YS1 "titlegap(0.2cm) outergap(0cm) lc(none)"
	glo YS2 "$YS1"
	glo XT1 "size(*.75)"
	glo XT2 "$XT1 color(white)"
	glo XL1 "labs(*.5) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0.2cm) outergap(0cm) lc(none)"
	glo XS2 "$XS1"
	glo LEG "col(7) ring(1) si(*.5) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	
	glo O1 lc(red) lp(solid) mfc(red) mc(red) ms(Oh) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(1) ///
			text(15.5 1995 "Sheep", place(se) color(red) size(vsmall))
	glo O2 lc(blue) lp(solid) mfc(blue) mc(blue) ms(Dh) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(1) ///
			text(11 1995 "Goats", place(ne) color(blue) size(vsmall))
	glo O3 lc(green) lp(solid) mfc(green) mc(green) ms(T) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(2) ///
			text(23 1995 "Cattle", place(se) color(green) size(vsmall))
	glo O4 lc(orange) lp(solid) mfc(orange) mc(orange) ms(S) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(2) ///
			text(18 1995 "Horses", place(ne) color(orange) size(vsmall))
	glo O5 lc(cyan) lp(solid) mfc(cyan) mc(cyan) ms(Sh) msi(vsmall) lw(thin) ///
		mlw(vthin) yaxis(2) ///
			text(3 1995 "Camels", place(ne) color(cyan) size(vsmall))


	//costumize 	
	loc ttl ""
	loc yti1 "Millions of Livestocks (Sheep and Goats)"	
	loc yti2 "Millions of Livestocks (Cattle, Horses, and Camels)"	
	loc xti "Year"
	loc ord `"7 "Sheep" 8 "Goats" 9 "Cattle" 10 "Horses" 11 "Camels" 1 "Privatization period" 2 "Zhud years" "'
	loc xla "1971(2)2023"
	loc yla1 "0(6)36"
	loc yla2 "0(1)6"			
	
	g upper = 35.99
	
	twoway ///
		(area upper y if inrange(y, 1991, 1993), lw(*0) bcolor(gs12%75) base(0.1)) ///
		(area upper y if inrange(y, 1999, 2002), lw(*0) bcolor(blue%30) base(0.1)) ///
		(area upper y if inrange(y, 2009, 2010), lw(*0) bcolor(blue%30) base(0.1)) ///
		(area upper y if inrange(y, 2017, 2018), lw(*0) bcolor(blue%30) base(0.1)) ///
		(area upper y if inrange(y, 2019, 2020), lw(*0) bcolor(blue%30) base(0.1)) ///	
		(area upper y if inrange(y, 2022, 2023), lw(*0) bcolor(blue%30) base(0.1)) ///	
		(connect sheep year , $O1 ) ///
		(connect goat year , $O2 ) ///
		(connect cattle year , $O3 ) ///
		(connect horse year, $O4 ) ///
		(connect camel year , $O5 ) ///
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 format(%9.0f)) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti(`xti' , $XT1 ) xla(`xla' , $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(12) $LEG) ///
			saving("$fig/fg_hs.gph", replace )
	graph export "$fig/fg01.png", replace height(1600)
	graph export "$olf/fig/main/fg01.png", replace height(1600)
	}
	
	
********************************************************************************
**# Figure 2: Weather Variable Trends in Mongolia
********************************************************************************
* Weather: TEMPERATURE, PRECIPITATION, WIND 
********************************************************************************	
	{
	use "$dta/wrk/wrk_soum.dta" , clear 
		keep year asid sgr*prec sgr*tmp_ave sgr*ws_ave 
			ren sgr_aut_* *_1
			ren sgr_wtr_* *_2
			ren sgr_spr_* *_3
			ren sgr_smr_* *_4
	
	reshape long tmp_ave_ prec_ ws_ave_ , i(year asid) j(ssn)
			tab ssn 
			la var ssn "Season"
			la def SSN , replace 
				la val ssn SSN   
		ren *_ave_ * 
		ren *_ * 
		
	loc v1 "tmp"
	loc v2 "prec"
	loc v3 "ws"

		drop if year < 1980	
		collapse ///
				(mean) `v1'_ave =`v1' ///
				(sem) `v1'_se=`v1' ///
				(mean) `v2'_ave =`v2' ///
				(sem) `v2'_se=`v2' ///
				(mean) `v3'_ave =`v3' ///
				(sem) `v3'_se=`v3' ///
				 , by(year)
	
	foreach v in tmp prec ws { 
		g `v'_ucl = `v'_ave + 1.96*`v'_se
		g `v'_lcl = `v'_ave - 1.96*`v'_se
	}
		
	glo SIZE "ysize(6) xsize(9)"
	glo GREG "graphregion(style(none) m(zero) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=2 b=0 l=2 r=2) fc(white) lc(white) lp(solid) ls(p1) lw(*0))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(black)"
	glo YT2 "size(*.75) color(white)"
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.1f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.1f)"
	glo YS1 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo YS2 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo XT1 "size(*.75) color(white)"
	glo XT2 "color(white)"
	glo XL1 "labs(*.5) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo XS2 "$XS1"
	glo LEG "col(1) ring(0) si(*.5) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	glo O1 lc(navy) lp(solid) lw(thin) mlc(navy) mlw(vthin) mfc(white)  ///
		  ms(O) msi(small) yaxis(1)	
	glo O2 lc(navy) lp(solid) lw(thin) yaxis(1)		
	glo O3 lc(navy) lp(dash) lw(*1) degree(4) yaxis(2)	

	//costumize 	
	loc ttl "" //"(a) Temperature"
	loc yti1 "(a) Average temperature ({sup:o}C)"	
	loc yti2 "Average temperature ({sup:o}C)"	
	loc xti ""
	loc ord `"2 "Average temperature ({sup:o}C)" 1 "95% CI" 3 "A polynomial trend line of degree 4" "'
	loc xla "1981(3)2024"
	loc yla1 "-2.5(1.5)3.5"
	loc yla2 "-2.5(1.5)3.5"	
	g upper = 3.5 
	
	twoway ///
		(rcap tmp_ucl tmp_lcl year, $O2) ///
		(connect tmp_ave year, $O1) ///	
		(lpoly tmp_ave year, $O3 ) ///		
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti("`xti'" , $XT1 ) xla(`xla', $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(11) $LEG) ///
				saving("$fig/fg_temp.gph", replace )

	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.0f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.0f)"

	glo O1 lc(navy) lp(solid) lw(thin) mlc(navy) mlw(vthin) mfc(white)  ///
		  ms(D) msi(small) yaxis(1)	
	glo O2 lc(navy) lp(solid) lw(thin) yaxis(1)		
	glo O3 lc(navy) lp(dash) lw(*1) degree(4) yaxis(2)	
		
	//costumize 	
	loc ttl "" //"(b) Percipitation"
	loc yti1 "(b) Accumulated precipitation (mm)"	
	loc yti2 "Accumulated precipitation (mm)"	
	loc xti ""
	loc ord `"2 "Accumulated precipitation (mm)" 1 "95% CI" 3 "A polynomial trend line of degree 4" "'
	loc xla "1981(3)2024"
	loc yla1 "50(10)110"
	loc yla2 "50(10)110"	
	
	replace upper = 110 
	
	twoway ///	
		(rcap prec_ucl prec_lcl year, $O2) ///
		(connect prec_ave year, $O1) ///	
		(lpoly prec_ave year, $O3 ) ///		
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti("`xti'", $XT1 ) xla(`xla' , $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(7) $LEG) ///
				saving("$fig/fg_perc.gph", replace )
				
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.1f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.1f)"
		
	glo O1 lc(navy) lp(solid) lw(thin) mlc(navy) mlw(vthin) mfc(white)  ///
		  ms(T) msi(small) yaxis(1)	
	glo O2 lc(navy) lp(solid) lw(thin) yaxis(1)		
	glo O3 lc(navy) lp(dash) lw(*1) degree(4) yaxis(2)	
	
	//costumize 	
	loc ttl "" //"(c) Wind speed"
	loc yti1 "(c) Average wind speed (m/s)"	
	loc yti2 "Average wind speed (m/s)"	
	loc xti ""
	loc ord `"2 "Average wind speed (m/s)" 1 "95% CI" 3 "A polynomial trend line of degree 4" "'
	loc xla "1981(3)2024"
	loc yla1 "2.6(.1)3"
	loc yla2 "2.6(.1)3"	
	
	twoway ///	
		(rcap ws_ucl ws_lcl year, $O2) ///
		(connect ws_ave year, $O1) ///	
		(lpoly ws_ave year, $O3 ) ///		
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti("`xti'" , $XT1 ) xla(`xla' , $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(11) $LEG) ///
				saving("$fig/fg_wind.gph", replace )
				
	graph combine ///
		$fig/fg_temp.gph ///
		$fig/fg_perc.gph ///
		$fig/fg_wind.gph ///
		,  iscale(*1) col(1) ///
			imargin(*1) ///
			plotregion(m(t=2 b=2 l=0 r=0) fc(white)) $GREG 
		
	graph export "$fig/fg02.png", replace height(1600)
	graph export "$olf/fig/main/fg02.png", replace height(1600)
	
	}
	


********************************************************************************
**# Figure 3: Comparison of herd size and GDD(> 20C) effect size estimates
********************************************************************************
* Coefpot 
********************************************************************************
	use "$dta/wrk/wrk_soum.dta", clear 
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
			
		*ecozones 
		tab eco, g(ez)
			la var ez1 "Mountain taiga"
			la var ez2 "Forest steppe"
			la var ez3 "Steppe"
			la var ez4 "Semi desert"
			la var ez5 "Desert"
			
		loc ZN `" "Mountain taiga" "Forest steppe" "Steppe" "Semi desert" "Desert" "'	

			foreach i of numlist 1/5 {
				loc z: word `i' of `ZN' 
				
				*GDD(20+)
					cap drop ez`i'_sgr_smr_hed5
						g sgr_smr_hed5_ez`i' = ez`i' * sgr_smr_hed5
							la var sgr_smr_hed5_ez`i' "\quad $ \times $ `z'"
				*HS Survey	
					cap drop survey_ez`i'
						g survey_ez`i' = ez`i' * survey
							la var survey_ez`i' "\quad $ \times $ `z'"
				*HS Census 	
					cap drop census_ez`i'
						g census_ez`i' = ez`i' * census
							la var census_ez`i' "\quad $ \times $ `z'"
			}

		*SGR WTHR 
		glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
		glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
		glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
		glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5_ez? sgr_smr_prec sgr_smr_ws_ave"
		
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

	
	tempfile OLS 
	save `OLS', replace 
*------------------------------------------------------------------------------*
	*first stage without ecozone interactions
*------------------------------------------------------------------------------*
	use `OLS' , clear 

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
	

	loc varlist $y $x lndvilag ///
		${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ///
			${SGRSPR} ${SGRWTR} ${SGRAUT}

	reghdfe `varlist' ///
		, absorb(i.year i.asid i.eco#i.year) ///
			vce(cluster asid) nocons 
					
		cap drop herdsizehat
			predict herdsizehat, xb 

	tempfile FS 
	save `FS'
*------------------------------------------------------------------------------*
	* Long differencing without ecozone interactions 
	foreach a of numlist 1987/2012 {
		
		use `FS', clear   

		*SGR WTHR 
		glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
		glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
		glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
		glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5 sgr_smr_prec sgr_smr_ws_ave"

		loc d 10 
		loc b = `a'+ `d'
			di "`a' , `b', `d'"
			
		cap drop period 
		g period = . 
		replace period = 1 if year >= `a'- 2 & year <= `a' + 2
		replace period = 2 if year >= `b'- 2 & year <= `b' + 2
		
		glo VARS lndvi $SGRAUT $SGRWTR $SGRSPR $SGRSMR herdsizehat
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
	
	g hshat = . 
	replace hshat = herdsizehat

	tempfile LD 
	save `LD', replace 
	
*------------------------------------------------------------------------------*
	*first stage with ecozone interactions 
*------------------------------------------------------------------------------*
	use `OLS', clear 
	
	glo y survey 
	glo x census_ez? 
	
	glo WGRSMR "lag_wgr_smr_prec lag_wgr_smr_hed5"
	glo WGRAUT "wgr_aut_ced3"
	glo WGRWTR "wgr_wtr_sd wgr_wtr_ced3 wgr_wtr_wed3"
	glo WGRSPR "wgr_spr_ced3"
	
	glo SGRAUT "sgr_aut_hed* sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed* sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed* sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5_ez1 sgr_smr_hed5_ez2 sgr_smr_hed5_ez3 sgr_smr_hed5_ez4 sgr_smr_hed5_ez5 sgr_smr_prec sgr_smr_ws_ave"  
	
	loc varlist $y $x lndvilag ///
		${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ///
			${SGRSPR} ${SGRWTR} ${SGRAUT}

	reghdfe `varlist' ///
		, absorb(i.year i.asid i.eco#i.year) ///
			vce(cluster asid) nocons 
			
		cap drop herdsizehat
			predict herdsizehat, xb 
			
	tempfile FSECO 
	save `FSECO', replace 
	
*------------------------------------------------------------------------------*
	* Long differencing with ecozone interactions 
	foreach a of numlist 1987/2012 {
		
		use `FSECO', clear   

		*SGR WTHR 
		glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
		glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
		glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
		glo SGRSMR "sgr_smr_hed5_ez1 sgr_smr_hed5_ez2 sgr_smr_hed5_ez3 sgr_smr_hed5_ez4 sgr_smr_hed5_ez5 sgr_smr_hed3 sgr_smr_hed4 sgr_smr_prec sgr_smr_ws_ave"  

		loc d 10 
		loc b = `a'+ `d'
			di "`a' , `b', `d'"
			
		cap drop period 
		g period = . 
		replace period = 1 if year >= `a'- 2 & year <= `a' + 2
		replace period = 2 if year >= `b'- 2 & year <= `b' + 2
		
		glo VARS lndvi $SGRAUT $SGRWTR $SGRSPR $SGRSMR herdsizehat
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

	*HS predicted  	
	g hshat = . 
	replace hshat = herdsizehat
	tab eco, g(ez)
	foreach j of numlist 1/5 {
		loc z: word `j' of `ZN' 
		cap drop hshat_ez`j'
			g hshat_ez`j' = ez`j' * hshat
	}
		
	tempfile LDECO 
	save `LDECO', replace 
*------------------------------------------------------------------------------*

// 	use `OLS', clear 
// 		g longdiff = 0 
// 		append using `LD'
// 			replace longdiff = 1 if longdiff == . 
// 		append using `LDECO'
// 			replace longdiff = 2 if longdiff == . 			
// 	tempfile COEF
// 	save `COEF', replace 
// *------------------------------------------------------------------------------*
	
	
*------------------------------------------------------------------------------*
	loc CL `" "Overall" " x Mountain taiga" " x Forest steppe" " x Steppe" " x Semi desert" " x Desert"  "'
	
	matrix HS = J(6,18,.)
	matrix colnames HS = hshat ll95 ul95 
	matrix rown HS = `CL'
	matrix EH = J(6,18,.)
	matrix colnames EH = sgr_smr_hed5 ll95 ul95 
	matrix rown EH = `CL'

*------------------------------------------------------------------------------*
	use `OLS' , clear 
	
	glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed5 sgr_smr_hed3 sgr_smr_hed4  sgr_smr_prec sgr_smr_ws_ave"

	glo y lndvi 
	glo x survey 
*------------------------------------------------------------------------------*
	* naive OLS: Survey without FEs 
	eststo OLS1: reghdfe lndvi survey ${SGRSMR} ///
			lndvilag , vce( cluster asid) residuals
			matrix list r(table)
		matrix HS[1, 1] = r(table)[1,1], r(table)[5,1], r(table)[6,1]
		matrix EH[1, 1] = r(table)[1,2], r(table)[5,2], r(table)[6,2]
			matrix list HS 
			matrix list EH 
*------------------------------------------------------------------------------*
	* naive OLS: Survey with FEs 
	eststo OLS2: reghdfe lndvi survey  ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT} ///
		 lndvilag , absorb(i.year i.asid i.eco#i.year) ///
			vce( cluster asid) residuals
		matrix list r(table)
		matrix HS[1, 4] = r(table)[1,1], r(table)[5,1], r(table)[6,1]
		matrix EH[1, 4] = r(table)[1,2], r(table)[5,2], r(table)[6,2]
			matrix list HS 
			matrix list EH 
*------------------------------------------------------------------------------*
	* naive OLS: Census without FEs 
	eststo OLS3: reghdfe lndvi census  ${SGRSMR}  ///
			lndvilag , vce( cluster asid) residuals
		matrix list r(table)
		matrix HS[1, 7] = r(table)[1,1], r(table)[5,1], r(table)[6,1]
		matrix EH[1, 7] = r(table)[1,2], r(table)[5,2], r(table)[6,2]
			matrix list HS 
			matrix list EH 
*------------------------------------------------------------------------------*
	* naive OLS: Census with FEs 
	eststo OLS4: reghdfe lndvi census ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT} ///
		lndvilag , absorb(i.year i.asid i.eco#i.year) ///
			vce( cluster asid) residuals
		matrix list r(table)
		matrix HS[1, 10] = r(table)[1,1], r(table)[5,1], r(table)[6,1]
		matrix EH[1, 10] = r(table)[1,2], r(table)[5,2], r(table)[6,2]
			matrix list HS 
			matrix list EH 
*------------------------------------------------------------------------------*
	* Short IV 
		** first stage 
		reghdfe survey census lndvilag ///
			${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}  ///
				${SGRSPR} ${SGRWTR} ${SGRAUT}   ///
					, absorb(i.year i.asid i.eco#i.year) ///
						vce(cluster asid) 
		cap drop herdsizehat
			predict herdsizehat, xb 
			
		** second stage 
		cap drop hshat 
		g hshat = . 
		replace hshat = herdsizehat
		
	eststo IV1: reghdfe lndvi hshat ${SGRSMR}  ///
		${SGRSPR} ${SGRWTR} ${SGRAUT} ///
			, absorb(i.year i.asid i.eco#i.year) ///
				vce( cluster asid)
		matrix list r(table)
		matrix HS[1, 13] = r(table)[1,1], r(table)[5,1], r(table)[6,1]
		matrix EH[1, 13] = r(table)[1,2], r(table)[5,2], r(table)[6,2]
			matrix list HS 
			matrix list EH 
*------------------------------------------------------------------------------*
	use `LD', clear 
	* Long IV 
		** second stage 
		cap drop hshat 
		g hshat = . 
		replace hshat = herdsizehat 
		
	eststo IV2 : reghdfe lndvi hshat ///
		${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}  ///
			 , absorb(i.year i.asid) ///
				vce(cluster asid)
		matrix list r(table)
		matrix HS[1, 16] = r(table)[1,1], r(table)[5,1], r(table)[6,1]
		matrix EH[1, 16] = r(table)[1,2], r(table)[5,2], r(table)[6,2]
			matrix list HS 
			matrix list EH 

*------------------------------------------------------------------------------*	*interactions 	
	use `OLS' , clear 
	
	*SGR WTHR 
	glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed5_ez1 sgr_smr_hed5_ez2 sgr_smr_hed5_ez3 sgr_smr_hed5_ez4 sgr_smr_hed5_ez5 sgr_smr_hed3 sgr_smr_hed4 sgr_smr_prec sgr_smr_ws_ave"  
*------------------------------------------------------------------------------*
	* naive OLS: Survey without FEs 
	eststo ECO1: reghdfe lndvi survey_ez? ${SGRSMR} ///
			lndvilag , vce( cluster asid) residuals
		matrix list r(table)
		foreach i of numlist 1/5 {
			loc j = `i' + 1
			matrix HS[`j', 1] = r(table)[1,`i'], r(table)[5,`i'], r(table)[6,`i']
			loc k = 5 + `i'
			matrix EH[`j', 1] = r(table)[1,`k'], r(table)[5,`k'], r(table)[6,`k']
		}		 
		matrix list HS 
		matrix list EH 
*------------------------------------------------------------------------------*
	* naive OLS: Survey with FEs 
	eststo ECO2: reghdfe lndvi survey_ez?  ${SGRSMR} ///
		${SGRSPR} ${SGRWTR} ${SGRAUT} ///
		lndvilag , absorb(i.year i.asid i.eco#i.year) ///
			vce( cluster asid) residuals	
		matrix list r(table)
		foreach i of numlist 1/5 {
			loc j = `i' + 1
			matrix HS[`j', 4] = r(table)[1,`i'], r(table)[5,`i'], r(table)[6,`i']
			loc k = 5 + `i'
			matrix EH[`j', 4] = r(table)[1,`k'], r(table)[5,`k'], r(table)[6,`k']
		}		 
		matrix list HS 
		matrix list EH 
*------------------------------------------------------------------------------*
	* naive OLS: Census without FEs 
	eststo ECO3: reghdfe lndvi census_ez?  ${SGRSMR}  ///
			lndvilag , vce( cluster asid) residuals
		matrix list r(table)
		foreach i of numlist 1/5 {
			loc j = `i' + 1
			matrix HS[`j', 7] = r(table)[1,`i'], r(table)[5,`i'], r(table)[6,`i']
			loc k = 5 + `i'
			matrix EH[`j', 7] = r(table)[1,`k'], r(table)[5,`k'], r(table)[6,`k']
		}		 
		matrix list HS 
		matrix list EH 
*------------------------------------------------------------------------------*
	* naive OLS: Census with FEs 
	eststo ECO4: reghdfe lndvi census_ez? ${SGRSMR}   ///
			${SGRSPR} ${SGRWTR} ${SGRAUT} ///
		lndvilag , absorb(i.year i.asid i.eco#i.year) ///
			vce( cluster asid) residuals	
		matrix list r(table)
		foreach i of numlist 1/5 {
			loc j = `i' + 1
			matrix HS[`j', 10] = r(table)[1,`i'], r(table)[5,`i'], r(table)[6,`i']
			loc k = 5 + `i'
			matrix EH[`j', 10] = r(table)[1,`k'], r(table)[5,`k'], r(table)[6,`k']
		}		 
		matrix list HS 
		matrix list EH 
*------------------------------------------------------------------------------*
	* Short IV 
		** first stage 
		reghdfe survey census_ez? lndvilag ///
			${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}  ///
				${SGRSPR} ${SGRWTR} ${SGRAUT}    ///
					, absorb(i.year i.asid ) ///
						vce(cluster asid) nocons
		cap drop herdsizehat
			predict herdsizehat, xb 
			
		** second stage 
		cap drop hshat 
		g hshat = . 
		replace hshat = herdsizehat
		*HS predicted  	
			foreach j of numlist 1/5 {
				loc z: word `j' of `ZN' 
				cap drop hshat_ez`j'
				g hshat_ez`j' = ez`j' * hshat
			}

	eststo ECO5: reghdfe lndvi hshat_ez? ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT} ///
			 , absorb(i.year i.asid ) ///
				vce( cluster asid) 
		matrix list r(table)
		foreach i of numlist 1/5 {
			loc j = `i' + 1
			matrix HS[`j', 13] = r(table)[1,`i'], r(table)[5,`i'], r(table)[6,`i']
			loc k = 5 + `i'
			matrix EH[`j', 13] = r(table)[1,`k'], r(table)[5,`k'], r(table)[6,`k']
		}		 
		matrix list HS 
		matrix list EH 
*------------------------------------------------------------------------------*
	use `LDECO', clear 
	* Long IV 
		** second stage 
		cap drop hshat 
		g hshat = . 
		replace hshat = herdsizehat

		*HS predicted  	
			foreach j of numlist 1/5 {				
				cap drop hshat_ez`j'
				g hshat_ez`j' = ez`j' * hshat 
			}
		
	eststo ECO6: reghdfe lndvi hshat_ez? ///
		${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT} ///
			, absorb(i.year i.asid ) ///
				vce(cluster asid) residuals 		
		matrix list r(table)
		foreach i of numlist 1/5 {
			loc j = `i' + 1
			matrix HS[`j', 16] = r(table)[1,`i'], r(table)[5,`i'], r(table)[6,`i']
			loc k = 5 + `i'
			matrix EH[`j', 16] = r(table)[1,`k'], r(table)[5,`k'], r(table)[6,`k']
		}		 
		matrix list HS 
		matrix list EH 
		
	glo SIZE "ysize(6) xsize(9)"
	glo GREG "graphregion(style(none) m(t=-2 b=20 l=20 r=0) fc(white) lc(none) )" 	
	glo PREG "plotregion(m(t=0 b=0 l=0 r=0) fc(white) lc(black) lp(solid) ls(p1) lw(thin))"
	glo TTL "size(*0) color(black)"
	glo YT1 "size(*1) color(black)"
	glo YL1 "ang(v) labs(*1) labgap(*0.1) nogrid tlc(white) labc(black) "
	glo YS1 "titlegap(0.2cm) outergap(0cm) lc(none)"
	glo XT1 "size(*.75)"
	glo XL1 "labs(*1) labgap(*0) format(%4.2f) nogrid tlc(black)"
	glo XS1 "titlegap(0.2cm) outergap(0cm) lc(none)"
	glo LEG "col(7) ring(1) si(*.5) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	coefplot ///
			(matrix(HS[,1]), ci((2 3)) pstyle(p5) msymbol(T)  label (Survey: OLS without FEs) ) ///
			(matrix(HS[,4]), ci((5 6)) pstyle(p3) msymbol(T)  label(Survey: OLS with FEs) ) ///
			(matrix(HS[,7]), ci((8 9)) pstyle(p5) msymbol(O)  label(Census: OLS without FEs) ) ///
			(matrix(HS[,10]), ci((11 12)) pstyle(p3) msymbol(O)  label(Census: OLS with FEs) ) ///
			(matrix(HS[,13]), ci((14 15)) pstyle(p10) msymbol(S)  label(Short-run IV (year-on-year)) ) ///
			(matrix(HS[,16]), ci((17 18)) pstyle(p1) msymbol(D)  label(Long-run IV (20-year diff.)) ) ///
			, xscale( noextend ) legend(colfirst cols(2) pos(6) order( 2 4 6 8)) ///
			xline(0, lc(red)) name(a, replace) title("(a) Herd size")  xla(-0.2(.05).4 ) ///
			graphregion(style(none) m(t=0 b=0 l=-8 r=0) fc(white) lc(none) )
			
	coefplot ///
			(matrix(EH[,1]), ci((2 3)) pstyle(p5) msymbol(T)  label (Survey: OLS without FEs) xscale(r(-.05 .02)) ) ///
			(matrix(EH[,4]), ci((5 6)) pstyle(p3) msymbol(T)  label(Survey: OLS with FEs) xscale(r(-.05 .02)) ) ///
			(matrix(EH[,7]), ci((8 9)) pstyle(p5) msymbol(O)  label(Census: OLS without FEs) xscale(r(-.05 .02)) ) ///
			(matrix(EH[,10]), ci((11 12)) pstyle(p3) msymbol(O)  label(Census: OLS with FEs) xscale(r(-.05 .02)) ) ///
			(matrix(EH[,13]), ci((14 15)) pstyle(p10) msymbol(S)  label(Short-run IV (year-on-year)) xscale(r(-.05 .02)) ) ///
			(matrix(EH[,16]), ci((17 18)) pstyle(p1) msymbol(D)  label(Long-run IV (10-year diff.)) xscale(r(-.05 .02)) ) ///
			, xscale(r(-.05 .02) noextend) legend(colfirst cols(1) pos(6) order(10 12) ) ///
			xline(0, lc(red)) name(b, replace) yscale(off) fxsize(65) title("(b) GDD(>20C)") xla(-0.05(.01).07) ///
			graphregion(style(none) m(t=0 b=0 l=0 r=2) fc(white) lc(none) )
			
	graph combine a b 	
	graph export "$fig/coefplot_new.png", replace height(1600)
	graph export "$olf/fig/main/fg03.png", replace height(1600)
	

********************************************************************************
**# Figure 4: Shapley-Owen Decomposition
********************************************************************************
* Coefpot 
********************************************************************************
	loc CL `" "Herdsize" "Climate" "FEs" "'
	
	matrix SH = J(3,2,.)
	matrix colnames SH = sriv lriv 
	matrix rown SH = `CL'
	matrix list SH 


	* Short IV 
	use `OLS' , clear 
	
	glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed5 sgr_smr_hed3 sgr_smr_hed4  sgr_smr_prec sgr_smr_ws_ave"

	glo y lndvi 
	glo x survey 
	
		** first stage 
		reghdfe survey census lndvilag ///
			${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR}  ///
				${SGRSPR} ${SGRWTR} ${SGRAUT}    ///
					, absorb(i.year i.asid ) ///
						vce(cluster asid) nocons
		cap drop herdsizehat
			predict herdsizehat, xb 
			
		** second stage 
		cap drop hshat 
		g hshat = . 
		replace hshat = herdsizehat
		*HS predicted  	
			foreach j of numlist 1/5 {
				loc z: word `j' of `ZN' 
				cap drop hshat_ez`j'
				g hshat_ez`j' = ez`j' * hshat
			}

	reghdfe lndvi hshat_ez? ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT} ///
			 , absorb(i.year i.asid ) ///
				vce( cluster asid) 

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
				
	reg lndvi hshat ${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}  ///
		`SFE' `EYFE', vce(cluster asid)

					shapley2, stat(r2) ///
						group( ///
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
							sgr_aut_ws_ave , ///
							`SFE'  ///
							`EYFE'  ///
							) 

	
			matrix SH[1, 1] = e(shapley_rel)[1,1]*100 
			matrix SH[2, 1] = e(shapley_rel)[2,1]*100 
			matrix SH[3, 1] = e(shapley_rel)[3,1]*100 
			matrix list SH 
			
	* Long IV 
	use `LD', clear 
		** second stage 
		cap drop hshat 
		g hshat = . 
		replace hshat = herdsizehat 
		
		
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
	
		reg lndvi hshat ///
			${SGRSMR} ${SGRSPR} ${SGRWTR} ${SGRAUT}  ///
				`YFE' `SFE' , vce(cluster asid)
				
				shapley2, stat(r2) ///
					group( ///
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
						sgr_aut_ws_ave , ///
						`YFE' `SFE' ///
						) 			 					
			matrix SH[1, 2] = e(shapley_rel)[1,1]*100 
			matrix SH[2, 2] = e(shapley_rel)[2,1]*100 
			matrix SH[3, 2] = e(shapley_rel)[3,1]*100 
			matrix list SH 
	

	clear 
	svmat double SH , names(col)
	g var = ""
		replace var = "Herd size" if _n == 1 
		replace var = "Climate" if _n == 2 
		replace var = "FEs" if _n == 3

	*cap drop tot 
	la var sriv "Short-run IV (% of R-squared)"
	la var lriv "Long-run IV (% of R-squared)"

	cap drop hs 
	g shs = sriv if var == "Herd size"
	g scl = sriv if var == "Climate"
	g sfe = sriv if var == "FEs"

	su sriv 
	graph hbar shs scl sfe      ///
		, over(var, sort(sriv) rev gap(50) des axis(lc(none)) label(labsize(*1.5) labcolor(red blue yellow)) )  ///
			bar(1, color(cranberry) )  bargap(-100) outergap(0) ///  //bargap(-100) outergap(-50)
			bar(2, color(midblue) )    ///
			bar(3, color(gray))  ///
				legend(off) yalt ylabel(0 "% of R{sup:2}" 10(20)80, labsize(*2) grid) ///
				title("(a) Short-run IV", size(*2)) ///
				ytitle( " " , size(*1)) ///
					blabel(total, pos(outside) format(%9.2f) size(*1.1) ) ///
						 saving("$gph/a1.gph", replace)	xsize(9) ysize(3)
						 
						 
	cap drop hs 
	g lhs = lriv if var == "Herd size"
	g lcl = lriv if var == "Climate"
	g lfe = lriv if var == "FEs"

	su lriv 
	graph hbar lhs lcl lfe ///
		, over(var, sort(lriv) rev gap(50) des axis(lc(none)) label(labsize(*1.5)) )  ///
			bar(1, color(cranberry) )  bargap(-100) outergap(0) ///  //bargap(-100) outergap(-50)
			bar(2, color(midblue) )    ///
			bar(3, color(gray))  ///
				legend(off) yalt ylabel(0 "% of R{sup:2}" 10(20)80, labsize(*2) grid) ///
				title("(b) Long-run IV", size(*2)) ///
				ytitle( " " , size(*1)) ///
					blabel(total, pos(outside) format(%9.2f) size(*1.1) color(black)  ) ///
						 saving("$gph/a2.gph", replace)	xsize(9) ysize(3) 
						 
	graph combine "$gph/a1.gph" "$gph/a2.gph" ///
		,  col(2)  imargin(0 0 0 0) ///
		xsize(9) ysize(4) graphregion(style(none) color(gs16)) ///
			saving("$gph/shapley.gph", replace)
	graph export "$fig/shapley.png", replace width(1600)
	graph export "$olf/fig/main/fg04.png", replace width(1600)
	
********************************************************************************
**# Figure C3: The Predicted Herd Size and Zhud Impacts
********************************************************************************
* PREDICTED HERDSIZE    
********************************************************************************	
	use "$dta/wrk/wrk_soum.dta", clear 
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
		
	*ecozones 
	tab eco, g(ez)
		la var ez1 "Mountain taiga"
		la var ez2 "Forest steppe"
		la var ez3 "Steppe"
		la var ez4 "Semi desert"
		la var ez5 "Desert"
		
	loc ZN `" "Mountain taiga" "Forest steppe" "Steppe" "Semi desert" "Desert" "'	

		foreach i of numlist 1/5 {
			loc z: word `i' of `ZN' 
			
			*GDD(20+)
				cap drop ez`i'_sgr_smr_hed5
					g sgr_smr_hed5_ez`i' = ez`i' * sgr_smr_hed5
						la var sgr_smr_hed5_ez`i' "\quad $ \times $ `z'"
			*HS Survey	
				cap drop survey_ez`i'
					g survey_ez`i' = ez`i' * survey
						la var survey_ez`i' "\quad $ \times $ `z'"
			*HS Census 	
				cap drop census_ez`i'
					g census_ez`i' = ez`i' * census
						la var census_ez`i' "\quad $ \times $ `z'"
		}

	*SGR WTHR 
	glo SGRAUT "sgr_aut_hed3 sgr_aut_hed4 sgr_aut_hed5 sgr_aut_prec sgr_aut_ws_ave"
	glo SGRWTR "sgr_wtr_hed3 sgr_wtr_hed4 sgr_wtr_hed5 sgr_wtr_prec sgr_wtr_ws_ave"
	glo SGRSPR "sgr_spr_hed3 sgr_spr_hed4 sgr_spr_hed5 sgr_spr_prec sgr_spr_ws_ave"
	glo SGRSMR "sgr_smr_hed3 sgr_smr_hed4 sgr_smr_hed5_ez? sgr_smr_prec sgr_smr_ws_ave"
	
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

	glo y survey 
	glo x census 

	loc varlist $y $x lndvilag ${WGRSMR} ${WGRAUT} ${WGRWTR} ${WGRSPR} ${SGRSPR} ${SGRWTR} ${SGRAUT}
	reghdfe `varlist' , absorb(i.year i.asid i.eco#i.year) vce(cluster asid)

	cap drop herdsizehat
		predict herdsizehat, xb 
	cap drop svyhat 
		g svyhat = exp(herdsizehat)

	drop if year < 1987
		loc v1 svyhat
		loc v2 su_svy_total 

	collapse ///
			(mean) `v1'_ave =`v1' ///
			(max) `v1'_max =`v1'  ///
			(min) `v1'_min =`v1' ///
			(sem) `v1'_se=`v1' ///
			(sd) `v1'_sd=`v1' ///
			(count) `v1'_n=`v1' ///
			(sum) `v1'_sum =`v1' ///
			(mean) `v2'_ave =`v2' ///
			(max) `v2'_max =`v2'  ///
			(min) `v2'_min =`v2' ///
			(sem) `v2'_se=`v2' ///
			(sd) `v2'_sd=`v2' ///
			(count) `v2'_n=`v2' ///
			(sum) `v2'_sum =`v2' ///				
			 , by(year)

	g `v1'_ucl = `v1'_ave + 1.96*`v1'_sd/sqrt(`v1'_n)
	g `v1'_lcl = `v1'_ave - 1.96*`v1'_sd/sqrt(`v1'_n)
	g `v2'_ucl = `v2'_ave + 1.96*`v2'_sd/sqrt(`v2'_n)
	g `v2'_lcl = `v2'_ave - 1.96*`v2'_sd/sqrt(`v2'_n)
		
		
	glo SIZE "ysize(5) xsize(9)"
	glo GREG "graphregion(style(none) m(0) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=0 b=0 l=2 r=2) fc(white) lc(black) lp(solid) ls(p1) lw(vthin))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(red)"
	glo YT2 "size(*.75) color(blue)"
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(red) labc(red) format(%9.2f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(blue) labc(blue) format(%9.2f)"
	glo YS1 "titlegap(0.1cm) outergap(0cm) lc(red) fext"
	glo YS2 "titlegap(0.1cm) outergap(0cm) lc(blue) fext"
	glo XT1 "size(*.75)"
	glo XT2 "$XT1 color(white)"
	glo XL1 "labs(*.5) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0.1cm) outergap(0cm) lc(none)"
	glo XS2 "$XS1"
	glo LEG "col(6) ring(1) si(*.5) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	
	glo O1 lc(red) lp(solid) mfc(white) mc(red) ms(O) msi(vsmall) ///
		lw(thin) mlw(thin) yaxis(1)
	glo O2 lc(blue) lp(solid) mfc(white) mc(blue) ms(D) msi(vsmall) ///
		lw(thin) mlw(thin) yaxis(2)
	glo O3 lc(red) lp(dash) mfc(white) mc(red) ms(O) msi(vsmall) ///
		lw(thin) mlw(thin) yaxis(1)		
	glo O4 lc(blue) lp(dash) mfc(white) mc(red) ms(D) msi(vsmall) ///
		lw(thin) mlw(thin) yaxis(2)		
	
	//costumize 	
	loc ttl ""
	loc yti1 "Predicted herdsize in June (in millions)"	
	loc yti2 "Actual herdsize in June (in millions)"	
	loc xti " "
	loc ord `"8 "Predicted" 7 "95% CI"  10 "Actual" 9 "95% CI" 1 "Privatization period" 2 "Zhud years" "'
	loc xla "1987(3)2024"
	loc yla1 ".1(.1).5"
	loc yla2 ".1(.1).5"
	
	g upper = .5 
		
		
	twoway ///
		(area upper y if inrange(y, 1991, 1993), lw(*0) bcolor(gs12%75) base(.1)) ///
		(area upper y if inrange(y, 1999, 2003), lw(*0) bcolor(blue%30) base(.1)) ///
		(area upper y if inrange(y, 2009, 2011), lw(*0) bcolor(blue%30) base(.1)) ///
		(area upper y if inrange(y, 2017, 2018), lw(*0) bcolor(blue%30) base(.1)) ///
		(area upper y if inrange(y, 2019, 2021), lw(*0) bcolor(blue%30) base(.1)) ///
		(area upper y if inrange(y, 2022, 2024), lw(*0) bcolor(blue%30) base(.1)) ///
		(rcap svyhat_ucl svyhat_lcl year  , $O1 ) ///
		(connect svyhat_ave year , $O1 ) ///
		(rcap su_svy_total_ucl su_svy_total_lcl year  , $O2 ) ///
		(connect su_svy_total_ave year , $O2 ) ///		
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti("`xti'" , $XT1 ) xla(`xla' , $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(12) $LEG) fysize(75) ///
			saving("$fig/fg_hshat.gph", replace )
	
********************************************************************************
	use "$dta/wrk/wrk_soum.dta" , clear 
	keep year asid su_cen_* 
		ren su_cen_* * 
	replace year = year - 1
		
	* at national level 		
	collapse (sum) sheep goat cattle horse camel total , by(year)

	tset year 
	foreach v of varlist sheep goat cattle horse camel total {
		g d_`v' = d.`v'
	}
	
	foreach v of varlist total {
		g g_`v' = (`v' - L.`v')/ L.`v' *100
	}
	
	g pos = 0 
	g neg = 0 
	
	foreach v in camel horse cattle goat sheep {
		g  pos_`v' = d_`v' + pos if d_`v' >= 0  
		replace pos  = pos_`v' if d_`v' >= 0 
		
		g  neg_`v' = d_`v' + neg if d_`v' < 0  
		replace neg   = neg_`v' if d_`v' < 0 		
	}

	glo SIZE "ysize(5) xsize(9)"
	glo GREG "graphregion(style(none) m(0) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=0 b=0 l=2 r=2) fc(white) lc(black) lp(solid) ls(p1) lw(thin))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(green)"
	glo YT2 "size(*.75) color(white)"
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(green) labc(green) format(%9.0f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(white) labc(white) format(%9.0f)"
	glo YS1 "titlegap(0.1cm) outergap(0cm) lc(green) fext"
	glo YS2 "titlegap(0.1cm) outergap(0cm) lc(black) fext"
	glo XT1 "size(*.75)"
	glo XT2 "$XT1 color(white)"
	glo XL1 "labs(*.5) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0.1cm) outergap(0cm) lc(none)"
	glo XS2 "$XS1"
	glo LEG "pos(11) col(1) ring(0) si(*.5) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	glo O1 fc(red blue green orange cyan) lw(*.25 ..) lc(white ..) fi(75 ..) yaxis(1) 
	glo O2 fc(red blue green orange cyan) lw(*.25 ..) lc(white ..) fi(75 ..) yaxis(1) 

	glo O0 lc(green) lp(solid) mfc(white) mc(green) ms(O) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(2)
		
	//costumize 	
	//costumize 	
	loc ttl ""
	loc yti1 "Percentage change (%)"	
	loc yti2 "Millions of Livestocks (Cattle, Horses, and Camels)"	
	loc xti "Year"
	loc ord `" 6 "Total livestocks in sheep units" 1 "Privatization period" 2 "Zhud years" "'
	loc xla "1987(3)2024"
	loc yla1 "-25(10)25"
	loc yla2 "-25(10)25"		
	
	drop if year < 1987
	set obs 39 
	replace year = 2024 if year == . 
	
	g upper = 25 
	
	twoway ///
		(area upper y if inrange(y, 1991, 1993), lw(*0) bcolor(gs12%75) base(-25)) ///
		(area upper y if inrange(y, 1999, 2003), lw(*0) bcolor(blue%30) base(-25)) ///
		(area upper y if inrange(y, 2009, 2011), lw(*0) bcolor(blue%30) base(-25)) ///
		(area upper y if inrange(y, 2017, 2018), lw(*0) bcolor(blue%30) base(-25)) ///
		(area upper y if inrange(y, 2019, 2021), lw(*0) bcolor(blue%30) base(-25)) ///
		(area upper y if inrange(y, 2022, 2024), lw(*0) bcolor(blue%30) base(-25)) ///
		(connect g_total year, $O0) ///
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti(`xti' , $XT1 ) xla(`xla' , $XL1 ) xsc( $XS1 ) ///
			legend( off ) ///
					yline(0 , lc(red%10) axis(2)) fysize(50) ///
			saving("$fig/fg_dhs.gph", replace )

	
	
	glo SIZE "ysize(6) xsize(9)"
	glo GREG "graphregion(style(none) m(zero) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=2 b=2 l=-2 r=0) fc(white) lc(white) lp(solid) ls(p1) lw(*0))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(black)"
	glo YT2 "size(*.75) color(white)"
	glo YL1 "ang(v) labs(*.75) labgap(*0) nogrid tlc(black) labc(black) format(%9.0f)"
	glo YL2 "ang(v) labs(*.75) labgap(*0) nogrid tlc(black) labc(black) format(%9.0f)"
	glo YS1 "titlegap(0cm) outergap(0cm) lc(black) ext"
	glo YS2 "titlegap(0cm) outergap(0cm) lc(black) ext"
	glo XT1 "size(*.75) color(white)"
	glo XT2 "color(white)"
	glo XL1 "labs(*.75) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0cm) outergap(0cm) lc(black) ext"
	glo XS2 "$XS1"
// 	glo LEG "col(3) ring(0) si(*.75) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	graph combine ///
		$fig/fg_hshat.gph ///
		$fig/fg_dhs.gph ///
		,  iscale(*1) col(1) ///
			imargin(*1)  ///
			$GREG $SIZE $PREG   

	graph export "$fig/fgC3.png", replace height(1600)
	graph export "$olf/fig/supp/fgC3.png", replace height(1600)
	
********************************************************************************
**# Figure H.1: Biomass vs NDVI vs EVI at the plot level
********************************************************************************
* BIOMASS   
********************************************************************************	
	use  $raw/bms/bms_plot.dta , clear 
	
		keep gid year bms

	loc STATVAR ""
	foreach v of varlist bms {
			
		loc STATVAR  "`STATVAR' (mean) `v'_ave = `v' (sem) `v'_se = `v'"
		di "`STATVAR'"
	}

	collapse `STATVAR' , by(year)
	
	foreach v in bms { 
		g `v'_ucl = `v'_ave + 1.96*`v'_se
		g `v'_lcl = `v'_ave - 1.96*`v'_se
	}	


	glo SIZE "ysize(6) xsize(9)"
	glo GREG "graphregion(style(none) m(zero) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=2 b=2 l=2 r=2) fc(white) lc(white) lp(solid) ls(p1) lw(*0))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(black)"
	glo YT2 "size(*.75) color(white)"
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.2f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.2f)"
	glo YS1 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo YS2 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo XT1 "size(*.75) color(white)"
	glo XT2 "color(white)"
	glo XL1 "labs(*.5) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo XS2 "$XS1"
	glo LEG "col(3) ring(0) si(*.5) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	
	glo O1 lc(dkgreen) lp(solid) lw(thin) mlc(dkgreen) mlw(vthin) mfc(white)  ///
		  ms(S) msi(small) yaxis(1)	
	glo O2 lc(dkgreen) lp(solid) lw(thin) yaxis(1)		
	glo O3 lc(dkgreen) lp(dash) lw(*1) degree(4) yaxis(2)	
		
	//costumize 	
	loc ttl "" //"(a) Temperature"
	loc yti1 "(a) Vegetation biomass (cwt/ha)"	
	loc yti2 "Average temperature ({sup:o}C)"	
	loc xti ""
	loc ord `"2 "Average vegetation biomass (august)" 1 "95% CI" 3 "A polynomial trend line of degree 4" "'
	loc xla "2007(1)2020"
	loc yla1 "1.5(2)6.5"
	loc yla2 "1.5(2)6.5"	
	
	twoway ///
		(rcap bms_ucl bms_lcl year, $O2) ///
		(connect bms_ave year, $O1) ///	
		(lpoly bms_ave year, $O3 ) ///		
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti("`xti'" , $XT1 ) xla(`xla', $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(11) $LEG) ///
				saving("$fig/fg_bms.gph", replace )

* VEGI    
********************************************************************************
	use  $dta/vegi/plot/vegi_plot_modis_aug.dta , clear 
	
		keep gid year *_sm_ave  
			ren *_sm_ave * 

	loc STATVAR ""
	foreach v of varlist ndvi evi savi msavi nirv {
		winsor2 `v' if year == 1985, cuts(10 90) replace 
			
		loc STATVAR  "`STATVAR' (mean) `v'_ave = `v' (sem) `v'_se = `v'"
		*di "`STATVAR'"
	}
	drop if year < 2007	| year > 2020	
	collapse `STATVAR' , by(year)
	
	foreach v in ndvi evi savi msavi nirv { 
		g `v'_ucl = `v'_ave + 1.96*`v'_se
		g `v'_lcl = `v'_ave - 1.96*`v'_se
	}
		

	glo SIZE "ysize(6) xsize(9)"
	glo GREG "graphregion(style(none) m(zero) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=2 b=2 l=2 r=2) fc(white) lc(white) lp(solid) ls(p1) lw(*0))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(black)"
	glo YT2 "size(*.75) color(white)"
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.2f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.2f)"
	glo YS1 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo YS2 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo XT1 "size(*.75) color(white)"
	glo XT2 "color(white)"
	glo XL1 "labs(*.5) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo XS2 "$XS1"
	glo LEG "col(3) ring(0) si(*.5) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	
	glo O1 lc(dkgreen) lp(solid) lw(thin) mlc(dkgreen) mlw(vthin) mfc(white)  ///
		  ms(O) msi(small) yaxis(1)	
	glo O2 lc(dkgreen) lp(solid) lw(thin) yaxis(1)		
	glo O3 lc(dkgreen) lp(dash) lw(*1) degree(4) yaxis(2)	
		
	//costumize 	
	loc ttl "" //"(a) Temperature"
	loc yti1 "(b) NDVI"	
	loc yti2 "Average temperature ({sup:o}C)"	
	loc xti ""
	loc ord `"2 "Average NDVI (august)" 1 "95% CI" 3 "A polynomial trend line of degree 4" "'
	loc xla "2007(1)2020"
	loc yla1 "0.25(.05)0.4"
	loc yla2 "0.25(.05)0.4"	
	
	twoway ///
		(rcap ndvi_ucl ndvi_lcl year, $O2) ///
		(connect ndvi_ave year, $O1) ///	
		(lpoly ndvi_ave year, $O3 ) ///		
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti("`xti'" , $XT1 ) xla(`xla', $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(11) $LEG) ///
				saving("$fig/fg_ndvi.gph", replace )
				
	
	glo O1 lc(dkgreen) lp(solid) lw(thin) mlc(dkgreen) mlw(vthin) mfc(white)  ///
		  ms(D) msi(small) yaxis(1)	
	glo O2 lc(dkgreen) lp(solid) lw(thin) yaxis(1)		
	glo O3 lc(dkgreen) lp(dash) lw(*1) degree(4) yaxis(2)	
		
	//costumize 	
	loc ttl "" //"(b) Percipitation"
	loc yti1 "(c) EVI"	
	loc yti2 "Accumulated precipitation (mm)"	
	loc xti ""
	loc ord `"2 "Average EVI (august)" 1 "95% CI" 3 "A polynomial trend line of degree 4" "'
	loc xla "2007(1)2020"
	loc yla1 "0.2(.05)0.25"
	loc yla2 "0.2(.05)0.25"		
	
	twoway ///
		(rcap evi_ucl evi_lcl year, $O2) ///
		(connect evi_ave year, $O1) ///	
		(lpoly evi_ave year, $O3 ) ///		
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti("`xti'", $XT1 ) xla(`xla' , $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(11) $LEG) ///
				saving("$fig/fg_evi.gph", replace )
				
				
				
	glo SIZE "ysize(6) xsize(9)"
	glo GREG "graphregion(style(none) m(zero) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=2 b=2 l=-2 r=0) fc(white) lc(white) lp(solid) ls(p1) lw(*0))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(black)"
	glo YT2 "size(*.75) color(white)"
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.0f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.0f)"
	glo YS1 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo YS2 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo XT1 "size(*.75) color(white)"
	glo XT2 "color(white)"
	glo XL1 "labs(*.5) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0.1cm) outergap(0cm) lc(black) ext"
	glo XS2 "$XS1"
	glo LEG "col(3) ring(0) si(*.75) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	graph combine ///
		$fig/fg_bms.gph ///
		$fig/fg_ndvi.gph ///
		$fig/fg_evi.gph ///
		,  iscale(*1) col(1) ///
			imargin(*1) ///
			$GREG $SIZE $PREG   

	graph export "$fig/fgH1.png", replace height(1600)
	graph export "$olf/fig/supp/fgH1.png", replace height(1600)
	

				
********************************************************************************
**# Figure H.2: Percentage change in the December livestock (1985-2023) by type
********************************************************************************
********************************************************************************
* LIVESTOCK at country level : by types 
********************************************************************************	

	use "$dta/wrk/wrk_soum.dta" , clear 
	keep year asid cen_* 
		ren cen_* * 
		 drop if year == 2024
	* at national level 		
	collapse (sum) sheep goat cattle horse camel total , by(year)

	tset year 
	foreach v of varlist sheep goat cattle horse camel {
		g g_`v' = (`v' - L.`v')/ L.`v' *100
	}
	
	glo SIZE "ysize(5) xsize(9)"
	glo GREG "graphregion(style(none) m(0) fc(white) lc(none))" 	
	glo PREG "plotregion(m(t=0 b=0 l=2 r=2) fc(white) lc(black) lp(solid) ls(p1) lw(thin))"
	glo TTL "size(*.75) color(white)"
	glo YT1 "size(*.75) color(black)"
	glo YT2 "size(*.75) color(white)"
	glo YL1 "ang(v) labs(*.5) labgap(*0) nogrid tlc(black) labc(black) format(%9.0f)"
	glo YL2 "ang(v) labs(*.5) labgap(*0) nogrid tlc(white) labc(white) format(%9.0f)"
	glo YS1 "titlegap(0.1cm) outergap(0cm) lc(none)"
	glo YS2 "$YS1"
	glo XT1 "size(*.75)"
	glo XT2 "$XT1 color(white)"
	glo XL1 "labs(*.5) labgap(*0) format(%4.0f) nogrid tlc(black)"
	glo XL2 "$XL1 labc(white) tlc(white)"
	glo XS1 "titlegap(0.1cm) outergap(0cm) lc(none)"
	glo XS2 "$XS1"
	glo LEG "col(7) ring(1) si(*.5) symxsize(5) symysize(2) rowgap(*0) linegap(*0) region(fc(none))"
	
	
	glo O1 lc(red) lp(solid) mfc(red) mc(red) ms(Oh) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(1)
	glo O2 lc(blue) lp(solid) mfc(blue) mc(blue) ms(Dh) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(1)
	glo O3 lc(green) lp(solid) mfc(green) mc(green) ms(T) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(2)
	glo O4 lc(orange) lp(solid) mfc(orange) mc(orange) ms(S) msi(vsmall) ///
		lw(thin) mlw(vthin) yaxis(2)
	glo O5 lc(cyan) lp(solid) mfc(cyan) mc(cyan) ms(Sh) msi(vsmall) lw(thin) ///
		mlw(vthin) yaxis(2)

	//costumize 	
	loc ttl ""
	loc yti1 "Percentage change in livestocks (%)"	
	loc yti2 "Millions of Livestocks (Cattle, Horses, and Camels)"	
	loc xti "Year"
	loc ord `"6 "Sheep" 7 "Goats" 8 "Cattle" 9 "Horses" 10 "Camels" 1 "Privatization period" 2 "Zhud years" "'
	loc xla "1987(5)2022"
	loc yla1 "-40(10)40"
	loc yla2 "-40(10)40"			
	
	drop if year < 1987
	set obs 39 
	replace year = 2024 if year == . 
	
	
	g upper = 40 
	
	twoway ///
		(area upper y if inrange(y, 1991, 1993), lw(*0) bcolor(gs12%75) base(-40)) ///
		(area upper y if inrange(y, 1999, 2002), lw(*0) bcolor(blue%30) base(-40)) ///
		(area upper y if inrange(y, 2009, 2010), lw(*0) bcolor(blue%30) base(-40)) ///
		(area upper y if inrange(y, 2017, 2018), lw(*0) bcolor(blue%30) base(-40)) ///
		(area upper y if inrange(y, 2019, 2020), lw(*0) bcolor(blue%30) base(-40)) ///	
		(area upper y if inrange(y, 2022, 2023), lw(*0) bcolor(blue%30) base(-40)) ///	
		(connect g_sheep year , $O1 ) ///
		(connect g_goat year , $O2 ) ///
		(connect g_cattle year , $O3 ) ///
		(connect g_horse year , $O4 ) ///
		(connect g_camel year , $O5 ) ///
		, title("`ttl'", $TTL ) $PREG $GREG $SIZE ///
			yti(`yti1' , axis(1) $YT1 ) yla(`yla1' , axis(1) $YL1 ) ysc( axis(1) $YS1 )  ///
			yti(`yti2' , axis(2) $YT2 ) yla(`yla2' , axis(2) $YL2 ) ysc( axis(2) $YS2 )  ///
			xti(`xti' , $XT1 ) xla(`xla' , $XL1 ) xsc( $XS1 ) ///
			legend(order(`ord') pos(12) $LEG) ///
					yline(0 , lc(red%10) axis(2)) ///
			saving("$fig/fg_dhs.gph", replace )
	graph export "$fig/fgH2.png", replace height(1600)
	graph export "$olf/fig/supp/fgH2.png", replace height(1600)
	
		
	
	