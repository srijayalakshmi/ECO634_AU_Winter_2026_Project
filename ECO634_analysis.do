cd "D:\Srijayalakshmi\AHMEDABAD UNIVERSITY\SEMESTER 2\ECO634\Project"

**Cleaning State Panel File
capture mkdir "D:\Srijayalakshmi\AHMEDABAD UNIVERSITY\SEMESTER 2\ECO634\Project\Data"
import delimited "D:\Srijayalakshmi\AHMEDABAD UNIVERSITY\SEMESTER 2\ECO634\Project\Data\prs_state_panel.csv", clear varnames(1) encoding(UTF-8) bindquote(strict)
drop state
drop loans_and_advancesrscrore
drop page_format
drop total_debt_outstandingofgsdp
foreach v of varlist state_name fiscal_year report_year estimate_type {
    replace `v' = strtrim(`v')
} // Remove trailing/leading whitespace
foreach v of varlist state_name estimate_type {
    replace `v' = lower(`v')
} // Standardize case
label variable state_name "Name of the Indian State/UT"
label variable fiscal_year "Financial Year (e.g., 2018-19)"
replace state_name = strtrim(strlower(state_name))
replace estimate_type = strtrim(strlower(estimate_type))
replace estimate_type = "budget estimate" if estimate_type == "budget estimates"
replace estimate_type = "revised estimate" if estimate_type == "revised estimates"
replace estimate_type = "actual" if estimate_type == "actuals"
gen year = real(substr(fiscal_year, 1, 4))
label var year "Fiscal year start (e.g. 2018 = 2018-19)"
drop if inlist(fiscal_year, "2016-17", "2016-20", "2017-18", "2017-20")
rename own_tax_revenuerscrore        own_tax_revenue_cr
rename non_tax_revenuerscrore        non_tax_revenue_cr
rename state_share_central_taxesrscrore central_tax_share_cr
rename grants_from_centrerscrore     central_grants_cr
rename total_revenue_receiptsrscrore total_revenue_receipts_cr
rename revenue_expenditurerscrore    revenue_expenditure_cr
rename capital_expenditurerscrore    capital_expenditure_cr
rename total_expenditurerscrore      total_expenditure_cr
rename revenue_deficit_surplusrscrore revenue_balance_cr
rename fiscal_deficitrscrore         fiscal_deficit_cr
destring own_tax_revenue_cr non_tax_revenue_cr central_tax_share_cr central_grants_cr total_revenue_receipts_cr revenue_expenditure_cr total_expenditure_cr capital_expenditure_cr total_expenditure_cr revenue_balance_cr fiscal_deficit_cr, replace ignore("NA")
ds, has(type numeric)
foreach v in `r(varlist)' {
    format `v' %15.0f
}
sort state_name year
order year, after(fiscal_year)
save "Data\prs_state_panel.dta", replace
export delimited using "Data\prs_state_panel_cleaned.csv", replace

**Cleaning State Panel Debt File
import delimited "D:\Srijayalakshmi\AHMEDABAD UNIVERSITY\SEMESTER 2\ECO634\Project\Data\prs_state_debt.csv", clear varnames(1) encoding(UTF-8) bindquote(strict)
drop state
foreach v of varlist state_name fiscal_year report_year estimate_type {
    replace `v' = strtrim(`v')
} // Remove trailing/leading whitespace
foreach v of varlist state_name estimate_type {
    replace `v' = lower(`v')
} // Standardize case
label variable state_name "Name of the Indian State/UT"
label variable fiscal_year "Financial Year (e.g., 2018-19)"
replace state_name = strtrim(strlower(state_name))
replace estimate_type = strtrim(strlower(estimate_type))
replace estimate_type = "budget estimate" if estimate_type == "budget estimates"
replace estimate_type = "revised estimate" if estimate_type == "revised estimates"
replace estimate_type = "actual" if estimate_type == "actuals"
gen year = real(substr(fiscal_year, 1, 4))
label var year "Fiscal year start (e.g. 2018 = 2018-19)"
drop if inlist(fiscal_year, "2016-17", "2016-20", "2017-18", "2017-20")
replace fiscal_year = "2018-19" if fiscal_year == "2019-18"
rename total_debt_outstandingofgsdp total_debt_outstanding_percent
destring total_debt_outstanding_percent, replace ignore("NA")
ds, has(type numeric)
foreach v in `r(varlist)' {
    format `v' %15.0f
}
sort state_name year
order year, after(fiscal_year)
save "Data\prs_state_debt.dta", replace
export delimited using "Data\prs_state_debt_cleaned.csv", replace

** Merging the above files
use "Data\prs_state_panel.dta", clear
merge 1:1 state_name fiscal_year report_year estimate_type year using "Data\prs_state_debt.dta"
keep if _merge == 3
drop _merge
gen priority = .
replace priority = 1 if estimate_type == "actual"
replace priority = 2 if estimate_type == "revised estimate"
replace priority = 3 if estimate_type == "budget estimate"
sort state_name fiscal_year priority
by state_name fiscal_year: keep if _n == 1
drop priority
rename estimate_type estimate_type_prs
label variable estimate_type_prs "Type of Estimate provided by PRS India"
save "Data\prs_state_panel_final.dta", replace
export delimited using "Data\prs_state_panel_final.csv", replace

** Cleaning RBI data file
import delimited "D:\Srijayalakshmi\AHMEDABAD UNIVERSITY\SEMESTER 2\ECO634\Project\Data\gsdp_rbi.csv", clear varnames(1) encoding(UTF-8) bindquote(strict)
drop rbi_edition_gsdp_constant rbi_edition_gsdp_current source_pdf_gsdp_constant source_pdf_gsdp_current rbi_edition_pop source_pdf_pop
replace state_name = strtrim(strlower(state_name))
drop estimate_type_gsdp_constant
rename estimate_type_gsdp_current estimate_type_rbi
label variable estimate_type_rbi "Type of Estimate provided by RBI"
foreach v of varlist state_name fiscal_year estimate_type_rbi nat_pop_growth_rate_total {
    replace `v' = strtrim(`v')
} // Remove trailing/leading whitespace
foreach v of varlist state_name estimate_type_rbi {
    replace `v' = lower(`v')
} // Standardize case
replace estimate_type_rbi = strtrim(strlower(estimate_type_rbi))
replace estimate_type_rbi = strtrim(strlower(estimate_type_rbi))
replace estimate_type_rbi = "revised estimate" if estimate_type_rbi == "revised estimates"
replace estimate_type_rbi = "actual" if estimate_type_rbi == "actuals"
replace estimate_type_rbi = "na" if estimate_type_rbi == ""
gen year = real(substr(fiscal_year, 1, 4))
destring calendar_year, replace ignore("NA")
replace year = calendar_year if missing(year) & !missing(calendar_year)
order year, after(fiscal_year)
destring gsdp_constant_crore gsdp_current_crore gsdp_constant_lakh gsdp_current_lakh nat_pop_growth_rate_total, replace ignore("NA")
sort state_name year variable
bysort state_name year: replace nat_pop_growth_rate_total = nat_pop_growth_rate_total[_N] if missing(nat_pop_growth_rate_total)
keep if variable == "gsdp"
drop calendar_year variable
label var year "Fiscal year start (e.g. 2011 = 2011-12)"
order state_name fiscal_year year estimate_type_rbi gsdp_constant_crore gsdp_current_crore nat_pop_growth_rate_total
sort state_name year
drop if inlist(estimate_type_rbi, "advance estimate")
order year, after(fiscal_year)
order nat_pop_growth_rate_total, after(gsdp_current_lakh)
label variable state_name "Name of the Indian State/UT"
label variable fiscal_year "Financial Year (e.g., 2018-19)"
label variable gsdp_constant_crore       "GSDP at Constant Prices (Base: 2011-12) in Rs. Crore"
label variable gsdp_current_crore        "GSDP at Current Prices in Rs. Crore"
label variable gsdp_constant_lakh        "GSDP at Constant Prices (Base: 2011-12) in Rs. Lakh"
label variable gsdp_current_lakh         "GSDP at Current Prices in Rs. Lakh"
label variable nat_pop_growth_rate_total "Natural Population Growth Rate (Total)"
ds, has(type numeric)
foreach v in `r(varlist)' {
    format `v' %15.0f
}
save "Data\gsdp_rbi.dta", replace
export delimited using "Data\gsdp_rbi_cleaned.csv", replace

** Cleaning Population Data
import delimited "D:\Srijayalakshmi\AHMEDABAD UNIVERSITY\SEMESTER 2\ECO634\Project\Data\census_2011_population.csv", clear varnames(1) bindquote(strict)
destring statecode disttcode, replace
rename areaname state_name
keep if agegroup == "All ages"
keep state_name agegroup totalpersons
drop agegroup
replace state_name = subinstr(state_name, "State - ", "", .)
split state_name, parse(" (")
replace state_name = state_name1
drop state_name1 state_name2
replace state_name = strtrim(lower(state_name))
replace state_name = "bihar" if state_name == "jharkhand"
replace state_name = "madhya pradesh" if state_name == "chhattisgarh"
replace state_name = "uttar pradesh" if inlist(state_name, "uttaranchal", "uttarakhand")
replace state_name = "odisha" if state_name == "orissa"
replace state_name = "puducherry" if state_name == "pondicherry"
drop if state_name == "jammu & kashmir"
gen is_ut = 0
replace is_ut = 1 if inlist(state_name, "andaman & nicobar islands", "chandigarh", "dadra & nagar haveli")
replace is_ut = 1 if inlist(state_name, "daman & diu", "lakshadweep", "puducherry")
replace is_ut = 1 if inlist(state_name, "delhi", "nct of delhi", "d & n haveli")
drop if is_ut == 1
drop is_ut
drop if state_name == "india"
rename totalpersons population
duplicates report state_name
duplicates drop state_name, force 
isid state_name
ds, has(type numeric)
foreach v in `r(varlist)' {
    format `v' %15.0f
}
save "Data\population_2011_census.dta", replace
export delimited using "Data\population_2011_census_cleaned.csv", replace

** Extrapolation
use "Data\gsdp_rbi.dta", clear
merge m:1 state_name using "Data\population_2011_census.dta"
drop if _merge == 1 // Drop states that have no GSDP/Growth data
drop _merge
sort state_name year
gen growth_factor = 1 + (nat_pop_growth_rate_total / 1000)
bysort state_name (year): gen ln_factor = ln(growth_factor[_n-1]) if year > 2011
bysort state_name (year): gen cum_ln_growth = sum(ln_factor) 
gen projected_pop = population * exp(cum_ln_growth)
replace projected_pop = population if year == 2011
label var projected_pop "Projected Population based on Natural Growth Rate"
drop growth_factor ln_factor cum_ln_growth population
ds, has(type numeric)
foreach v in `r(varlist)' {
    format `v' %15.0f
}
save "Data\population_gsdp.dta", replace
export delimited using "Data\population_gsdp_cleaned.csv", replace

** Merge PRS Data, RBI & Census Population Data
use "Data\population_gsdp.dta", clear
merge 1:1 state_name fiscal_year year using "Data\prs_state_panel_final.dta"
keep if _merge == 3
drop _merge report_year
gen total_exp_pct_gsdp_curr = (total_expenditure_cr / gsdp_current_crore) * 100
label var total_exp_pct_gsdp_curr "Total Expenditure as % of GSDP (Current Prices)"
gen total_exp_pct_gsdp_const = (total_expenditure_cr / gsdp_constant_crore) * 100
label var total_exp_pct_gsdp_const "Total Expenditure as % of GSDP (Constant Prices)"
gen per_capita_gsdp_curr = (gsdp_current_crore * 10000000) / projected_pop
label var per_capita_gsdp_curr "Per Capita GSDP (Current Prices in INR)"
gen per_capita_gsdp_const = (gsdp_constant_crore * 10000000) / projected_pop
label var per_capita_gsdp_const "Per Capita GSDP (Constant Prices in INR)"
replace capital_expenditure_cr = total_expenditure_cr - revenue_expenditure_cr if missing(capital_expenditure_cr)
gen capex_share_total_exp = (capital_expenditure_cr / total_expenditure_cr) * 100
label var capex_share_total_exp "Capital Expenditure as % of Total Expenditure"
tabstat total_exp_pct_gsdp_curr per_capita_gsdp_curr capex_share_total_exp, statistics(mean sd min max) by(state_name)
destring total_exp_pct_gsdp_curr total_exp_pct_gsdp_const per_capita_gsdp_curr per_capita_gsdp_const capex_share_total_exp, replace ignore("NA")
label variable total_exp_pct_gsdp_curr   "Total Expenditure as % of GSDP (Current Prices)"
label variable total_exp_pct_gsdp_const  "Total Expenditure as % of GSDP (Constant Prices)"
label variable per_capita_gsdp_curr      "Per Capita GSDP in INR (Current Prices)"
label variable per_capita_gsdp_const     "Per Capita GSDP in INR (Constant Prices)"
label variable capex_share_total_exp     "Capital Expenditure as % of Total Expenditure"
ds, has(type numeric)
foreach v in `r(varlist)' {
    format `v' %15.0f
}
save "Data\prs_rbi_merged.dta", replace
export delimited using "Data\prs_rbi_merged.csv", replace
codebook
labelbook

** Preparing for Analysis
encode state_name, gen(state_id)
destring year, replace force
xtset state_id year
xtdescribe
label var state_id "State (numeric)"
gen own_tax_share    = (own_tax_revenue_cr / total_revenue_receipts_cr) * 100
label var own_tax_share "Own Tax Revenue Share (%)"
gen non_tax_share    = (non_tax_revenue_cr / total_revenue_receipts_cr) * 100
label var non_tax_share "Non-Tax Revenue Share (%)"
gen cent_tax_share   = (central_tax_share_cr / total_revenue_receipts_cr) * 100
label var cent_tax_share "Central Tax Share (%)"
gen cent_grant_share = (central_grants_cr / total_revenue_receipts_cr) * 100
label var cent_grant_share "Central Grants Share (%)"
gen fiscal_deficit_pct_gsdp = (fiscal_deficit_cr / gsdp_current_crore) * 100
label var fiscal_deficit_pct_gsdp "Fiscal Deficit % GSDP (current)"
gen rev_exp_share = (revenue_expenditure_cr / total_expenditure_cr) * 100
label var rev_exp_share "Revenue Exp % Total Exp"
gen ln_pcgsdp = ln(per_capita_gsdp_curr)
label var ln_pcgsdp "Log Per-Capita GSDP"
gen covid = (year == 2020 | year == 2021) //COVID dummy
label var covid "COVID years (2020-21 & 2021-22)"
estpost summarize

xtline capex_share_total_exp, overlay title("CapEx Share") ytitle("CapEx % Total Exp") xtitle("Year")
graph export "Analysis\graph_1_xtline_capex_all.png", replace width(1600)

xtline fiscal_deficit_pct_gsdp, overlay title("Fiscal Deficit % GSDP — All 23 States") ytitle("FD % GSDP") xtitle("Year")
graph export "Analysis\graph_2_xtline_fd_all.png", replace width(1600)

** Analysis on Revenue
preserve
estpost tabstat own_tax_share non_tax_share cent_tax_share cent_grant_share, by(year) statistics(mean) columns(statistics) listwise
esttab . using "Analysis\revenue_shares.tex", replace cells("mean(fmt(2))") label booktabs nonumbers title("Mean Revenue Shares by Year") addnotes("Source: PRS India & RBI")
restore

preserve
collapse (mean) own_tax_share non_tax_share cent_tax_share cent_grant_share (count) n = state_id , by(year)
list year own_tax_share non_tax_share cent_tax_share cent_grant_share, sep(0) // Table: mean revenue shares over time
restore

xtreg own_tax_share i.year, fe vce(robust) // State-level fixed-effects: did own-tax share grow post-COVID?
estadd local statefe "Yes"
estimates store fe_owntax

xtreg cent_tax_share i.year, fe vce(robust)
estadd local statefe "Yes"
estimates store fe_centtax

esttab fe_owntax fe_centtax using "Analysis\Regression_Results_tax.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(%15.9f) se(%15.9f) compress nogaps label scalars("statefe State Fixed Effects") title("Impact on Tax due to COVID") mtitle("Own Tax Share" "Central Tax Share") // Output results to a table

esttab fe_owntax fe_centtax using "Analysis\Regression_Results_tax.tex", replace star(* 0.10 ** 0.05 *** 0.01)  b(%15.9f) se(%15.9f) label scalars("statefe State Fixed Effects") stats(N r2_w, labels("Observations" "Within R²")) title("Impact on Tax due to COVID") mtitle("Own Tax Share" "Central Tax Share") booktabs

preserve
keep if state_name == "maharashtra" // Since Maharashtra is the largest economy
sort year // Build stacked bar manually: we need shares that sum to 100. Non-central own revenues = own_tax + non_tax. Central inflows = central_tax_share + central_grants
graph bar own_tax_share non_tax_share cent_tax_share cent_grant_share, ///
    over(year, label(angle(45) labsize(small))) ///
    stack                                        ///
    bar(1, fcolor(navy)   lcolor(black) lwidth(thin)) ///
    bar(2, fcolor(teal)   lcolor(black) lwidth(thin)) ///
    bar(3, fcolor(orange) lcolor(black) lwidth(thin)) ///
    bar(4, fcolor(red)    lcolor(black) lwidth(thin)) ///
    legend(order(1 "Own Tax Revenue"     ///
                 2 "Non-Tax Revenue"     ///
                 3 "Central Tax Share"   ///
                 4 "Central Grants")     ///
           position(6) rows(2) size(small)) ///
    ytitle("Share of Total Revenue Receipts (%)") ///
    title("Revenue Composition — Maharashtra", size(medsmall)) ///
    subtitle("2018–19 to 2023–24", size(small)) ///
    note("Source: PRS India & RBI Handbook of Indian Statistics", size(vsmall))

graph export "Analysis\graph_3_revenue_composition_maharashtra.png", replace width(1600)
restore

** Analysis on Expenditure and Revenue-Capital Mix
xtsum capex_share_total_exp
estpost summarize capex_share_total_exp, detail
esttab . using "Analysis\capex_share_total_exp_table_1.tex", replace cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") label booktabs nonumbers title("Summary Statistics: Capex Share") addnotes("Note: Results show overall distribution.")
outreg2 using "Analysis\capex_share_total_exp_table_2.tex", replace sum(log) eqkeep(N mean sd min max) title(Summary Statistics) tex

xtreg capex_share_total_exp i.year, fe vce(robust)
estadd local statefe "Yes"
estimates store fe_capex

gen post2021 = (year >= 2021) // Post-COVID increase?
xtreg capex_share_total_exp post2021, fe vce(robust)
estadd local statefe "Yes"
estimates store fe_capex_post

esttab fe_capex fe_capex_post using "Analysis\Regression_Results_capex.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(%15.9f) se(%15.9f) compress nogaps label scalars("statefe State Fixed Effects") title("Impact on Capital Expenditure due to COVID") mtitle("Year Effects" "Post-COVID Trend") // Output results to a table

esttab fe_capex fe_capex_post using "Analysis\Regression_Results_capex.tex", replace star(* 0.10 ** 0.05 *** 0.01)  b(%15.9f) se(%15.9f) label scalars("statefe State Fixed Effects") stats(N r2_w, labels("Observations" "Within R²")) title("Impact on Capital Expenditure due to COVID") mtitle("Year Effects" "Post-COVID Trend") booktabs

local focal "maharashtra" "tamil nadu" "uttar pradesh" "west bengal" "rajasthan" // Line chart: CapEx share for all states. Sort out a state_name label for legend (use first 3 chars for space). We plot all 23 states as thin grey, then highlight 5 focal states. Five focal states chosen for regional spread: Maharashtra (West), Tamil Nadu (South), Uttar Pradesh (North), West Bengal (East), Rajasthan (large poor state)

local c1 "navy"
local c2 "maroon"
local c3 "forest_green"
local c4 "purple"
local c5 "orange"
	
twoway ///
    (connected capex_share_total_exp year if state_name=="maharashtra",   ///
        lcolor(`c1')  mcolor(`c1')  msymbol(circle)    lwidth(medthick)) ///
    (connected capex_share_total_exp year if state_name=="tamil nadu",    ///
        lcolor(`c2')  mcolor(`c2')  msymbol(triangle)  lwidth(medthick)) ///
    (connected capex_share_total_exp year if state_name=="uttar pradesh", ///
        lcolor(`c3')  mcolor(`c3')  msymbol(square)    lwidth(medthick)) ///
    (connected capex_share_total_exp year if state_name=="west bengal",   ///
        lcolor(`c4')  mcolor(`c4')  msymbol(diamond)   lwidth(medthick)) ///
    (connected capex_share_total_exp year if state_name=="rajasthan",     ///
        lcolor(`c5')  mcolor(`c5')  msymbol(plus)      lwidth(medthick)), ///
    xlabel(2018(1)2023, labsize(small) angle(45))                        ///
    ylabel(, labsize(small))                                             ///
    ytitle("Capital Expenditure as % of Total Expenditure")              ///
    xtitle("Fiscal Year (starting year)")                                ///
    title("Capital Expenditure Share — Selected States", size(medsmall)) ///
    subtitle("2018–19 to 2023–24", size(small))                          ///
    legend(order(1 "Maharashtra" 2 "Tamil Nadu" 3 "Uttar Pradesh"        ///
                  4 "West Bengal" 5 "Rajasthan")                         ///
            position(6) rows(2) size(small))                             ///
    note("Source: PRS India & RBI Handbook of Indian Statistics", size(vsmall))

graph export "Analysis\graph_4_capex_share_states.png", replace width(1600)

scatter capex_share_total_exp ln_pcgsdp, ///
    mcolor(navy%70) msymbol(circle) ///
    ytitle("CapEx Share (%)") xtitle("Log Per-Capita GSDP (current Rs)") ///
    title("CapEx Share vs Development Level", size(medsmall)) ///
    note("Each dot = state-year observation. Source: PRS India & RBI.", size(vsmall))
graph export "Analysis\graph_5_capex_vs_pcgsdp.png", replace width(1400) // Cross-sectional correlation: capex share vs per-capita GSDP

** Fiscal Deficit and Debt Sustainability
tabstat fiscal_deficit_pct_gsdp, by(state_name) stat(mean sd min max) longstub format(%7.2f)

xtreg fiscal_deficit_pct_gsdp i.year ln_pcgsdp covid, fe vce(robust)
estimates store fe_fd

esttab fe_fd using "Analysis\Regression_Results_fiscal_deficit.rtf", replace star(* 0.10 ** 0.05 *** 0.01) b(%15.9f) se(%15.9f) compress nogaps label scalars("statefe State Fixed Effects") title("Fiscal Deficit") mtitle("Fiscal Deficit (% GSDP)") // Output results to a table

esttab fe_capex fe_capex_post using "Analysis\Regression_Results_fiscal_deficit.tex", replace star(* 0.10 ** 0.05 *** 0.01)  b(%15.9f) se(%15.9f) label scalars("statefe State Fixed Effects") stats(N r2_w, labels("Observations" "Within R²")) title("Fiscal Deficit") mtitle("Year-wise Effects" "Post-COVID Trend") booktabs

preserve // Debt sustainability flag: States whose average fiscal deficit/GSDP exceeds 4% is "concerning"
collapse (mean) avg_fd = fiscal_deficit_pct_gsdp ///
         (mean) avg_debt = total_debt_outstanding_percent, by(state_name)
gen concern = (avg_fd > 4)
list state_name avg_fd avg_debt concern, sep(0) clean
restore

local c1 "navy"
local c2 "maroon"
local c3 "forest_green"
local c4 "purple"
local c5 "orange"

* 2. The full graph block
twoway ///
    (connected fiscal_deficit_pct_gsdp year if state_name=="maharashtra", ///
        lcolor(`c1') mcolor(`c1') msymbol(circle) lwidth(medthick)) ///
    (connected fiscal_deficit_pct_gsdp year if state_name=="tamil nadu", ///
        lcolor(`c2') mcolor(`c2') msymbol(triangle) lwidth(medthick)) ///
    (connected fiscal_deficit_pct_gsdp year if state_name=="uttar pradesh", ///
        lcolor(`c3') mcolor(`c3') msymbol(square) lwidth(medthick)) ///
    (connected fiscal_deficit_pct_gsdp year if state_name=="west bengal", ///
        lcolor(`c4') mcolor(`c4') msymbol(diamond) lwidth(medthick)) ///
    (connected fiscal_deficit_pct_gsdp year if state_name=="rajasthan", ///
        lcolor(`c5') mcolor(`c5') msymbol(plus) lwidth(medthick)) ///
    (scatteri 3 2017.8 3 2023.2, recast(line) lcolor(gs8) lpattern(dash) lwidth(thin)), ///
    xlabel(2018(1)2023, labsize(small) angle(45)) ///
    ylabel(0(1)7, labsize(small)) ///
    ytitle("Fiscal Deficit as % of GSDP") ///
    xtitle("Fiscal Year (starting year)") ///
    title("Fiscal Deficit Trends — Selected States", size(medsmall)) ///
    subtitle("2018–19 to 2023–24 | Dashed line = 3% FRBM target", size(small)) ///
    legend(order(1 "Maharashtra" 2 "Tamil Nadu" 3 "Uttar Pradesh" ///
                 4 "West Bengal" 5 "Rajasthan") ///
           position(6) rows(2) size(small)) ///
    note("Source: PRS India & RBI Handbook of Indian Statistics", size(vsmall))

* 3. Export
graph export "Analysis\graph_6_fiscal_deficit_states.png", replace width(1600)
