''wfcreate(wf = open, page = annual) a 1995 2015
cd "C:\Users\rs\OneDrive - Aalborg Universitet\AAU\02 PhD\Thesis template\thesis_in_rmarkdown\Data\sfc-fi-fl\Danish_Model"
' open hamid_dataset.csv
' open forecasted_Series2.xlsx
open 2019-11-19-data.xlsx


'##############################
'PARAMETERS
'Create series: Crisis dummies
series d_1998 = @recode(@year = 1998,1,0) 'Series definition: Dummy variable
series d_200 = @recode(@year = 2007 or @year = 2008,1,0) 'Series definition: Dummy variable
series d_2004 = @recode(@year = 2004,1,0) 'Series definition: Dummy variable
series d_2006 = @recode(@year = 2006,1,0) 'Series definition: Dummy variable
series d_2007 = @recode(@year = 2007,1,0) 'Series definition: Dummy variable
series d_2008 = @recode(@year = 2008,1,0) 'Series definition: Dummy variable
series d_2009 = @recode(@year = 2009,1,0) 'Series definition: Dummy variable
series d_2010 = @recode(@year = 2010,1,0) 'Series definition: Dummy variable
series d_2011 = @recode(@year = 2011,1,0) 'Series definition: Dummy variable
series d_2014 = @recode(@year = 2014,1,0) 'Series definition: Dummy variable: High taxes: 2014
series d_2015 = @recode(@year = 2015,1,0) 'Series definition: Dummy variable
series d_2016 = @recode(@year = 2016,1,0) 'Series definition: Dummy variable


'##############################
'Create series: Lagged values (initial values - 1995)
series k_nf 'Series definition: Stock of Capital: NFC
series k_f 'Series definition: Stock of Capital: FC
series k_g 'Series definition: Stock of Capital: GOVT
series k_h 'Series definition: Stock of Capital: HH
series k_h_k = k_h / pk 'Series definition: Stock of Capital: HH: Real prices
series zz_i = @cumsum(zz1) 'Series definition: Index: House price: Imported from DST: HH: Index
series zz_index = zz_i / @elem(zz_i, "2010")  'Series definition: Index: House price: Mark-up pricing on construction costs: HH: Index
series tobin_q = zz_i / pk 'Series definition: Index: House price index: Tobin's Q = Ratio between the house price index and the construction cost of housing index: HH
'series zz = k_h / (k_h_K) 'Series definition: Index: House price index: HH: Index
series dep_h_k = dep_h / pk 'Series definition: Depreciation on houses: HH: Real prices
series k_h_cg = k_h - kcg_h 'Series definition: Capital Gains on Stock of Housing: HH
series kcg_h_k = kcg_h / pk 'Series definition: Capital Gains on Stock of Housing: HH: Real prices

'##############################
'Create series: Household income
series pir_h = d41_h_R + d42_h_R + d44_h_R  'Series definition: Property income received: HH
series pip_h = d41_h_P  'Series definition: Property income paid: HH
series y_h = w_h + b2_h + pir_h - pip_h + stra_h  'Series definition: Total income: Received: HH
series y_d_h = y_h - tax_h  'Series definition: Disposable Income: HH
series y_d_h_k = y_d_h / pc 'Series definition: Disposable Income: HH: Real prices

'##############################
'Create series: Financial stocks: HH
series eqa_h 'Series definition: Financial Assets: Equity assets: HH
series iba_h 'Series definition: Financial Assets: Interest bearing assets: HH
series iba_f_h_rv = ibl_h_rv 'Series definition: Financial Assets: Interest bearing assets: HH: Revaluations
series pena_h 'Series definition: Financial Assets: Pension assets: HH
series ibl_h 'Series definition: Financial Liabilities: Interest bearing liabilities: HH
series ibl_fi_h = alpha * (ibl_h) 'Series definition: Financial Liabilities: Interest bearing liabilities: Fixed rate: HH
series ibl_f_h_rv = iba_h_rv 'Series definition: Financial Liabilities: Interest bearing liabilities: HH: Revaluations
series ibl_fl_h = (1 - alpha) * (ibl_h) 'Series definition: Financial Liabilities: Interest bearing liabilities: Flexible rate: HH
series fnw_h = (iba_h + eqa_h + pena_h) - (ibl_h) 'Series definition: Financial net wealth: HH
series fnw_h_k = fnw_h / pc  'Series definition: Financial net wealth: HH: Real prices
series nw_h = fnw_h + k_h 'Series definition: Net Wealth: HH: Total
series nw_h_k = Nw_h / pc  'Series definition: Net Wealth: HH: Real prices

'##############################
'Create series: Financial stocks: FC
series eqa_f 'Series definition: Financial Assets: Equity assets: FC
series iba_f 'Series definition: Financial Assets: Interest bearing assets: FC
series iba_f_h = ibl_h 'Series definition: Financial Assets: Interest bearing assets: FC
series iba_f_h_tr = ibl_h_tr 'Series definition: Financial Assets: Interest bearing assets: FC: Transactions
series nib_f_rv = (iba_f_rv - ibl_f_rv) - (ibl_h_rv - iba_h_rv) 'Series definition: Net financial stock: Interest bearing: FC: Revaluations
series eql_f 'Series definition: Financial Liabilities: Equity liabilities: FC
series ibl_f 'Series definition: Financial Liabilities: Interest bearing liabilities: FC
series ibl_f_h = iba_h 'Series definition: Financial Liabilities: Interest bearing liabilities: FC
series ibl_f_h_tr = iba_h_tr 'Series definition: Financial Liabilities: Interest bearing liabilities: FC: Transactions
series penl_f 'Series definition: Financial Liabilities: Pension liabilities: FC
series neq_f = eqa_f - eql_f 'Series definition: Net financial stock: Equity: FC
series neq_f_tr = eqa_f_tr - eql_f_tr 'Series definition: Net financial stock: Equity: FC: Transactions
series neq_f_rv = eqa_f_rv - eql_f_rv 'Series definition: Net financial stock: Equity: FC: Revaluations
series nib_f = (iba_f - ibl_f) - (iba_f_h - ibl_f_h) 'Series definition: Net financial stock: Interest bearing: FC: Total
series nib_f_tr = (iba_f_tr - ibl_f_tr) - (iba_f_h_tr - ibl_f_h_tr) 'Series definition: Net financial stock: Interest bearing: FC: Transactions
'series r_ibl_h' = d41_h_P(+1) / ibl_h  'Series definition: Rate of Interest: Morgage lending: Hh: (Ratio of interest payments to interest bearing liabilites of households): HH

'##############################
'Create series: Financial stocks: GOVT
series eqa_g 'Series definition: Financial Assets: Equity assets: GOVT
series iba_g 'Series definition: Financial Assets: Interest bearing assets: GOVT
series ibl_g 'Series definition: Financial Liabilities: Interest bearing liabilities: GOVT
series nib_g = iba_g - ibl_g + eqa_g  'Series definition: Net financial stock: Interest bearing: GOVT
series nib_g_tr = iba_g_tr - ibl_g_tr + eqa_g_tr 'Series definition: Net financial stock: Interest bearing: GOVT: Transactions
series nib_g_rv = iba_g_rv - ibl_g_rv + eqa_g_rv 'Series definition: Net financial stock: Interest bearing: GOVT: Revaluations


'##############################
'Create series: Financial stocks: ROW
series eqa_row 'Series definition: Financial Assets: Equity assets: ROW
series iba_row 'Series definition: Financial Assets: Interest bearing assets: ROW
series pena_row 'Series definition: Financial Assets: Pension assets: ROW
series eql_row 'Series definition: Financial Liabilities: Equity liabilities: ROW
series ibl_row 'Series definition: Financial Liabilities: Interest bearing liabilities: ROW
'series ibl_row_rv1 = D(ibl_row) - ibl_row_tr 'Series definition: Financial Liabilities: Interest bearing liabilities: ROW
series penl_row 'Series definition: Financial Liabilities: Pension liabilities: ROW
series penl_f_tr = neq_f_tr - nl_f + iba_f_h_tr - ibl_f_h_tr + nib_f_tr  'Series definition: Financial Liabilities: Pension liabilities: ROW: Transactions
series neq_row = eqa_row - eql_row 'Series definition: Net financial stock: Equity: ROW
series neq_row_tr = eqa_row_tr - eql_row_tr 'Series definition: Net financial stock: Equity: ROW: Transactions
series neq_row_rv = eqa_row_rv - eql_row_rv 'Series definition: Net financial stock: Equity: ROW: Revaluations
series nib_row = iba_row - ibl_row 'Series definition: Net financial stock: Interest bearing: ROW
series nib_row_tr = iba_row_tr - ibl_row_tr 'Series definition: Net financial stock: Interest bearing: ROW: Transactions
series nib_row_rv = iba_row_rv - ibl_row_rv1 'Series definition: Net financial stock: Interest bearing: ROW: Revaluations
series npen_row = pena_row - penl_row 'Series definition: Net financial stock: Pension: ROW: Total
series npen_row_tr = pena_row_tr - penl_row_tr 'Series definition: Net financial stock: Pension: ROW: Transactions
series npen_row_rv = pena_row_rv - penl_row_rv 'Series definition: Net financial stock: Pension: ROW: Revaluations

'##############################
'Create series: Financial stocks: NFC
series eqa_nf 'Series definition: Financial Assets: Equity assets: NFC
series iba_nf 'Series definition: Financial Assets: Interest bearing assets: NFC
series eql_nf 'Series definition: Financial Liabilities: Equity liabilities: NFC
series ibl_nf 'Series definition: Financial Liabilities: Interest bearing liabilities: NFC
series neq_nf = eqa_nf - eql_nf + eqa_g 'Series definition: Net financial stock: Equity: NFC
series neq_nf_tr =  - (neq_f_tr + eqa_h_tr + neq_row_tr) 'Series definition: Net financial stock: Equity: NFC: Transactions
series neq_nf_rv = eqa_nf_rv - eql_nf_rv + eqa_g_rv 'Series definition: Net financial stock: Equity: NFC: Revaluations
series nib_nf = iba_nf - ibl_nf - eqa_g + 2  'Series definition: Net financial stock: Interest bearing: NFC
series nib_nf_tr = iba_nf_tr - ibl_nf_tr - eqa_g_tr 'Series definition: Net financial stock: Interest bearing: NFC: Transactions
series nib_nf_rv = iba_nf_rv - ibl_nf_rv - eqa_g_rv 'Series definition: Net financial stock: Interest bearing: NFC: Revaluations


'##############################
'Create series: Inflation and Price deflators

series pm 'Series definition: Price deflator: Imports
series px 'Series definition: Price deflator: Exports
series py 'Series definition: Price deflator: GDP
series pc 'Series definition: Price deflator: Consumption
series pk 'Series definition: Price deflator: Investment (excluding dwellings)
series pg 'Series definition: Price deflator: Government consumption
series pi = (0.78 * pk + 0.22 * ph)  'Series definition: Price deflator: Investment (weighted average of deflators for  housing(pk) and all other investment (pk)) . Proportion of dwellings in total investment is approx 0.22, and pi closely resembles the deflator for total investment reported by ameco.: NFC
series wi = (wage - wage(-1)) / wage(-1) 'Series definition: Inflation rate: Wage inflation
series infl = (pc - pc(-1)) / pc(-1) 'Series definition: Inflation rate: Price inflation: Rate
series infl_m = (pm - pm(-1)) / pm(-1) 'Series definition: Inflation rate: Price inflation: foreign: Rate

'##############################
'Create series: Labour force
series wage = w_h / N 'Series definition: Labour force: Wage rate: LABOUR: Unit price
series y = GDP 'Series definition: GDP: TOTAL
series nu = w_row / wage 'Series definition: Labour force: Employed persons: Danish nationals: Employed abroad: LABOUR
series nf = Nu + N 'Series definition: Labour force: Employed persons: Danish waged: LABOUR
series prod = gdp / Nf 'Series definition: Labour Force: Labour productivity: LABOUR
series un = LF - N 'Series definition: Labour force: Unemployed persons: LABOUR
series ur = UN / LF 'Series definition: Labour force: Unemployment rate: LABOUR
series y_k = gdp / py 'Series definition: GDP: TOTAL: Real prices
series y_d_k = y_d_h / py 'Series definition: Disposable Income: TOTAL: Real prices
series m_k = M / pm  'Series definition: Imports: ROW: Real prices
series x_k = x / px  'Series definition: Exports: ROW: Real prices
series y_f = w_nf + b2  'Series definition: GDP: TOTAL: Factor costs
series ws = w_nf / y_f  'Series definition: Wage share: LABOUR: Ratio
series ul_cost = ws * y_f / GDP  'Series definition: Labour force: Unit labour cost: LABOUR
series ulc = ul_cost / @elem(ul_cost, "2010")  'Series definition: Labour force: Unit labour cost: Price index: Index for the price deflator with reference to 2010 (all other price deflator indices are referenced to 2010): LABOUR: Index
series y_k_eu = GDP_EU  'Series definition: GDP of EU: 2010 reference levels (in Euros): EU

'##############################
'Create series: Investment and depreciation
series inv_nf_k = inv_nf / pk  'Series definition: Gross fixed capital formation: NFC: Real prices
series inv_g_k = inv_g / pk  'Series definition: Gross fixed capital formation: GOVT: Real prices
series inv_f_k = inv_f / pk  'Series definition: Gross fixed capital formation: FC: Real prices
'series inv_h_k = inv_h / ph_k  'Series definition: Gross fixed capital formation: HH: Real prices
series inv_h_k = inv_h / pk 'Series definition: Gross fixed capital formation: HH: Real prices
series i_k = inv_nf_k + inv_f_k + inv_g_k + inv_h_k  'Series definition: Gross fixed capital formation: TOTAL: Real prices
series i = inv_nf + inv_f + inv_g + inv_h  'Series definition: Gross fixed capital formation: NFC
series inv_nf_k = inv_nf / pk  'Series definition: Gross fixed capital formation: NFC: Real prices
series dep_nf_k = dep_nf / pk  'Series definition: Depreciation of fixed capital: NFC: Real prices
series dep_h_k = dep_h / pk  'Series definition: Depreciation of fixed capital: HH: Real prices
series test = d(k_nf) 'Series definition: Test variable: Stock of Capital (Change in): NFC
series ph_k = ph_k 'Series definition: Variable not used: House prices: HH
'series k_nf_k = k_nf - kcg_nf_SUM  'Series definition: Stock of Capital: NFC: Real prices
series k_nf_k = k_nf / pnfc_k 'Series definition: Stock of Capital: NFC: Real prices
'series k_h_k = Hh / ph_k 'Series definition: Stock of Capital: HH: Real prices
'series hh_k = Hh / ph_k 'Series definition: Stock of Capital: Hh: Real prices: HH: Real prices
series hfhf = k_h_k * ph_k 'Series definition: Test variable: Not used: HH
series p_cgk_nf = k_nf / k_nf_k 'Series definition: Capital gains: NFC: Index -  / @elem(knf / knf_k, "2010") : NFC

'##############################
'Create series: GNI, GDP and main components
series g_k = g / pg  'Series definition: Government expenditure: Real prices: GOVT: Real prices
series c_h_k = c_h / pc  'Series definition: Consumption: HH: Real prices
series y_h_k = y_h / pc 'Series definition: Total income: HH: Real prices
series s = c_h + i + g + x  'Series definition: Gross national income: TOTAL: Nominal prices
series s_k = c_h_k + i_k + g_k + x_k  'Series definition: Gross national income: TOTAL: Real prices
series private = s_k - g_k  'Series definition: Gross income: PvT: Real prices
series y_k = c_h_k + i_k + g_k + x_k - m_k 'Series definition: GDP: TOTAL: Real prices
series nx = x - m 'Series definition: Net Exports: ROW

'##############################
'Create series: Key ratios
series cu = y_k / k_nf_k 'Series definition: Capacity Utilisation (Real GNI  /  real capital stock): Ratio
series profit = 1 - ws 'Series definition: Profit share (Residual of wage share)
series lev =  - (neq_nf + nib_nf) / y 'Series definition: Leverage ratio: NFC: Funding liabilities to GDP: NFC: Ratio
series lev1 =  - (neq_nf + nib_nf) 'Series definition: Leverage ratio: NFC: Funding liabilities: NFC: Ratio
series top = (x_k + m_k) / y_k 'Series definition: Ratio of foreign to domestic trade: NFC

'##############################
'Create series: interest-rates
series r_eq_dk' = (d42_nf_P + d42_f_P) / (eql_nf(-1) + eql_f(-1))  'Series definition: Rate of return: Equities: Denmark
series r_eq_row' = (d42_row_P) / (eql_row(-1))  'Series definition: Rate of return: Equities: Foreign
series r_pen' = (d44_f_P) / (penl_f(-1)) 'Series definition: Rate of return: Pensions: HH
series r_n' = ( r_iba_f - r_ibl_f) / (iba_f - ibl_f) 'Series definition: Rate of return: Mean: FC
series r_iba_f' = d41_f_r(+1) / iba_f  'Series definition: Rate of interest: Interest bearing assets: FC
series r_ibl_f' = d41_f_p(+1) / ibl_f  'Series definition: Rate of interest: Interest bearing liabilities: FC
'series r_ibl_h' = d41_h_P(+1) / ibl_h  'Series definition: Rate of Interest: Morgage lending: Hh: (Ratio of interest payments to interest bearing liabilites of households): HH
'series r_ibl_fi_h = r_r_ibl_fi_h 'Series definition: Rate of Interest: Morgage lending: Fixed rate: HH
'series r_ibl_fl_h' = @recode(alpha=1,0, r_ibl_fl_h/ibl_fl_h) 'Series definition: Rate of Interest: Morgage lending: Flexible rate: HH
'series r_ibl_fl_h = r_r_ibl_fl_h 'Series definition: Rate of Interest: Morgage lending: Flexible rate: HH
series r_ibl_fi_h = r_ibl_h 'Series definition: Rate of Interest: Morgage lending: Fixed rate: HH
series r_ibl_fl_h = r_ibl_h 'Series definition: Rate of Interest: Morgage lending: Flexible rate: HH
series r_ibl_h_sens = alpha * r_r_ibl_fi_h + (1 - alpha) * r_r_ibl_fl_h 'Series definition: Interest paid: Morgage lending: Flexible rate: HH
series r_iba_h' = d41_h_R(+1) / iba_h  'Series definition: Rate of interest: Interest bearing assets (Ratio of interest payments received to interest bearing assets): HH

'##############################
'Create series: interest-rates
series pip_ibl_fi_h = ibl_fi_h * r_ibl_fi_h 'Series definition: Interest paid: Morgage lending: Fixed rate: HH
series pip_ibl_fl_h = ibl_fl_h * r_ibl_fl_h 'Series definition: Interest paid: Morgage lending: Flexible rate: HH
series pir_iba_fi_f_h = pip_ibl_fi_h 'Series definition: Interest Received: Morgage lending: Fixed rate: FC
series pir_iba_fl_f_h = pip_ibl_fl_h 'Series definition: Interest Received: Morgage lending: Flexible rate: FC

'Error-term interest
series r_g_error2 = 0 'Series definition: Property income error (d42_g_R - r_eqdk*eqa_g(-1)): GOVT

'series r_h_error1 = d41_h_R - r_iba_h(-1) * iba_h(-1) + (d41_h_P - r_ibl_h(-1) * ibl_h(-1))  'Series definition: Property income error: HH
'series r_nf_net_error1 = d41_nf_R - d41_nf_P - d42_g_R - r_n(-1) * nib_nf(-1) 'Series definition: Property income error: NFC: Net
'series r_g_net_error1 = d41_g_R - d41_g_P + d42_g_R - r_n(-1) * nib_g(-1) 'Series definition: Property income error: GOVT
'series r_row_net_error1 = (d41_row_R - d41_row_P) - (r_n(-1) * nib_row(-1)) 'Series definition: Property income error: ROW: Net
'series r_f_net_error1 = (d41_f_R - d41_f_P) - (r_n(-1) * nib_f(-1) + r_ibl_h(-1) * iba_f_h(-1) - r_iba_h(-1) * ibl_f_h(-1) ) 'Series definition: Property income error: FC: Net
'series r_h_error2 = d42_h_R - r_eq_dk * eqa_h(-1)  'Series definition: Property income error: HH
'series r_nf_net_error2 = d42_nf_R - d42_nf_P - r_eq_dk * neq_nf(-1) + d42_g_R  'Series definition: Property income error: NFC: Net
'series r_f_net_error2 = (d42_f_R - d42_f_P) - r_eq_dk * neq_f(-1)  'Series definition: Property income error: FC: Net
'series r_row_net_error2 = (d42_row_R - d42_row_P) - r_eq_dk * neq_row(-1)  'Series definition: Property income error: ROW: Net
'series r_h_error3 = d44_h_R - r_pen * pena_h(-1)  'Series definition: Property income error: HH
'series r_f_error3 = d44_f_P - r_pen * penl_f(-1)  'Series definition: Property income error: FC
'series r_row_net_error3 = (d44_row_R - d44_row_P) - r_pen * npen_row(-1)  'Series definition: Property income error: ROW: Net

series error_check1 = r_h_error1 + r_nf_net_error1 + r_g_net_error1 + r_f_net_error1 + r_row_net_error1 'Series definition: Error check
series error_check2 = r_h_error2 + r_nf_net_error2 + r_g_error2 + r_f_net_error2 + r_row_net_error2 'Series definition: Error check
series error_check3 = r_h_error3 + r_f_error3 + r_row_net_error3 'Series definition: Error check



'##############################
'Create series: Parameters
'series phi1 = sco_h / y_h 'Series definition: Social contributions to income: HH: Ratio
'series phi2 = pena_h_tr / sco_h 'Series definition: Pension transactions to social contributions: HH: Ratio
'series eqa_r_h = eqa_h_tr / (eql_nf_tr + eql_f_tr) 'Series definition: Financial Assets: Equity assets: HH
'series rho = eql_f / fl_f 'Series definition: Equity to liabilities ratio: FC: Ratio
'series delta = eqa_nf / fa_nf 'Series definition: Equity to assets ratio: NFC
'series beta = eqa_h / fa_h 'Series definition: Equity to assets ratio: HH

'##############################
'Eviews commands: Set sample size and coefficient parameter
'##############################
smpl 1995 2016
coef(300) beta


'##############################
'Estimate parameters and equations
'##############################
equation eq2.ls(cov = white) log(c_h_k) = beta(1) + beta(2) * log(y_d_h_k) + beta(3) * log(nw_h_k(-1)) + beta(4) * log(c_h_k(-1)) + beta(5) * @trend + beta(6) * d_2009 'Estimated equation: Log-linear: Consumption: Real prices
equation eq3.ls(cov = white) tax_h = beta(7) * y_h + beta(8) * d_2014  'Estimated equation: Tax: HH
equation eq4.ls(cov = white) tax_nf = beta(9) * gdp 'Estimated equation: Tax (+ p(10) * d_2009): NFC
equation eq5.ls(cov = white) tax_f = beta(11) * (d41_f_R - d41_f_P) + beta(12) * d_2007  'Estimated equation: Tax (+ p(13) * d_2006): FC
equation eq6.ls(cov = white) tax_row = beta(14) * w_row + beta(15) * pir_row 'Estimated equation: Tax: ROW
'equation eq8.ls(cov = white) log(m_k) = beta(20) + beta(21) * log(py(-1) / pm(-1)) + beta(22) * (log(c_h_k + i_k + x_k))  'Estimated equation: Log-linear: Imports: Real prices: ROW: Real prices
equation eq8.ls(cov = white) log(m_k) = beta(20) + beta(21) * log(py(-1) / pm(-1)) + beta(22) * (log(c_h_k(-1) + i_k(-1) + x_k(-1))) + beta(23) * d_2009  'Estimated equation: Log-linear: Imports: Real prices: (+ p(23)*d_2007): ROW: Real prices
equation eq9.ls(cov = white) log(x_k) = beta(24) + beta(25) * log(px(-1) / pm(-1)) + beta(26) * log(fee59)  'Estimated equation: Log-linear: Exports: ROW: Real prices
equation eq10.ls(cov = white) log(inv_nf_k) = beta(28) + beta(30) * log(y_k(-1)) - beta(31) * log(k_nf_k(-1))  'Estimated equation: Log-linear: Gross fixed capital formation (+ P(32)*d_2009): NFC: Real prices
equation eq11.ls(cov = white) log(px) = beta(33) + beta(34) * log(pm) + beta(35) * log(ulc(-1))  'Estimated equation: Log-linear: Prices: Exports: ROW: Rate
'equation eq12.ls(cov = white) log(inv_h_k) = beta(36) * ph_k + beta(37) * log(hh_k(-1)) + beta(38) * log(y_d_h_k(-1)) + beta(39) * log(inv_h_k(-1)) 'Estimated equation: Log-linear: Gross fixed capital formation: HH: Real prices
equation eq12.ls(cov = white) log(inv_h_k) = beta(36) * log(inv_h_k(-3)) + beta(37) * log(ph_k) + beta(38) * log(ph_k(-1)) + beta(39) * log(y_d_h_k) + beta(40) * log(y_d_h_k(-2)) + beta(41) + beta(42) * @trend 'Estimated equation: Log-linear: Gross fixed capital formation: HH: Real prices
equation eq13.ls(cov = white) log(sbe_h) = beta(43) + beta(44) * log(un) + beta(45) * log(wage) + beta(46) * @trend  'Estimated equation: Log-linear: Social benefit transfers: HH
equation eq14.ls(cov = white) log(n) = beta(48) + beta(49) * log(y_k(-1)) + beta(50) * log(lf) + beta(51) * @trend 'Estimated equation: Log-linear: Population: Total: LABOUR
equation eq15.ls(cov = white) log(sbe_g) = beta(52) + beta(53) * log(un) + beta(54) * log(wage) + beta(55) * @trend  'Estimated equation: Log-linear: Social benefit transfers: GOVT
equation eq16.ls(cov = white) wi = beta(57) + beta(58) * ur + beta(59) * d_2011 + beta(60) * @trend  'Estimated equation: Inflation rate: Wage inflation: TOTAL
equation eq17.ls(cov = white) b2 = beta(62) + beta(63) * y + beta(64) * @trend + beta(65) * d_2009 'Estimated equation: Gross operating surplus: National: NFC
equation eq19.ls(cov = white) pena_h = beta(68) * y_h + beta(69) * pena_h(-1) + beta(70)  'Estimated equation: Financial Assets: Pension assets: HH
equation eq20.ls(cov = white) log(pena_h) = beta(71) * log(y_h) + beta(72) * log(nw_h) + beta(73) * @trend 'Estimated equation: Log-linear: Financial Assets: Pension assets: HH
equation eq21.ls(cov = white) log(pena_h_tr) = beta(74) + beta(75) * r_pen + beta(76) * log(cpen_h) + beta(77) * log(fa_h(-1)) 'Estimated equation: Log-linear: Financial Assets: Pension assets: HH: Transactions
equation eq22.ls(cov = white) ibl_h_tr = beta(79) * inv_h + beta(80) * ibl_h(-1) + beta(81) * fa_h_tr + beta(82) * r_ibl_fl_h(-1) 'Estimated equation: Financial Liabilities: Interest bearing liabilities: HH: Transactions
equation eq23.ls(cov = white) iba_f_tr = beta(83) * ( ibl_h_tr) + beta(84) * d_2015 + beta(85) * d_200 + beta(86) * (y - y(-1)) 'Estimated equation: Financial Assets: Interest bearing assets: FC: Transactions
equation eq27.ls(cov = white) eqa_h_tr = beta(96) * r_eq_dk + beta(97) * R_IBA_H(-1) + beta(98) * IBL_H_TR + beta(99) * D_2007 + beta(100) * D_2010 'Estimated equation: Financial Assets: Equity assets: HH: Transactions
equation eq24.ls(cov = white) log(eql_nf) = beta(90) * @trend + beta(92) * log(inv_nf) + beta(93) * eql_nf_rv 'Estimated equation: Log-linear: Financial Liabilities: Equity liabilities: NFC: Log-linear
equation eq25.ls(cov = white) eqa_nf_tr = beta(91) * eql_nf_tr 'Estimated equation: Financial Assets: Equity assets: NFC: Transactions
equation eq26.ls(cov = white) pc = beta(94) * wage + beta(95) * pm + pc(-1) 'Estimated equation: Price deflator: Consumption
'equation eq27.ls(cov = white) eqa_h_tr = c(1) * eqa_h_tr(-1) + c(2) * r_eq_dk + c(3) * r_eq_dk(-1) + c(4) * r_eq_dk(-2) + c(6) * r_iba_h(-1) + c(7) * r_iba_h(-2) + c(8) * r_iba_h(-3) + c(9) + c(10) * d_2016 'Estimated equation: Financial Assets: Equity assets: HH: Transactions
'equation eq28.ls(cov = white) wage = beta(87) + beta(88) * wage(-1) + beta(89) * ur 'Estimated equation: Wages: Change in


'##############################
'Eviews Commands: Initiate the model
'##############################

model model

'##############################
'Add model equations to the model
'##############################

'##############################
'GDP, wage share and Unit labour costs
model.append y_f = w_nf + b2  'Model equation: GDP: TOTAL: Factor costs
model.append ws = w_nf / y_f  'Model equation: Labour force: Wage share: LABOUR: Ratio
model.append ul_cost = ws * y_f / y  'Model equation: Labour force: Unit labour cost: LABOUR: Log-linear
model.append ulc = ul_cost / @elem(ul_cost, "2010")  'Model equation: Labour force: Unit labour cost: Price index: Index for the price deflator with reference to 2010 (all other price deflator indices are referenced to 2010): LABOUR

'##############################
'NON-FINANCIAL SECTOR
'##############################

'##############################
'NFC: Real side

model.append y = c_h + g + i + x - m  'Model equation: GDP: TOTAL
model.append y_k = c_h_k + i_k + g_k + x_k - m_k 'Model equation: GDP: TOTAL: Real prices
model.append s_k = c_h_k + i_k + g_k + x_k 'Model equation: Gross National Income: TOTAL
model.append g_k = g / pg 'Model equation: Government expenditure: Real prices: NFC
model.append x = x_k * px 'Model equation: Exports: NFC
model.append m = m_k * pm 'Model equation: Imports: NFC
model.append py = y / y_k  'Model equation: Price deflator: GDP
model.append s = c_h + g + i + x 'Model equation: Gross National Income: TOTAL: Nominal prices
model.append w_nf = wage * Nf  'Model equation: Wages: NFC: Nominal prices
model.append b2_nf = b2 - (b2_h + b2_f + b2_g)  'Model equation: Gross operating surplus (B2): NFC
model.append b2 = beta(62) + beta(63) * y + beta(64) * @trend + beta(65) * d_2009 'Model equation: Estimated equation: Gross operating surplus: National: NFC
model.append i_k = inv_nf_k + inv_f_k + inv_g_k + inv_h_k 'Model equation: Gross fixed capital formation: NFC
model.append k_nf_k = k_nf / pnfc_k  'Model equation: Stock of Capital: NFC: Real prices
model.append k_nf = k_nf(-1) + inv_nf - dep_nf + kcg_nf  'Model equation: Stock of Capital: NFC
model.append i = inv_nf + inv_f + inv_g + inv_h 'Model equation: Gross fixed capital formation: NFC
model.append inv_nf = inv_nf_k * pk 'Model equation: Gross fixed capital formation: NFC
model.append cu = y_k / k_nf_k 'Model equation: Capacity Utilisation (Real GNI  /  real capital stock): Ratio
model.append inv_nf_k = exp(log(inv_nf_k(-1)) - 0.414271821219 * d(log(inv_nf_k(-1))) + 3.23125209925 * d(log(cu(-1))) - 0.254777552329 * d_2009 + 0.0397927288877) 'Model equation: Gross fixed capital formation: NFC: Real prices

'model.append i = i_k * pi  'Model equation: Gross fixed capital formation: NFC
'model.append pi = (0.78 * pk + 0.22 * ph)  'Model equation: Price deflator: Investment (weighted average of deflators for  housing(pk) and all other investment (pk)) . Proportion of dwellings in total investment is approx 0.22, and pi closely resembles the deflator for total investment reported by ameco.: NFC
'model.append inv_nf_k = exp(beta(28) + beta(30) * log(y_k(-1)) - beta(31) * log(k_nf_k(-1)) ) 'Model equation: Gross fixed capital formation: NFC: Real prices
'eviews command: d(log(inv_nf_k)) c d(log(inv_nf_k(-1))) d(log(cu(-1))) d_2009 = : NFC
'model.append top = (x_k + m_k) / y_k 'Model equation: Ratio of foreign to domestic trade: NFC
'model.append inv_nf_k = exp(log(inv_nf_k(-1)) - 0.389028227773 * D(LOG(inv_nf_K(-1))) + 3.04135584243 * D(LOG(CU(-1))) + 1.25041146348 * D(LOG(TOP)) - 0.00461898829747) 'Model equation: Gross fixed capital formation: NFC: Real prices

model.append tax_nf = beta(9) * y  'Model equation: Tax (+ p(10) * d_2009): NFC
model.append s_nf = y - w_nf + b2_nf - b2 + r_n(-1) * nib_nf(-1) + r_eq_dk * neq_nf(-1) - tax_nf + stra_nf + r_nf_net_error1 + r_nf_net_error2 'Model equation: Savings: NFC
model.append ctr_nf =  - (ctr_h + ctr_f + ctr_g + ctr_row) 'Model equation: Capital transfers: NFC
model.append np_nf =  - (np_h + np_f + np_g + np_row) 'Model equation: Net purchases of non-financial assets (NP): NFC
model.append nl_nf = s_nf - inv_nf - np_nf + ctr_nf  'Model equation: Sector Balance: NFC

'##############################
'NFC: Returns on financial assets
model.append npir_nib_nf = r_n(-1) * nib_nf(-1) 'Model equation: Net property income received: NIB: NFC: NFC
model.append npir_neq_nf = r_eq_dk * neq_nf(-1) 'Model equation: Net property income received: NEQ: NFC: NFC
model.append npir_nf = npir_nib_nf + npir_neq_nf  'Model equation: Net property income paid: NFC: NFC

'##############################
'NFC: Financial side
model.append fnl_nf = nib_nf_tr + neq_nf_tr  'Model equation: Financial Net Lending (Balance): NFC

'##############################
'NFC: Change in stocks
model.append nib_nf = nib_nf(-1) + nib_nf_tr + nib_nf_rv 'Model equation: Net financial stock: Interest bearing: NFC
model.append nib_nf_tr = nl_nf - neq_nf_tr 'Model equation: Net financial stock: Interest bearing: NFC: Transactions
model.append neq_nf = neq_nf(-1) + neq_nf_tr + neq_nf_rv 'Model equation: Net financial stock: Equity: NFC
model.append neq_nf_rv =  - (eqa_h_rv + neq_row_rv + neq_f_rv) 'Model equation: Net financial stock: Equity: NFC: Revaluations

'model.append neq_nf =  - (neq_f + eqa_h + neq_row) 'Model equation: Net financial stock: Equity: NFC
'model.append neq_nf_tr =  - (neq_f_tr + eqa_h_tr + neq_row_tr) 'Model equation: Net financial stock: Equity: NFC: Transactions
'model.append neq_nf_tr = D(neq_nf) - neq_nf_rv 'Model equation: Net financial stock: Equity: NFC: Transactions

'##############################
'NFC: Net wealth and net financial wealth
model.append nw_nf = fnw_nf + k_nf  'Model equation: Net Wealth: NFC: Real prices
model.append fnw_nf = (nib_nf + neq_nf) 'Model equation: Financial net wealth: NFC


'##############################
'HOUSEHOLD
'##############################

'##############################
'HH: Real side

model.append y_h = w_h + b2_h + r_iba_h(-1) * iba_h(-1) + r_eq_dk * eqa_h(-1) + r_pen * pena_h(-1) - sco_h + sbe_h + otr_h + r_h_error2 + r_h_error3 + r_h_error1 - pip_ibl_fi_h(-1)  - pip_ibl_fl_h(-1)  'Model equation: Total income: Received: HH
'model.append y_h = w_h + b2_h + r_iba_h(-1) * iba_h(-1) + r_eq_dk * eqa_h(-1) + r_pen * pena_h(-1) - sco_h + sbe_h + otr_h + r_h_error2 + r_h_error3 + r_h_error1 - pip_ibl_fi_h(-1)  - pip_ibl_fl_h(-1)  'Model equation: Total income: Received: HH
'model.append y_h = w_h + b2_h + r_iba_h(-1)*iba_h(-1) + r_eq_dk*eqa_h(-1) + r_pen*pena_h(-1) - sco_h + sbe_h + otr_h + r_h_error2 + r_h_error3 + r_h_error1 - r_ibl_h(-1)*ibl_h(-1) 'Model equation: Total income: Received: HH
model.append ibl_h_tr = beta(79) * inv_h + beta(80) * ibl_h(-1) + beta(81) * fa_h_tr + beta(82) * r_ibl_fl_h(-1) 'Model equation: Financial Liabilities: Interest bearing liabilities: HH: Transactions
model.append sbe_h = exp( 0.59 * log(sbe_h(-1)) + 0.06 * log(un) + 0.88 * log(wage(-1)) - 0.01 * @trend) 'Model equation: Social benefit transfers: HH
model.append y_d_h = y_h - tax_h  'Model equation: Disposable Income: HH
model.append y_d_h_k = y_d_h / pc 'Model equation: Disposable Income: HH: Real prices
model.append tax_h = beta(7) * y_h + beta(8) * d_2014 'Model equation: Tax: HH
model.append c_h_k = exp(log(c_h_k(-1)) + 0.233173694484 * D(LOG(c_h_k(-1))) + 0.513151597559 * D(LOG(y_d_h_k)) + 0.387107648882 * D(LOG(y_d_h_k(-2))) + 0.0964585931719 * D(LOG(NW_h_k(-1))) - 0.00700214328644) 'Model equation: Consumption: HH: Real prices
model.append c_h = c_h_k * pc 'Model equation: Consumption: HH: Nominal prices
model.append pc = exp(log(pc(-1)) + 0.462120225875 * D(LOG(PC(-1))) + 0.269934560083 * D(LOG(WAGE(-2))) + 0.10 * D(LOG(PM)) + 0.00819246251177 * D_2008 - 0.000669904612675) 'Model equation: Price deflator: Consumption
model.append inv_h = inv_h_k * pk 'Model equation: Gross fixed capital formation: HH
model.append inv_h_k = exp(log(inv_h_k(-1)) - 0.36280487765 * d(log(inv_h_k(-1))) + 2.69627020709 * d(log(y_d_h_k)) + 3.19495217118 * d(log(y_d_h_k(-2))) + 2.54879216558 * d(tobin_q) + 1.91648377822 * d(tobin_q(-1)) - 0.115869704654) 'Model equation: Gross fixed capital formation: HH: Real prices
model.append tobin_q = zz_i / pk 'Model equation: Index: House price index: Tobin's Q = Ratio between the house price index and the construction cost of housing index: HH
model.append zz1 = d(zz_i) 'Model equation: Index: House price: Imported from DST: HH: Differenced index
model.append dep_h = dep_h_k * pk 'Model equation: Depreciation on houses: HH: Nominal prices
model.append k_h = k_h(-1) * (1 + zz1) + inv_h - dep_h 'Model equation: Stock of Capital: HH
model.append k_h_k = k_h / pk 'Model equation: Stock of Capital: HH: Real prices
genr kcg_h_sum = @cumsum( kcg_h) 'cumulative sum of real capital gains for households fixed capital ': 'cumulative sum of real capital gains for fhouseholds fixed capital: HH
genr p_cgk_h = kcg_h_sum / @elem(kcg_h_sum, "2010") 'index of real capital gains for firms capital ': 'index of real capital gains for firms capital: HH
model.append sco_h = phi1 * y_h(-1) 'Model equation: Social benefit contributions: HH
model.append stra_h = sbe_h + otr_h - sco_h 'Model equation: Transfers: HH
model.append s_h = y_d_h - c_h + cpen_h  'Model equation: Savings: HH
model.append nl_h = s_h - inv_h - np_h + ctr_h  'Model equation: Sector Balance: HH

'model.append b2_h = b2_h 'Model equation: Gross operating surplus: HH
'model.append sbe_h = exp( beta(43) + beta(44) * log(un) + beta(45) * log(wage) + beta(46) * @trend ) 'Model equation: Social benefit transfers: HH
'model.append c_h_k = exp(beta(1) + beta(2) * log(y_d_h_k) + beta(3) * log(nw_h_k(-1)) + beta(4) * log(c_h_k(-1)) + beta(5) * @trend + beta(6) * d_2009) 'Model equation: Consumption: HH: Real prices
'estimation command: d(log(c_h_k)) c d(log(c_h_k(-1))) d(log(y_d_h_k)) d(log(y_d_h_k(-2))) d(log(nw_h_k(-1))): HH
'model.append pc = beta(94) * wage + beta(95) * pm + pc(-1) 'Model equation: Price deflator: Consumption
'model.append pc = exp(log(pc(-1)) + 0.495239524623 * D(LOG(PC(-1))) + 0.299870186224 * D(LOG(PY)) + 0.0823074275464 * D(LOG(PM)) + 0.00204823395223) 'Model equation: Price deflator: Consumption
'model.append pc = exp(log(pc(-1)) + 0.446945211915 * d(log(pc(-1))) + 0.268948446621 * d(log(wage(-2))) + 0.0934277596321 * d(log(pm)) - 0.0411256164375 * d(log(pm(-3))) + 0.00939888438739 * d_2008 + 0.00020309409238) 'Model equation: Price deflator: Consumption
'model.append inv_h = inv_h_k * ph_k 'Model equation: Gross fixed capital formation: HH
'model.append inv_h_k = exp( beta(36) * log(inv_h_k(-3)) + beta(37) * log(ph_k) + beta(38) * log(ph_k(-1)) + beta(39) * log(y_d_h_k) + beta(40) * log(y_d_h_k(-2)) + beta(41) + beta(42) * @trend) 'Model equation: Gross fixed capital formation: HH: Real prices
'model.append inv_h_k = exp(log(inv_h_k(-1)) - 0.329072721748 * d(log(inv_h_k(-1))) + 2.39914135074 * zz + 1.61625020131 * zz(-1) + 2.66853866006 * d(log(y_d_h_k)) + 2.93177620953 * d(log(y_d_h_k(-2))) - 4.12657872692) 'Model equation: Gross fixed capital formation: HH: Real prices
'model.append inv_h_k = exp(log(inv_h_k(-1)) - 0.333139783457 * D(LOG(inv_h_K(-1))) + 2.67221425603 * D(LOG(y_d_h_k)) + 2.92565274164 * D(LOG(y_d_h_k(-2))) + 2.37210999078 * ZZ1 + 1.59792143582 * ZZ1(-1) - 0.111676356667) 'Model equation: Gross fixed capital formation: HH: Real prices
'model.append inv_h_k = exp(log(inv_h_k(-1)) - 0.268270922701 * d(log(inv_h_k(-1))) + 2.36269074136 * d(log(y_d_h_k)) + 2.56003647823 * d(log(y_d_h_k(-2))) + 1.07514977424e - 06 * kcg_h + 6.4234388429e - 07 * kcg_h(-1) - 0.0889747724964) 'Model equation: Gross fixed capital formation: HH: Real prices
'eviews command: d(log(inv_h_k)) c d(log(inv_h_k(-1))) zz zz(-1) d(log(y_d_h_k)) d(log(y_d_h_k(-2))): HH
'model.append dep_h_k = dep_h / pk 'Model equation: Depreciation of fixed capital: HH: Real prices
'model.append kcg_h_k = kcg_h / pk 'Model equation: Capital Gains on Stock of Housing: HH: Real prices
'model.append k_h_k = k_h_k(-1) + inv_h_k - dep_h_k + kcg_h_k 'Model equation: Stock of Capital: HH: Real prices
'model.append k_h = k_h_k * pk 'Model equation: Stock of Capital: HH
'model.append k_h_cg = k_h(-1) + inv_h - dep_h 'Model equation: Capital Gains on Stock of Housing: HH
'model.append k_h_k = k_h_cg / pk 'Model equation: Stock of Capital: HH: Real prices
'model.append k_h = zz * k_h_k * pk 'Model equation: Stock of Capital: HH

'##############################
'HH: Financial side

model.append fnl_h = (fa_h_tr - fl_h_tr)  'Model equation: Financial Net Lending (Balance): HH
model.append fa_h_tr = iba_h_tr + eqa_h_tr + pena_h_tr 'Model equation: Financial Assets: HH: Transactions
model.append iba_h_tr = nl_h + ibl_h_tr - eqa_h_tr - pena_h_tr 'Model equation: Financial Assets: Interest bearing assets: HH: Transactions
'model.append pena_h_tr = exp(4.66344599453 + 1.12141270492 * LOG(R_PEN) + 0.88730632867 * LOG(cpen_h) - 0.000000611626943812 * D( LOG(FA_H(-1))) ) 'Model equation: Financial Assets: Pension assets: HH: Transactions
'model.append pena_h_tr = exp(beta(74) + beta(75) * r_pen + beta(76) * log(cpen_h) + beta(77) * log(fa_h(-1))) 'Model equation: Financial Assets: Pension assets: HH: Transactions
'model.append pena_h_tr = exp(-0.372398982121 * log(pena_h_tr(-2)) + 0.664464607179 * log(w_h) + 1.63004423686 * log(r_pen) + 0.799504294612 * log(cpen_h) + 0.510843635559 * log(cpen_h(-2)) - 3.38040096983 - 0.190587645859 * d_2008) 'Model equation: Financial Assets: Pension assets: HH: Transactions
model.append pena_h_tr = 0.248689563511 * pena_h_tr(-1) + 0.163031646201 * w_h + 2714501.58026 * r_pen - 212033.200006 'Model equation: Financial Assets: Pension assets: HH: Transactions
model.append fl_h_tr = ibl_h_tr 'Model equation: Financial liabilities: HH: Transactions
model.append iba_h = iba_h(-1) + iba_h_tr + iba_h_rv 'Model equation: Financial Assets: Interest bearing assets: HH
model.append eqa_h = eqa_h(-1) + eqa_h_tr + eqa_h_rv 'Model equation: Financial Assets: Equity assets: HH
'model.append eqa_h_tr = c(1) * eqa_h_tr(-1) + c(2) * r_eq_dk + c(3) * r_eq_dk(-1) + c(4) * r_eq_dk(-2) + c(6) * r_iba_h(-1) + c(7) * r_iba_h(-2) + c(8) * r_iba_h(-3) + c(9) + c(10) * d_2016 'Model equation: Financial Assets: Equity assets: HH: Transactions
'model.append eqa_h_tr = 427062.810696 * r_eq_dk - 581223.16801 * R_IBA_H(-1) + 0.235711420148 * IBL_H_TR - 59072.6981175 * D_2007 - 64431.2275666 * D_2010 'Model equation: Financial Assets: Equity assets: HH: Transactions
model.append eqa_h_tr = beta(96) * r_eq_dk + beta(97) * R_IBA_H(-1) + beta(98) * IBL_H_TR + beta(99) * D_2007 + beta(100) * D_2010 'Model equation: Financial Assets: Equity assets: HH: Transactions
model.append pena_h = pena_h(-1) + pena_h_tr + pena_h_rv 'Model equation: Financial Assets: Pension assets: HH
model.append ibl_h = ibl_h(-1) + ibl_h_tr + ibl_h_rv 'Model equation: Financial Liabilities: Interest bearing liabilities: HH
model.append ibl_fi_h = alpha * (ibl_h) 'Model equation: Financial Liabilities: Interest bearing liabilities: Fixed rate: HH
model.append ibl_fl_h = (1 - alpha) * (ibl_h) 'Model equation: Financial Liabilities: Interest bearing liabilities: Flexible rate: HH
'model.append r_ibl_h = alpha * r_r_ibl_fi_h + (1 - alpha) * r_r_ibl_fl_h 'Model equation: Rate of Interest: Morgage lending: Hh: (Ratio of interest payments to interest bearing liabilites of households): HH
'model.append r_ibl_fi_h = r_ibl_h 'Model equation: Rate of Interest: Morgage lending: Fixed rate: HH
'model.append r_ibl_fl_h = r_ibl_fi_h 'Model equation: Rate of Interest: Morgage lending: Flexible rate: HH
model.append pip_ibl_fi_h = ibl_fi_h * r_ibl_fi_h 'Model equation: Property income paid: Morgage lending: Fixed rate: HH
model.append pip_ibl_fl_h = ibl_fl_h * r_ibl_fl_h 'Model equation: Property income paid: Morgage lending: Flexible rate: HH
model.append r_ibl_h_sens = alpha * r_r_ibl_fi_h + (1 - alpha) * r_r_ibl_fl_h 'Model equation: Interest rate: Mortgage debt: Proportionally weighted interest rate: HH
model.append fa_h = iba_h + eqa_h + pena_h 'Model equation: Financial Assets: HH

'##############################
'HH: Returns on financial assets
model.append pir_iba_h = iba_h * r_iba_h 'Model equation: Property income received: IBA: HH: HH
model.append pir_eqa_h = r_eq_dk * eqa_h(-1) 'Model equation: Property income received: EQA: HH: HH
model.append pir_pena_h = r_pen * pena_h(-1) 'Model equation: Property income received: PENA: HH: HH
model.append pip_ibl_h = pip_ibl_fi_h + pip_ibl_fl_h 'Model equation: Property income paid: Morgage lending: All: HH
model.append npir_h = pir_iba_h + pir_eqa_h + pir_pena_h - pip_ibl_h 'Model equation: Net property income paid: HH: HH

'##############################
'Housing market

'##############################
'HH: Net wealth and net financial wealth

model.append fnw_h = (iba_h + eqa_h + pena_h) - (ibl_h)  'Model equation: Financial net wealth: HH
model.append fnw_h_k = fnw_h / pc 'Model equation: Financial net wealth: HH
model.append nw_h = fnw_h + k_h 'Model equation: Net Wealth: HH: Total
model.append nw_h_k = nw_h / pc  'Model equation: Net Wealth: HH: Real prices


'##############################
'FINANCIAL CORPORATE SECTOR
'##############################

'##############################
'THIS SECTOR IS COMPLETELY EXOGENOUS AT THE MOMENT

'##############################
'FC: Real side

'model.append pir_f = d41_f_R + d42_f_R  'Model equation: Property income received: FC
'model.append pip_f = d41_f_P + d42_f_P + d44_f_P 'Model equation: Property income paid: FC
'model.append s_f = b2_f - r_iba_h(-1) * ibl_f_h(-1) + r_n(-1) * nib_f(-1) + r_eq_dk * neq_f(-1) - r_pen * penl_f(-1) - tax_f + stra_f - cpen_f + r_f_net_error1 + r_f_net_error2 + r_ibl_h(-1) * iba_f_h(-1) 'Model equation: Savings: FC
model.append s_f = b2_f - r_iba_h(-1) * ibl_f_h(-1) + r_n(-1) * nib_f(-1) + r_eq_dk * neq_f(-1) - r_pen * penl_f(-1) - tax_f + stra_f - cpen_f + r_f_net_error1 + r_f_net_error2 + pir_iba_fi_f_h(-1) + pir_iba_fl_f_h(-1) 'Model equation: Savings: FC
model.append k_f = k_f(-1) + inv_f - dep_f + kcg_f 'Model equation: Stock of Capital: FC
model.append nl_f = s_f - inv_f - np_f + ctr_f  'Model equation: Sector Balance: FC

'##############################
'FC: Financial side

model.append fnl_f = nib_f_tr + neq_f_tr - penl_f_tr + iba_f_h_tr - ibl_f_h_tr  'Model equation: Financial Net Lending (Balance): FC

'##############################
'FC: Change in stocks

model.append cpen_f = cpen_h 'Model equation: Financial Liabilities: Change in pension entitlements: FC
model.append iba_f_h_rv = ibl_h_rv 'Model equation: Financial Assets: Interest bearing assets: FC: Revaluations
model.append iba_f_h_tr = ibl_h_tr 'Model equation: Financial Assets: Interest bearing assets: FC: Transactions
model.append ibl_f_h = ibl_f_h(-1) + ibl_f_h_tr + ibl_f_h_rv 'Model equation: Financial Liabilities: Interest bearing liabilities: FC
model.append iba_f_h = iba_f_h(-1) + iba_f_h_tr + iba_f_h_rv 'Model equation: Financial Assets: Interest bearing assets: FC
model.append pir_iba_fi_f_h = pip_ibl_fi_h 'Model equation: Interest Received: Morgage lending: Fixed rate: FC
model.append pir_iba_fl_f_h = pip_ibl_fl_h 'Model equation: Interest Received: Morgage lending: Flexible rate: FC
model.append ibl_f_h_rv = iba_h_rv 'Model equation: Financial Liabilities: Interest bearing liabilities: FC: Revaluations
model.append ibl_f_h_tr = iba_h_tr  'Model equation: Financial Liabilities: Interest bearing liabilities: FC: Transactions
model.append nib_f_tr =  - (nib_nf_tr + nib_g_tr + nib_row_tr) 'Model equation: Net financial stock: Interest bearing: FC: Transactions
model.append nib_f = nib_f(-1) + nib_f_tr + nib_f_rv 'Model equation: Net financial stock: Interest bearing: FC
model.append nib_f_rv =  - (nib_g_rv + nib_row_rv + nib_nf_rv) 'Model equation: Net financial stock: Interest bearing: FC: Revaluations
model.append neq_f_tr = nl_f - iba_f_h_tr + ibl_f_h_tr - nib_f_tr + penl_f_tr 'Model equation: Net financial stock: Equity: FC: Transactions
model.append neq_f = neq_f(-1) + neq_f_tr + neq_f_rv 'Model equation: Net financial stock: Equity: FC
model.append penl_f = penl_f(-1) + penl_f_tr + penl_f_rv 'Model equation: Financial Liabilities: Pension liabilities: FC
model.append penl_f_rv = pena_h_rv + npen_row_rv 'Model equation: Financial Liabilities: Pension liabilities: FC: Revaluations
model.append penl_f_tr = pena_h_tr + npen_row_tr 'Model equation: Financial Liabilities: Pension liabilities: FC: Transactions

'##############################
'FC: Returns on financial assets
model.append npir_nib_f = r_n(-1) * nib_f(-1) 'Model equation: Net property income received: NIB: FC: FC
model.append npir_neq_f = r_eq_dk * neq_f(-1) 'Model equation: Net property income received: NEQ: FC: FC
model.append pip_penl_f = r_pen * penl_f(-1) 'Model equation: Property income paid: PENL: FC: FC
model.append pip_ibl_f = r_iba_h(-1) * ibl_f_h(-1) 'Model equation: Property income paid: IBL: FC: FC
model.append npir_f = npir_nib_f + npir_neq_f  + pir_iba_fi_f_h + pir_iba_fl_f_h - pip_ibl_f - pip_penl_f 'Model equation: Net property income paid: FC: FC

'##############################
'FC: Net wealth and net financial wealth

'model.append fnw_f = (iba_f + eqa_f) - (ibl_f + eql_f + penl_f)  'Model equation: Financial net wealth: FC
model.append fnw_f = (nib_f + neq_f + iba_f_h) - (ibl_f_h + penl_f) 'Model equation: Financial net wealth: FC
model.append nw_f = fnw_f + k_f  'Model equation: Net Wealth: FC: Total


'##############################
'Government
'##############################

'##############################
'GOVT: Real side

model.append tax_g = Tax_nf + Tax_f + Tax_h + Tax_row 'Model equation: Tax: GOVT
model.append stra_g =  - (stra_nf + stra_f + stra_h + stra_row) 'Model equation: Transfers: GOVT
model.append s_g = b2_g + r_n(-1) * nib_g(-1) + tax_g + stra_g - g + r_g_net_error1  'Model equation: Savings (+ r_g_error2): GOVT
'model.append sbe_g = exp( beta(52) + beta(53) * log(un) + beta(54) * log(wage) + beta(55) * @trend ) 'Model equation: Social benefit transfers: GOVT
'model.append dep_g = beta(66) * k_g 'Model equation: Depreciation of fixed capital: GOVT: GOVT
'model.append b2_g = dep_g 'Model equation: Gross operating surplus: GOVT
model.append k_g = k_g(-1) + inv_g - dep_g + kcg_g 'Model equation: Stock of Capital: GOVT
model.append nl_g = s_g - inv_g - np_g + ctr_g  'Model equation: Sector Balance: GOVT


'##############################
'GOVT: Financial side

'model.append fnl_g = fa_g_tr - fl_g_tr  'Model equation: Financial Net Lending (Balance): GOVT
model.append fnl_g = nib_g_tr  'Model equation: Financial Net Lending (Balance): GOVT
'model.append fa_g_tr = iba_g_tr + eqa_g_tr  'Model equation: Financial Assets: GOVT: Transactions
'model.append fl_g_tr = ibl_g_tr  'Model equation: Financial liabilities: GOVT: Transactions

'##############################
'GOVT: Change in stocks

'model.append iba_g = iba_g(-1) + iba_g_tr + iba_g_rv 'Model equation: Financial Assets: Interest bearing assets: GOVT
'model.append eqa_g = eqa_g(-1) + eqa_g_tr + eqa_g_rv 'Model equation: Financial Assets: Equity assets: GOVT
'model.append eqa_g_tr = c(11) * r_eq_dk + c(12) * d_1998 + c(13) * d_2004 'Model equation: Financial Assets: Equity assets: GOVT: Transactions
'model.append ibl_g = ibl_g(-1) + ibl_g_tr + ibl_g_rv 'Model equation: Financial Liabilities: Interest bearing liabilities: GOVT
model.append nib_g = nib_g(-1) + nib_g_tr + nib_g_rv 'Model equation: Net financial stock: Interest bearing: GOVT
model.append nib_g_tr = nl_g  'Model equation: Net financial stock: Interest bearing: GOVT: Transactions

'##############################
'GOVT: Returns on financial assets
model.append npir_nib_g = r_n(-1) * nib_g(-1) 'Model equation: Net property income received: NIB: GOVT: GOVT
model.append npir_g = npir_nib_g 'Model equation: Net property income paid: GOVT: GOVT

'##############################
'GOVT: Net wealth and net financial wealth

model.append nw_g = fnw_g + k_g  'Model equation: Net Wealth: GOVT: Total
'model.append fnw_g = (iba_g + eqa_g) - (ibl_g) 'Model equation: Financial net wealth: GOVT
model.append fnw_g = (nib_g) 'Model equation: Financial net wealth: GOVT


'##############################
'REST OF THE WORLD
'##############################

'##############################
'ROW: Real side

'model.append pir_row = r_ibl_f(-1) * iba_row(-1) + r_eq_dk * eqa_row(-1) + r_pen * pena_row(-1) 'Model equation: Property income received: ROW
'model.append pip_row = r_iba_f(-1) * ibl_row(-1) + r_eq_dk * eql_row(-1) + r_pen * penl_row(-1) 'Model equation: Property income paid: ROW
model.append private = s_k - g_k 'Model equation: Demand: PvT: Real prices
'model.append px = exp(beta(33) + beta(34) * log(pm) + beta(35) * log(ulc(-1))) 'Model equation: Price deflator: Exports
model.append px = exp(log(px(-1)) + 0.0409774893982 * d(log(px(-2))) + 1.05 * d(log(pm)) + 0.269665736139 * d(log(ulc(-1))) + 0.00287674220327) 'Model equation: Price deflator: Exports
'model.append m_k = exp(beta(20) + beta(21) * log(py(-1) / pm(-1)) + beta(22) * (log(c_h_k(-1) + i_k(-1) + x_k(-1))) + beta(23) * d_2009) 'Model equation: Imports: ROW: Real prices
model.append m_k = exp(-12.1636484353 + 0.222912324292 * log(py(-1) / pm(-1)) + 1.76672186633 * log(private) + 0.0560742708115 * d_2009) 'Model equation: Imports: ROW: Real prices
'model.append m_k = exp(log(m_k(-1)) + 0.109274723315 * d(log(m_k(-2))) - 0.277014740374 * d(log(py(-1) / pm(-1))) + 1.5459531773 * d(log(private)) + 0.00312219382121) 'Model equation: Imports: ROW: Real prices
model.append x_k = exp(beta(24) + beta(25) * log(px(-1) / pm(-1)) + beta(26) * log(fee59))  'Model equation: Exports: ROW: Real prices
'model.append x_k = exp (log(x_k(-1)) + 0.378637769824 * D(LOG(x_k(-1))) + 0.401423295285 * D(LOG(PX / PM)) + 0.581035198097 * D(LOG(FEE59)) - 0.00724081316519) 'Model equation: Exports: ROW: Real prices
model.append s_row =  - (x - m) + r_eq_dk * neq_row(-1) + r_pen * npen_row(-1) + r_n(-1) * nib_row(-1) + w_row - tax_row + stra_row + r_row_net_error1 + r_row_net_error2 + r_row_net_error3 'Model equation: Savings: ROW
'model.append neq_row = eqa_row - eql_row 'Model equation: Net financial stock: Equity: ROW
'model.append npen_row = pena_row - penl_row 'Model equation: Net financial stock: Pension: ROW
'model.append nib_row = iba_row - ibl_row 'Model equation: Net financial stock: Interest bearing: ROW
'model.append tax_row = beta(14) * w_row + beta(15) * pir_row 'Model equation: Tax: ROW
model.append nx = x - m 'Model equation: Net Exports: ROW
model.append cab =  - nl_row  'Model equation: Current account balance: ROW
model.append bop = cab + fab  'Model equation: Identity: Balance of payments: ROW
model.append fab = (fnl_row)  'Model equation: Financial account balance: ROW
model.append nl_row = (s_row - np_row + ctr_row)  'Model equation: Sector Balance (investment for row = 0): ROW
'model.append testo = pir_row - pip_row 'Model equation: Test variable: Net property income: ROW


'##############################
'ROW: Financial side

'model.append fnl_row = fa_row_tr - fl_row_tr  'Model equation: Financial Net Lending (Balance): ROW
model.append fnl_row = nib_row_tr + neq_row_tr + npen_row_tr  'Model equation: Financial Net Lending (Balance): ROW
'model.append fa_row_tr = iba_row_tr + eqa_row_tr + pena_row_tr  'Model equation: Financial Assets: ROW: Transactions
'model.append fl_row_tr = ibl_row_tr + eql_row_tr + penl_row_tr  'Model equation: Financial liabilities: ROW: Transactions
model.append nib_row = nib_row(-1) + nib_row_tr + nib_row_rv 'Model equation: Net financial stock: Interest bearing: ROW
model.append nib_row_tr = nl_row - neq_row_tr - npen_row_tr 'Model equation: Net financial stock: Interest bearing: ROW: Transactions
model.append neq_row = neq_row(-1) + neq_row_tr + neq_row_rv 'Model equation: Net financial stock: Equity: ROW
model.append npen_row = npen_row(-1) + npen_row_tr + npen_row_rv 'Model equation: Net financial stock: Pension: ROW
'model.append ibl_row = ibl_row(-1) + ibl_row_tr + ibl_row_rv 'Model equation: Financial Liabilities: Interest bearing liabilities: ROW
'model.append eql_row = eql_row(-1) + eql_row_tr + eql_row_rv 'Model equation: Financial Liabilities: Equity liabilities: ROW
'model.append penl_row = penl_row(-1) + penl_row_tr + penl_row_rv 'Model equation: Financial Liabilities: Pension liabilities: ROW

'##############################
'ROW: Returns on financial assets
model.append npir_nib_row = r_n(-1) * nib_row(-1) 'Model equation: Net property income received: NIB: ROW: ROW
model.append npir_neq_row = r_eq_dk * neq_row(-1) 'Model equation: Net property income received: NEQ: ROW: ROW
model.append npir_npen_row = r_pen * npen_row(-1) 'Model equation: Net property income received: NPEN: ROW: ROW
model.append npir_row = npir_nib_row + npir_neq_row + npir_npen_row 'Model equation: Net property income paid: ROW: ROW

'##############################
'ROW: Net wealth and net financial wealth

model.append nw_row = fnw_row  'Model equation: Net Wealth (No fixed assets): ROW: Financial
model.append fnw_row = (nib_row + neq_row + npen_row) 'Model equation: Financial net wealth: ROW

'##############################
'LABOUR MARKET
'##############################

model.append un = LF - n 'Model equation: Labour force: Unemployed persons: LABOUR
model.append ur = UN / LF 'Model equation: Labour force: Unemployment rate: LABOUR
model.append nf = Nu + n 'Model equation: Labour force: Employed persons: Danish waged: LABOUR
model.append nu = w_row / wage 'Model equation: Labour force: Employed persons: Danish nationals: Employed abroad: LABOUR
model.append n = exp(beta(48) + beta(49) * log(y_k(-1)) + beta(50) * log(lf) + beta(51) * @trend) 'Model equation: Labour force: Denmark for workers in production: LABOUR
model.append w_h = wage * n 'Model equation: Wages: HH: Nominal prices
'model.append wi = beta(57) + beta(58) * ur + beta(59) * d_2011 + beta(60) * @trend  'Model equation: Inflation rate: Wage inflation: LABOUR
'model.append wage = (1 + wi) * wage(-1) 'Model equation: Wages: Change in: (Estimated equation): LABOUR
'model.append wage = beta(87) + beta(88) * wage(-1) + beta(89) * ur  'Model equation: Wages: Change in: (Estimated equation): LABOUR
'model.append wage = 0.943735517579 * wage(-1) - 142.155225633 * ur - 131.01582817 * ur(-2) + 34.2775854816 'Model equation: Wages: Change in: (Estimated equation): LABOUR
'model.append wage = exp(log(wage(-1)) + 0.311016504901 * d(log(wage(-1))) + 0.5033217485 * d(log(wage(-2))) - 0.0113593726518 * d(log(un)) - 0.0209207246283 * d(log(un(-2))) + 0.00274205886526) 'Model equation: Wages: Change in: (Estimated equation): LABOUR
'model.append wage = wage(-1) + 0.599216598125 * d(wage(-1)) + 0.326526179536 * d(wage(-2)) - 80.3296391587 * d(ur) + 116.88553171 * d(ur(-1)) - 269.809166074 * d(ur(-2)) + 139.762096203 * d(ur(-3)) + 0.272714500856  'Model equation: Wages: Change in: (Estimated equation): LABOUR
model.append wage = wage(-1) + 0.42652589954 * D(WAGE(-1)) + 0.453061361725 * D(WAGE(-2)) - 101.475779645 * D(UR) + 88.9803626473 * D(UR(-1)) - 126.265628377 * D(UR(-2)) - 4.69192147383 * D_2011 + 0.822995896164 'Model equation: Wages: Change in: (Estimated equation): LABOUR


'##############################
'COMPLETELY SEALING THE SYSTEM
'##############################

 ': 'there are very small discrepancies in summing the errors across the sectors which show up in the net lending as well. I completely seal the system by making the sum of erros equal to zero
model.append r_h_error1 =  - ( r_nf_net_error1 + r_g_net_error1 + r_f_net_error1 + r_row_net_error1)  'Model equation: Property income error: HH
model.append r_h_error2 =  - ( r_nf_net_error2 + r_g_error2 + r_f_net_error2 + r_row_net_error2) 'Model equation: Property income error: HH
model.append r_h_error3 =  - ( r_f_error3 + r_row_net_error3) 'Model equation: Property income error: HH

'##############################
'CONSISTENCY CHECKS ON TRANSACTIONS
'##############################

model.append nl_check = (nl_h + nl_f + nl_nf + nl_g + nl_row) 'Model equation: Consistency check: Net lending
model.append check_np = np_row + np_h + np_f + np_nf + np_g 'Model equation: Consistency check
model.append check_ctr = ctr_row + ctr_h + ctr_f + ctr_nf + ctr_g 'Model equation: Consistency check
model.append check_stra = stra_h + stra_nf + stra_f + stra_g + stra_row 'Model equation: Consistency check
model.append check_invest = i - inv_nf - inv_f - inv_g - inv_h 'Model equation: Consistency check
model.append check_ib_tr = iba_h_tr - ibl_f_h_tr + ibl_h_tr - iba_f_h_tr 'Model equation: Consistency check
model.append check_eq_tr = neq_nf_tr + neq_f_tr + eqa_h_tr + neq_row_tr 'Model equation: Consistency check
model.append check_pen_tr = pena_h - penl_f + npen_row 'Model equation: Consistency check
model.append check_tax =  - tax_g + tax_nf + tax_f + tax_h + tax_row 'Model equation: Consistency check
model.append check_b2 = b2_nf + b2_f + b2_h + b2_g - b2 'Model equation: Consistency check
model.append check_wage =  - w_nf + w_h + w_row 'Model equation: Consistency check

model.append check_nib_rv = nib_nf_rv + iba_h_rv + iba_f_h_rv + nib_f_rv + nib_g_rv + nib_row_rv - ibl_h_rv - ibl_f_h_rv 'Model equation: Consistency check
model.append check_npen_rv = pena_h_rv - penl_f_rv + npen_row_rv 'Model equation: Consistency check
model.append check_neq_rv = neq_nf_rv + eqa_h_rv + neq_f_rv + neq_row_rv 'Model equation: Consistency check

model.append check_nib_tr = nib_nf_tr + nib_f_tr + nib_g_tr + nib_row_tr 'Model equation: Consistency check
model.append check_nib = nib_nf + nib_f + nib_g + nib_row 'Model equation: Consistency check
model.append check_nib_tflow = r_n(-1) * nib_row(-1) + r_n(-1) * nib_g(-1) + r_n(-1) * nib_nf(-1) + r_n(-1) * nib_f(-1) 'Model equation: Consistency check
model.append check_ib_tflow = r_iba_h(-1) * iba_h(-1) - r_iba_h(-1) * ibl_f_h(-1) + r_ibl_h(-1) * ibl_h(-1) - r_ibl_h(-1) * iba_f_h(-1) 'Model equation: Consistency check

model.append check_error1 = r_h_error1 + r_nf_net_error1 + r_g_net_error1 + r_f_net_error1 + r_row_net_error1 'Model equation: Consistency check
model.append check_error2 = r_h_error2 + r_nf_net_error2 + r_g_error2 + r_f_net_error2 + r_row_net_error2 'Model equation: Consistency check
model.append check_error3 = r_h_error3 + r_f_error3 + r_row_net_error3 'Model equation: Consistency check

'##############################
'SET SAMPLES AND END OF MODEL
'##############################

smpl 1996 @last

model.scenario baseline
model.solve(i = p)

'##############################
'Scenario 1: Increase the interest rate: Shock of 0.02 to flexible, and 0.005 to fixed
'##############################

 'Preserve baseline values: Save original value to baseline
smpl @all
genr r_ibl_fl_h_0 = r_ibl_fl_h 'Preserve baseline values: Save original value to baseline
genr r_iba_h_0 = r_iba_h 'Preserve baseline values: Save original value to baseline
genr r_ibl_fi_h_0 = r_ibl_fi_h 'Preserve baseline values: Save original value to baseline
genr r_n_0 = r_n 'Preserve baseline values: Save original value to baseline
'genr r_eq_dk_0 = r_eq_dk 'Preserve baseline values: Save original value to baseline
'genr r_eq_row_0 = r_eq_row 'Preserve baseline values: Save original value to baseline
'genr r_pen_0 = r_pen 'Preserve baseline values: Save original value to baseline

 'Shock 1: Introduce shock
smpl 2020 @last
r_ibl_fl_h = 0.02 + r_ibl_fl_h 'Shock 1: Introduce shock
'r_iba_h = 0.02 + r_iba_h 'Shock 1: Introduce shock
r_ibl_fi_h = 0.005 + r_ibl_fi_h 'Shock 1: Introduce shock
'r_n = (0.02 * (1-alpha)) + (0.005 * alpha)  + r_n 'Shock 1: Introduce shock
'r_eq_dk = 0.02 + r_eq_dk 'Shock 1: Introduce shock
'r_eq_row = 0.02 + r_eq_row 'Shock 1: Introduce shock
'r_pen = 0.02 + r_pen 'Shock 1: Introduce shock

 'Sample and solve: Select the sample, scenario and solve
smpl @all
model.scenario "Scenario 1"
model.solve(i = p)


 'Preserve scenario 1 values: Save Scenario 1 values to new variables
genr r_ibl_fl_h_1 = r_ibl_fl_h 'Preserve scenario 1 values: Save Scenario 1 values to new variables
genr r_iba_h_1 = r_iba_h 'Preserve scenario 1 values: Save Scenario 1 values to new variables
genr r_ibl_fi_h_1 = r_ibl_fi_h 'Preserve scenario 1 values: Save Scenario 1 values to new variables
genr r_n_1 = r_n 'Preserve scenario 1 values: Save Scenario 1 values to new variables
'genr r_eq_dk_1 = r_eq_dk 'Preserve scenario 1 values: Save Scenario 1 values to new variables
'genr r_eq_row_1 = r_eq_row 'Preserve scenario 1 values: Save Scenario 1 values to new variables
'genr r_pen_1 = r_pen 'Preserve scenario 1 values: Save Scenario 1 values to new variables

 'Restore baseline: Replace baseline values to the model
smpl @all
r_ibl_fl_h = r_ibl_fl_h_0 'Restore baseline: Replace baseline values to the model
r_iba_h = r_iba_h_0 'Restore baseline: Replace baseline values to the model
r_ibl_fi_h = r_ibl_fi_h_0 'Restore baseline: Replace baseline values to the model
r_n = r_n_0 'Restore baseline: Replace baseline values to the model
'r_eq_dk = r_eq_dk_0 'Restore baseline: Replace baseline values to the model
'r_eq_row = r_eq_row_0 'Restore baseline: Replace baseline values to the model
'r_pen = r_pen_0 'Restore baseline: Replace baseline values to the model

'##############################
'Scenario 2: Shock to house prices in 2025
'##############################

 'Preserve baseline values: Save original value to baseline
smpl @all
genr zz_i_0 = zz_i 'Preserve baseline values: Save original value to baseline

 'Shock 2: Introduce shock
smpl 2022 @last
zz_i = zz_i * 0.8 'Shock 2: Introduce shock

 'Sample and solve: Select the sample, scenario and solve
smpl @all
model.scenario(n, a="_2") "Scenario 2"
model.solve(i = p)

 'Preserve scenario 2 values: Save Scenario 2 values to new variables
genr zz_i_2 = zz_i 'Preserve scenario 2 values: Save Scenario 2 values to a new variable

 'Restore baseline: Replace baseline values to the model
smpl @all
zz_i = zz_i_0 'Restore baseline: Replace baseline values to the model

'##############################
'Scenario 3: Shocks 1 and 2 combined
'##############################


 'Preserve baseline values: Save original value to baseline
smpl @all
genr r_ibl_fl_h_0 = r_ibl_fl_h 'Preserve baseline values: Save original value to baseline
genr r_iba_h_0 = r_iba_h 'Preserve baseline values: Save original value to baseline
genr r_ibl_fi_h_0 = r_ibl_fi_h 'Preserve baseline values: Save original value to baseline
genr r_n_0 = r_n 'Preserve baseline values: Save original value to baseline
'genr r_eq_dk_0 = r_eq_dk 'Preserve baseline values: Save original value to baseline
'genr r_eq_row_0 = r_eq_row 'Preserve baseline values: Save original value to baseline
'genr r_pen_0 = r_pen 'Preserve baseline values: Save original value to baseline

 'Preserve baseline values: Save original value to baseline
genr zz_i_0 = zz_i 'Preserve baseline values: Save original value to baseline

 'Shock 1: Introduce shock
smpl 2020 @last
r_ibl_fl_h = 0.02 + r_ibl_fl_h 'Shock 1: Introduce shock
'r_iba_h = (0.02 * (1-alpha)) + (0.005 * alpha)   + r_iba_h 'Shock 1: Introduce shock
r_ibl_fi_h = 0.005 + r_ibl_fi_h 'Shock 1: Introduce shock
'r_n = (0.02 * (1-alpha)) + (0.005 * alpha)  + r_n 'Shock 1: Introduce shock
'r_eq_dk = 0.02 + r_eq_dk 'Shock 1: Introduce shock
'r_eq_row = 0.02 + r_eq_row 'Shock 1: Introduce shock
'r_pen = 0.02 + r_pen 'Shock 1: Introduce shock

 'Shock 2: Introduce shock
smpl 2022 @last
zz_i = zz_i * 0.8 'Shock 2: Introduce shock


 'Sample and solve: Select the sample, scenario and solve
smpl @all
model.scenario(n, a="_3") "Scenario 3"
model.solve(i = p)

 'Preserve scenario 3 values: Save Scenario 1 values to new variables
genr r_ibl_fl_h_3 = r_ibl_fl_h 'Preserve scenario 3 values: Save Scenario 1 values to new variables
genr r_iba_h_3 = r_iba_h 'Preserve scenario 3 values: Save Scenario 1 values to new variables
genr r_ibl_fi_h_3 = r_ibl_fi_h 'Preserve scenario 3 values: Save Scenario 1 values to new variables
genr r_n_3 = r_n 'Preserve scenario 3 values: Save Scenario 1 values to new variables
'genr r_eq_dk_3 = r_eq_dk 'Preserve scenario 1 values: Save Scenario 1 values to new variables
'genr r_eq_row_3 = r_eq_row 'Preserve scenario 1 values: Save Scenario 1 values to new variables
'genr r_pen_3 = r_pen 'Preserve scenario 1 values: Save Scenario 1 values to new variables

 'Preserve scenario 3 values: Save Scenario 2 values to new variables
genr zz_i_3 = zz_i 'Preserve scenario 3 values: Save Scenario 2 values to a new variable


 'Restore baseline: Replace baseline values to the model
smpl @all
r_ibl_fl_h = r_ibl_fl_h_0 'Restore baseline: Replace baseline values to the model
r_iba_h = r_iba_h_0 'Restore baseline: Replace baseline values to the model
r_ibl_fi_h = r_ibl_fi_h_0 'Restore baseline: Replace baseline values to the model
r_n = r_n_0 'Restore baseline: Replace baseline values to the model
'r_eq_dk = r_eq_dk_0 'Restore baseline: Replace baseline values to the model
'r_eq_row = r_eq_row_0 'Restore baseline: Replace baseline values to the model
'r_pen = r_pen_0 'Restore baseline: Replace baseline values to the model

zz_i = zz_i_0 'Restore baseline: Replace baseline values to the model


'##############################
'Scenario 4: Change in  alpha from 2017 plus shocks 1 and 2
'##############################


 'Preserve baseline values: Save original value to baseline
smpl @all
genr r_ibl_fl_h_0 = r_ibl_fl_h 'Preserve baseline values: Save original value to baseline
genr r_iba_h_0 = r_iba_h 'Preserve baseline values: Save original value to baseline
genr r_ibl_fi_h_0 = r_ibl_fi_h 'Preserve baseline values: Save original value to baseline
genr r_n_0 = r_n 'Preserve baseline values: Save original value to baseline
'genr r_eq_dk_0 = r_eq_dk 'Preserve baseline values: Save original value to baseline
'genr r_eq_row_0 = r_eq_row 'Preserve baseline values: Save original value to baseline
'genr r_pen_0 = r_pen 'Preserve baseline values: Save original value to baseline

 'Preserve baseline values: Save original value to baseline
genr alpha_0 = alpha 'Preserve baseline values: Save original value to baseline

 'Preserve baseline values: Save original value to baseline
genr zz_i_0 = zz_i 'Preserve baseline values: Save original value to baseline

 'Shock 1: Introduce shock
smpl 2020 @last
r_ibl_fl_h = 0.02 + r_ibl_fl_h 'Shock 1: Introduce shock
'r_iba_h = (0.02 * (1-alpha)) + (0.005 * alpha)   + r_iba_h 'Shock 1: Introduce shock
r_ibl_fi_h = 0.005 + r_ibl_fi_h 'Shock 1: Introduce shock
'r_n = (0.02 * (1-alpha)) + (0.005 * alpha)  + r_n 'Shock 1: Introduce shock
'r_eq_dk = 0.02 + r_eq_dk 'Shock 1: Introduce shock
'r_eq_row = 0.02 + r_eq_row 'Shock 1: Introduce shock
'r_pen = 0.02 + r_pen 'Shock 1: Introduce shock

 'Shock 2: Introduce shock
smpl 2022 @last
zz_i = zz_i * 0.8 'Shock 2: Introduce shock

 'Shock 3: Introduce shock
smpl 2017 @last
alpha = 0.8 'Shock 3: Introduce shock

 'Sample and solve: Select the sample, scenario and solve
smpl @all
model.scenario(n, a="_4") "Scenario 4"
model.solve(i = p)

 'Preserve scenario 4 values: Save Scenario 1 values to new variables
genr r_ibl_fl_h_4 = r_ibl_fl_h 'Preserve scenario 4 values: Save Scenario 1 values to new variables
genr r_iba_h_4 = r_iba_h 'Preserve scenario 4 values: Save Scenario 1 values to new variables
genr r_ibl_fi_h_4 = r_ibl_fi_h 'Preserve scenario 4 values: Save Scenario 1 values to new variables
genr r_n_4 = r_n 'Preserve scenario 4 values: Save Scenario 1 values to new variables
'genr r_eq_dk_4 = r_eq_dk 'Preserve scenario 4 values: Save Scenario 1 values to new variables
'genr r_eq_row_4 = r_eq_row 'Preserve scenario 4 values: Save Scenario 1 values to new variables
'genr r_pen_4 = r_pen 'Preserve scenario 4 values: Save Scenario 1 values to new variables

 'Preserve scenario 4 values: Save Scenario 2 values to new variables
genr zz_i_4 = zz_i 'Preserve scenario 4 values: Save Scenario 2 values to a new variable

 'Preserve scenario 4 values: Save Scenario 3 values to new variables
genr alpha_4 = alpha 'Preserve scenario 4 values: Save Scenario 3 values to new variables

 'Restore baseline: Replace baseline values to the model
smpl @all
r_ibl_fl_h = r_ibl_fl_h_0 'Restore baseline: Replace baseline values to the model
r_iba_h = r_iba_h_0 'Restore baseline: Replace baseline values to the model
r_ibl_fi_h = r_ibl_fi_h_0 'Restore baseline: Replace baseline values to the model
r_n = r_n_0 'Restore baseline: Replace baseline values to the model
'r_eq_dk = r_eq_dk_0 'Restore baseline: Replace baseline values to the model
'r_eq_row = r_eq_row_0 'Restore baseline: Replace baseline values to the model
'r_pen = r_pen_0 'Restore baseline: Replace baseline values to the model

zz_i = zz_i_0 'Restore baseline: Replace baseline values to the model

alpha = alpha_0 'Restore baseline: Replace baseline values to the model


'##############################
'Create new variables for later analysis
'##############################

'genr debt_coverage_0 = pip ': Generate additional variables
'genr inv_cap_ratio_0 = alpha ': Generate additional variables
'genr sav_cap_ratio_0 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables
'genr alpha_2 = alpha ': Generate additional variables

'smpl @all
'model.solve(i = p)


'##############################
'Export data to CSV file
'##############################
'wfsave(type = txt) scenario-compounded.csv na = "", delim = ","
wfsave(type = txt) scenario-isolated.csv na = "", delim = ","


'##############################
'Figure 1
'smpl 2014 2030
'graph fig1.line d(c_h_k_0) d(c_h_k_1) d(c_h_k_2) d(c_h_k_3)
'fig1.options linepat
'fig1.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig1.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig1.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig1.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'show fig1

'##############################
'Figure 1.a
'smpl 2014 2030
'graph fig1a.line d(c_h_k_0) d(c_h_k_1) d(c_h_k_2) d(c_h_k_3)
'fig1a.options linepat
'fig1a.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig1a.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig1a.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig1a.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'show fig1a

'##############################
'Figure 1.b
'smpl 2014 2030
'graph fig1b.line d(y_d_h_k_0) d(y_d_h_k_1) d(y_d_h_k_2) d(y_d_h_k_3)
'fig1b.options linepat
'fig1b.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig1b.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig1b.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig1b.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'show fig1b

'##############################
'Figure 1.c
'smpl 2014 2030
'graph fig1c.line (inv_h_k_0) (inv_h_k_1) (inv_h_k_2) (inv_h_k_3)
'fig1c.options linepat
'fig1c.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig1c.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig1c.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig1c.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'show fig1c

'##############################
'Figure 2
'smpl 2014 2030
'graph fig2.line (cab_0) (cab_1) (cab_2) (cab_3)
'fig2.options linepat
'fig2.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig2.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig2.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig2.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'show fig2

'##############################
'Figure 2.a
'smpl 2014 2030
'graph fig2a.line (cab_0 / y_k_0) (cab_1 / y_k_1) (cab_2 / y_k_2) (cab_3 / y_k_3)
'fig2a.options linepat
'fig2a.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig2a.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig2a.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig2a.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'show fig2a

'##############################
'Figure 3
'smpl 2014 2030
'graph fig3.line (i_k_0) (i_k_1) (i_k_2) (i_k_3)
'fig3.options linepat
'fig3.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig3.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig3.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig3.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'show fig3

'##############################
'Figure 3.a
'smpl 2014 2030
'graph fig3a.line (i_k_0 / y_k_0) (i_k_1 / y_k_1) (i_k_2 / y_k_3) (i_k_2 / y_k_3)
'fig3a.options linepat
'fig3a.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig3a.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig3a.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig3a.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'show fig3a

'##############################
'Figure 4
'smpl 2014 2030
'graph fig4.line (nl_h_0 / y_0) (nl_nf_0 / y_0) (nl_f_0 / y_0) (nl_row_0 / y_0) (nl_g_0 / y_0)
'fig4.options linepat
'fig4.setelem(1) lcolor(blue) lwidth(1) lpat(1)
'fig4.setelem(2) lcolor(green) lwidth(1) lpat(1)
'fig4.setelem(3) lcolor(red) lwidth(1) lpat(1)
'fig4.setelem(4) lcolor(black) lwidth(1) lpat(1)
'fig4.setelem(5) lcolor(orange) lwidth(1) lpat(1)
'show fig4

'##############################
'Figure 5
'smpl 2014 2030
'graph fig5.line (c_h_k_0 / y_k_0) (c_h_k_1 / y_k_1) (c_h_k_2 / y_k_2) (c_h_k_3 / y_k_3) (y_d_h_0 / y_k_0) (y_d_h_1 / y_k_1) (y_d_h_2 / y_k_2) (y_d_h_3 / y_k_3)
'fig5.options linepat
'fig5.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig5.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig5.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig5.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'fig5.setelem(5) lcolor(0, 50, 130) lwidth(1) lpat(1)
'fig5.setelem(6) lcolor(0, 150, 255) lwidth(1) lpat(2)
'fig5.setelem(7) lcolor(0, 200, 255) lwidth(1) lpat(4)
'fig5.setelem(8) lcolor(0, 55, 255) lwidth(1) lpat(5)
'show fig5

'##############################
'Figure 5a
'smpl 2014 2030
'graph fig5a.line (c_h_k_0) (c_h_k_1) (c_h_k_2) (c_h_k_3) (y_d_h_0) (y_d_h_1) (y_d_h_2) (y_d_h_3)
'fig5a.options linepat
'fig5a.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig5a.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig5a.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig5a.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'fig5a.setelem(5) lcolor(0, 50, 130) lwidth(1) lpat(1)
'fig5a.setelem(6) lcolor(0, 150, 255) lwidth(1) lpat(2)
'fig5a.setelem(7) lcolor(0, 200, 255) lwidth(1) lpat(4)
'fig5a.setelem(8) lcolor(0, 55, 255) lwidth(1) lpat(5)
'show fig5a


'##############################
'Figure 6
'smpl 2014 2030
'graph fig6.line (pip_ibl_fi_h_0 / y_k_0) (pip_ibl_fi_h_1 / y_k_1) (pip_ibl_fi_h_2 / y_k_2) (pip_ibl_fi_h_3 / y_k_3) (pip_ibl_fl_h_0 / y_k_0) (pip_ibl_fl_h_1 / y_k_1) (pip_ibl_fl_h_2 / y_k_2) (pip_ibl_fl_h_3 / y_k_3)
'fig6.options linepat
'fig6.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig6.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig6.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig6.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'fig6.setelem(5) lcolor(0, 50, 130) lwidth(1) lpat(1)
'fig6.setelem(6) lcolor(0, 150, 255) lwidth(1) lpat(2)
'fig6.setelem(7) lcolor(0, 200, 255) lwidth(1) lpat(4)
'fig6.setelem(8) lcolor(0, 55, 255) lwidth(1) lpat(5)
'show fig6


'##############################
'Figure 7
'smpl 2014 2030
'graph fig7.line (nl_check_0) (nl_check_1) (nl_check_2)
'fig7.options linepat
'fig7.setelem(1) lcolor(blue) lwidth(1) lpat(1)
'fig7.setelem(2) lcolor(green) lwidth(1) lpat(2)
'fig7.setelem(3) lcolor(red) lwidth(1) lpat(4)
'show fig7

'##############################
'Figure 8
'smpl 2014 2030
'graph fig8.line (iba_h_0) (iba_h_1) (iba_h_2) (iba_h_3) (ibl_h_0) (ibl_h_1) (ibl_h_2) (ibl_h_3)
'fig8.options linepat
'fig8.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig8.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig8.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig8.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'fig8.setelem(5) lcolor(0, 50, 130) lwidth(1) lpat(1)
'fig8.setelem(6) lcolor(0, 150, 255) lwidth(1) lpat(2)
'fig8.setelem(7) lcolor(0, 200, 255) lwidth(1) lpat(4)
'fig8.setelem(8) lcolor(0, 55, 255) lwidth(1) lpat(5)
'show fig8

'##############################
'Figure 9
'smpl 2014 2030
'graph fig9.line (iba_f_h_0) (iba_f_h_1) (iba_f_h_2) (iba_f_h_3) (ibl_f_h_0) (ibl_f_h_1) (ibl_f_h_2) (ibl_f_h_3)
'fig9.options linepat
'fig9.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig9.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig9.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig9.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'fig9.setelem(5) lcolor(0, 50, 130) lwidth(1) lpat(1)
'fig9.setelem(6) lcolor(0, 150, 255) lwidth(1) lpat(2)
'fig9.setelem(7) lcolor(0, 200, 255) lwidth(1) lpat(4)
'fig9.setelem(8) lcolor(0, 55, 255) lwidth(1) lpat(5)
'show fig9

'##############################
'Figure 10
'smpl 2014 2030
'graph fig10.line (nw_h_0) (nw_h_1) (nw_h_2) (nw_h_3) (nw_f_0) (nw_f_1) (nw_f_2) (nw_f_3)
'fig10.options linepat
'fig10.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig10.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig10.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig10.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'fig10.setelem(5) lcolor(0, 50, 130) lwidth(1) lpat(1)
'fig10.setelem(6) lcolor(0, 150, 255) lwidth(1) lpat(2)
'fig10.setelem(7) lcolor(0, 200, 255) lwidth(1) lpat(4)
'fig10.setelem(8) lcolor(0, 55, 255) lwidth(1) lpat(5)
'show fig10

'##############################
'Figure 11
'smpl 2014 2030
'graph fig11.line (nw_nf_0) (nw_nf_1) (nw_nf_2) (nw_nf_3) (nw_g_0) (nw_g_1) (nw_g_2) (nw_g_3) (nw_row_0) (nw_row_1) (nw_row_2) (nw_row_3)
'fig11.options linepat
'fig11.setelem(1) lcolor(0, 0, 0) lwidth(1) lpat(1)
'fig11.setelem(2) lcolor(89, 89, 89) lwidth(1) lpat(2)
'fig11.setelem(3) lcolor(166, 166, 166) lwidth(1) lpat(4)
'fig11.setelem(4) lcolor(225, 30, 0) lwidth(1) lpat(5)
'fig11.setelem(5) lcolor(255, 45, 0) lwidth(1) lpat(1)
'fig11.setelem(6) lcolor(255, 200, 0) lwidth(1) lpat(2)
'fig11.setelem(7) lcolor(255, 165, 0) lwidth(1) lpat(4)
'fig11.setelem(8) lcolor(255, 130, 0) lwidth(1) lpat(5)
'fig11.setelem(9) lcolor(0, 50, 130) lwidth(1) lpat(1)
'fig11.setelem(10) lcolor(0, 150, 255) lwidth(1) lpat(2)
'fig11.setelem(11) lcolor(0, 200, 255) lwidth(1) lpat(4)
'fig11.setelem(12) lcolor(0, 55, 255) lwidth(1) lpat(5)
'show fig11
