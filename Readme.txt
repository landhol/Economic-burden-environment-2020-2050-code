README

This repository contains the analysis code used in the manuscript: "Health-augmented macroeconomic estimates and projections of economic burden of environmental risks from 2020 to 2050".

The scripts implement a health-augmented dynamic macroeconomic framework integrating epidemiological projections with education-stratified effects of mortality and morbidity on labor supply, treatment costs, and counterfactual economic scenarios. Code is provided for transparency and reproducibility. We may not be able to answer detailed technical questions about all scripts.


1. MainAnalysis Folder

This folder includes the scripts used to perform the main analyses and generate the primary results.

Files in MainAnalysis:

	Mortality_morbidity_UI.R
Processes mortality and morbidity inputs and uncertainty intervals used in the analysis.

	Population_UI.R
Calculates counterfactual population trajectories under no-risk scenarios.

	Labor_participation_UI.R
Estimates counterfactual labor participation rates, including education-stratified effects.

	Aggregate_human_capital_Ht.R
Calculates aggregate human capital (H(t)) based on demographic and labor inputs.

	Treatment_cost_n2.R
Estimates disease-specific treatment expenditures by country.

	Y_new.R
Calculates aggregate output and cumulative economic burden by comparing baseline and counterfactual scenarios.

	Imputation_new.R
Performs extrapolation or imputation for countries with missing data.

	PAF_21.R
Calculates economic burden attributable to specific environmental risk factors.


2. Tables and Figures Folder

This folder contains scripts used to generate tables and figures presented in the manuscript.

Files in Tables and Figures:

	table_stratified_analysis.R
Performs stratified analyses and generates summary tables.

	By_country_map.R
Produces country-level maps of economic burden estimates.

	stacked_bar_preprocessing.R
Prepares data for stacked bar charts (figures created using GraphPad or equivalent software).

	Heat_map_processing_and_plot.R
Processes data and generates heat maps used in the figures.



Data

Country-level economic, demographic, and disease burden inputs were obtained from publicly available international databases. These included:

	Per-capita gross domestic product (GDP) estimates and projections through 2050 from the Institute for Health Metrics and Evaluation (IHME).
	Physical capital stocks and capital shares from the Penn World Table version 10.0.
	National saving rates and health expenditures from the World Bank database.
	Labor-force participation rates from the International Labour Organization (ILO).
	Population projections from 2020 to 2050 from the United Nations Department of Economic and Social Affairs (UN DESA).
	Age-specific educational attainment data from the Barro–Lee dataset.
	Years of life lost (YLLs), years lived with disability (YLDs), disability-adjusted life years (DALYs), and population-attributable fractions (PAFs) from the Global Burden of Disease (GBD) Study 2021.
	Disease-specific treatment cost estimates were derived from previously published studies.
