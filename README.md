# BRFSS-Health-Survey-Dashboard

##Introduction
BRFSS, or the Behavioral Risk Factor Surveillance System, is the world’s largest and longest-running telephone survey system for collecting health-related information from adults in the U.S.. It is a collaborative project between the Centers for Disease Control and Prevention (CDC) and all U.S. states, the District of Columbia, and U.S. territories. The survey gathers data on health behaviors, chronic conditions, and preventive care to monitor public health and inform policy.

##What BRFSS collects
Health-related risk behaviors: Such as tobacco use, alcohol consumption, and physical activity.
Chronic health conditions: Data on conditions like heart disease, diabetes, and cancer.
Preventive care: Information on access to and use of health services, including vaccinations and screenings.
Demographics and other factors: Includes information on health care access, sleep patterns, and other demographic data.

##How BRFSS is conducted
Surveys: Telephone surveys are conducted monthly using landlines and cellular phones.
Data collection: Each state conducts its own survey with the assistance of the CDC, and the data is aggregated at the state level.
Population: The target population is the civilian noninstitutionalized adult population aged 18 and older.
Scale: The survey system completes over 400,000 adult interviews each year across all participating states and territories.

##Why BRFSS is important
Monitoring health: It provides state-specific data that helps monitor public health trends and assess progress toward national health objectives.
Informing policy: The data is used to design and evaluate public health programs and policies aimed at improving the health of the population.
Research: It serves as a crucial data source for researchers studying health-related issues across the country.
About the Dataset
The BRFSS data repository is enormous and not available to the public. However, summary statistics are publicly available [here](https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-P/dttw-5yxu/about_data) in *.csv format at the relatively modest size of 1GB. Please download this summary data into your IDE as tibble df to explore it with this markdown file.

For each question asked in column Question, summary statistics include percentage confidence intervals for each possible response value in column Response. The left (right) endpoint of this interval is in Confidence_limit_Low (Confidence_limit_High). Most importantly for your project goals, these percentages are computed on grouped data. Grouping is done always using columns Locationabbr (state/territory) and Year. For this broadest grouping, columns Break_Out and Break_Out_Category take value “Overall”. Further grouping is indicated in those columns, including by: age, household income, sex, race, and highest education level.

##Project Goal
Produce a dashboard interface to the BRFSS dataset, allowing the user to select the survey question and the dashboard displays the various breakouts of the survey results for the question. Each subpanel of the dashboard should subset the results by only one criteria (age, for example). There should be a subpanel devoted to choosing the question. In addition, for each possible response, a:

confidence interval overall in an Overall subpanel
confidence interval versus gender in a By_Gender subpanel
confidence interval versus age in a By_Age_Group subpanel
confidence interval versus state/territory in a By_Location subpanel
confidence interval versus level of education in a By_Education subpanel
confidence interval versus income level in a By_Income subpanel
confidence interval versus year in a Temporal subpanel

##Proposed workflow
Consider a parquet-formatted dataset, based on observed speedup of information retrieval.
Design the question selection tool.
Design the aggregation workflow for each subpanel, beginning with the Overall subpanel.
Design the Overall subpanel without “more” or “less” options.
Augment the Overall subpanel with “more” or “less” options.
Design the other subpanels without “more” or “less” options.
Modify these subpanels to include “more” or “less” options.

##Aggregation Strategy for “Overall” Breakout
Most of the questions have a breakout group called “Overall”. As the name suggests, this is the broadest aggregation so far in the dataset. However, percentages labeled “Overall” are still per-territory, per-year summary metrics and not true overall results! Here, we consider a method to aggregate and produce confidence limits for response percentages.

link-shared 10-minute recorded presentation (due 12/3/2025). Walk-through dashboard demo. Equal time for each team member.
