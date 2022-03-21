# Stream Intermittency Visualization Dashboard (SIVD)

## Project Description
The SIVD tool provides the ability to analyze high frequency Stream, Temperature, Intermittency and Conductivity (STIC; [Chapin et al. 2014](https://doi.org/10.1002/2013WR015158)) logger data, field observations and associated datasets in an environment that promotes interactivity and data exploration. This dashboards facilitated data processing related to stream data collection efforts at 180 perennial and non-perennial sites across the Great Plains and Upper Midwest of the United States.  The dashboard served multiple purposes as part of data processing and analysis workflow, 1) to visualize multiple data streams of high frequency sensor data, 2) to have at-a-glance hydrologic metrics created during and after data processing, and 3) to view data quality metrics we created to describe high-frequency dataset quality (e.g. length, completeness). We provide an example dataset and the R code used to create the Shiny dashboard. We also created a video that shows an example of how the dashboard was used to visualize data and infer wet and dry periods in a stream reach.
## Usage
### Code
Code in this repository can be used to generate the Shiny dashboard.  There are two R files used to create the SIVD.
* SIVD_main.R
	* Contains the UI, server, and ShinyApp call that create the dashboard
* HTML_tables.R
	* Contains the code to generate the HTML tables in the dashboard
### Data
There are four CSV files containing the data used by the application.
* GP_P1-3_logger_daily_2022-03-16.csv
* GP_P1-3_logger_hourly_2022-03-16.csv
* GP_P1-3_summary_2022-03-16.csv
* S123_field_data_2022-03-15.csv

### Software
The dashboard was developed with R software, version 4.1.0, on a Windows computer.  

### References
* [Chapin, T.P., Todd, A.S. and Zeigler, M.P., 2014. Robust, low‐cost data loggers for stream temperature, flow intermittency, and relative conductivity monitoring. Water Resources Research, 50(8), pp.6542-6548.](https://doi.org/10.1002/2013WR015158)
* [Shiny from RStudio](https://shiny.rstudio.com/)


# Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an “as is” basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

