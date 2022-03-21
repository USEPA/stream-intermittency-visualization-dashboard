# Stream Intermittency Visualization Dashboard (SIVD)

## Project Description
The SIVD tool provides researchers and practitioners alike the ability to analyze high frequency stream temperature, intermittency, and conductivity (STIC; [Chapin et al. 2014](https://doi.org/10.1002/2013WR015158)) logger data and associated datasets in an environment that promotes interactivity and data exploration.
  STIC logger data and field observation data used for this example dashboard come from a study quantifying days of flow versus no-flow for 183 stream reaches across the Great Plains and Midwest regions of the United States.  

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

