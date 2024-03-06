**Introduction**
The Stakeholder Metrics Dashboard is an Rshiny App currently being developed by RS + JP in order to provide real-time recruitment data visualizations to the C4CP stakeholders. The app is currently hosted on the dev server of Posit-Connect: `https://appshare-dev.cancer.gov/`. 

**Application Structure**
The main body of the dashboard is contained in the `app.R` file, saved in the `stakeholderMetrics/app` folder. This file is where all UI is defined and where all plots are called and aggregated. Each plot is contained in a separate file, which matches the function named within the file. All plots are generated from data saved in the table: `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.complete_table`. This table is also a work-in-progress. Queries used to generate the variables within this table are located in the `stakeholderMetrics/bq2_queries/` directory. 

**Installation and Set up**
There is no installation or set up required for a user to begin editing the dashboard application code. There is however, set up required to access the app.

In order for a new user to access the application on Posit Connect:
1. Navigate to: `https://appshare-dev.cancer.gov/`
2. Login using your NIH credentials
3. Alert RS or JP that you have logged-in and are ready to be invited to the app
4. RS/JP will add you to the approved user list
5. You will receive an email from Posit about your app access

**GH Preferences**
If you are tasked with making any changes to the dashboard, please assign yourself a specific, detailed issue and create a branch for that issue.
