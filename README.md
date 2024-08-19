## Introduction
The Stakeholder Metrics Dashboard is an Rshiny App currently being developed by RS + JP in order to provide real-time recruitment data visualizations to the C4CP stakeholders. The app is currently hosted on the dev server of Posit-Connect: `https://appshare-dev.cancer.gov/`. 

## Application Structure
The main body of the dashboard is contained in the `app.R` file, saved in the `stakeholderMetrics/app` folder. This file is where all UI is defined and where all plots are called and aggregated. Each plot is contained in a separate file, which matches the function named within the file. All plots are generated from data saved in the table: `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.complete_table`. This table is also a work-in-progress. Queries used to generate the variables within this table are located in the `stakeholderMetrics/bq2_queries/` directory. 

The application has 2 basic functions which generate the graphical interface. The `server` function authenticates the user (this authentication code was written by D. Russ), pulls data from GCP, cleans the data (mostly labeling data, not much cleaning is done), filters data (if filters are used) and creates plots. The `server` function is called exactly once when the app is started. However, the `reactive` components, such as filtering data based on user-identified values, will be re-triggered, once the `applyFilters` button is pressed. Plots will also be re-generated with the newly filtered data.

The second function of the app is the `ui` component. This is where aesthetics and layouts for the dashboard are built. Every component built in the `ui` function will be referenced in the `server` function. If a plot is not showing up, its most likely not defined in both the `ui` and `server` components. 

Finally, most of the aesthetics of the dashboard are housed in the `customCSS.R` file. This file applies rules globally to the dashboard and its elements such as: dashboard and plot fonts, font sizes, plot bucket sizes, dashboard element color palette.

## Installation and Set up
There is no installation or set up required for a user to begin editing the dashboard application code. There is however, set up required to access the app.

In order for a new user to access the application on Posit Connect:
1. Navigate to: `https://appshare-dev.cancer.gov/`
2. Login using your NIH credentials
3. Alert RS or JP that you have logged-in and are ready to be invited to the app
4. RS/JP will add you to the approved user list
5. You will receive an email from Posit about your app access

## GH Preferences
If you are tasked with making any changes to the dashboard, please assign yourself a specific, detailed issue and create a branch for that issue.


## Backend GCP Structure
Stakeholder Metrics Dashboard Data Pipeline
<img width="885" alt="Screenshot 2024-06-13 at 10 46 11 AM" src="https://github.com/Analyticsphere/stakeholderMetrics/assets/32822979/2a25c648-e26f-46b3-a01c-a5a7d654c91a">

Stakeholder Metrics Dashboard Scheduled Query Structure
<img width="915" alt="Screenshot 2024-08-19 at 1 04 49 PM" src="https://github.com/user-attachments/assets/ec4ddb3b-2950-4a65-b39f-b7f57909ff1a">


|Dashboard Tab| Datasource|Datasource Update Schedule|
---------------|----------|--------------------------|
Verified |`nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.complete_table`| `complete query`, 12pm daily|
Verified | `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.figure1_activity`| `figure1_activity`, 11am daily |
Verified | `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.figure2_age`| `figure2_age`, 11am daily |
Verified | `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.figure3_race`| `figure3_race`, 11am daily |
Invited |`nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.invited_participants_complete`| `invited_participants_complete`, updated 12pm daily|








## How to Publish the App from Rstudio
As of 3/12/24, we are publishing the app on the posit dev server: https://appshare-dev.cancer.gov/. The name of the app: Stakeholer Dashboard, Author: Rebecca Sansale.
How to Publish the app from Rstudio:
1. Login to the VPN and Posit Connect.
2. Open the "app.R" file in Rstudio
3. Click the down arrow next to the blue circle icon<img width="243" alt="Screenshot 2024-03-12 at 1 14 31 PM" src="https://github.com/Analyticsphere/stakeholderMetrics/assets/32822979/ee9934fb-6b60-4e28-b79a-6311bf44da34">
4. A small drop-down menu with appear, if you see the "Stakeholder Dashboard" option, click that one. <img width="343" alt="Screenshot 2024-03-12 at 1 15 14 PM" src="https://github.com/Analyticsphere/stakeholderMetrics/assets/32822979/0e790036-98b3-439b-8cdb-68c295241607">
5. A pop up window will appear. Check the name of the dashboard you are updating (middle right panel) and that the user account information is correct. Ensure all files that the app requires to run are selected in the left panel. <img width="613" alt="Screenshot 2024-03-12 at 1 17 59 PM" src="https://github.com/Analyticsphere/stakeholderMetrics/assets/32822979/3a752146-1fb5-44ab-976c-4dcf2b05c10a">

6. Click "publish"
7. Posit connect will open in your browser, directly to the app page.



