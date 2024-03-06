CREATE OR REPLACE TABLE
  `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.activity_data` AS
WITH
  verified AS (
  SELECT
    Connect_ID,
    -- Calculate the week number since a fixed date for recruitment
    d_914594314 AS VerifiedDate,
    -- Calculate the week number since a fixed date for verification
    DATE_TRUNC(DATE(d_914594314), week(Monday)) AS VerifiedWeekDate,
    -- Determine the last blood update date based on conditions
    CASE
      WHEN (d_173836415_d_266600170_d_592099155='664882224' AND d_878865966 = '353358909') THEN DATE_DIFF(DATE(GREATEST(d_173836415_d_266600170_d_982213346,d_173836415_d_266600170_d_398645039,d_173836415_d_266600170_d_822274939)),DATE('2021-07-23'),week(Monday))
      WHEN (d_173836415_d_266600170_d_592099155 ='534621077'
      AND d_878865966='353358909') THEN DATE_DIFF(DATE(d_173836415_d_266600170_d_561681068),DATE('2021-07-23'),week(Monday))
  END
    AS BloodSampleWeek, #week when blood sample collected (wrt start of connect study)
    CASE
      WHEN (d_173836415_d_266600170_d_592099155='664882224' AND d_878865966='353358909' AND d_100767870='353358909') THEN GREATEST(d_173836415_d_266600170_d_982213346,d_173836415_d_266600170_d_398645039,d_173836415_d_266600170_d_822274939,d_517311251,d_832139544,d_264644252,d_770257102)
      WHEN (d_173836415_d_266600170_d_592099155 ='534621077'
      AND d_878865966='353358909'
      AND d_100767870='353358909') THEN GREATEST(d_173836415_d_266600170_d_561681068,d_517311251,d_832139544,d_264644252,d_770257102)
  END
    AS AllModulesCompleteDate #date when all modules have been completed
  FROM
    `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP`
  WHERE
    d_821247024 = '197316935'
    AND d_747006172 != '353358909' )
SELECT
  Connect_ID,
  VerifiedWeekDate,
  BloodSampleWeek,
  AllModulesCompleteDate
FROM
  verified;