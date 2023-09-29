
--Step 1: Define project, dataset, and tabled
DECLARE project STRING;
DECLARE dataset STRING;
DECLARE source_table STRING;
DECLARE destination_table STRING;
DECLARE sql_query STRING;

SET project = 'project_ID';
SET dataset = 'dataset_ID';
SET source_table = 'participants_JP';
SET destination_table = 'BQ2';

--Step 2: Create a temporary table with data from the source table
CREATE OR REPLACE TABLE `project_ID.dataset_ID.temp_table` AS
SELECT *,
  -- Step 3: Create new variables and perform transformations
  IF(d_100767870 = 104430631 AND d_878865966 = 104430631, 1, 0) AS verified_no_activities,
  IF(d_100767870 = 353358909, 1, 0) AS survey_only,
  IF(d_100767870 = 104430631 AND d_878865966 = 353358909, 1, 0) AS blood_only,
  PARSE_DATE('%Y-%m-%d', d_264644252) AS d_264644252,
  PARSE_DATE('%Y-%m-%d', d_770257102) AS d_770257102,
  PARSE_DATE('%Y-%m-%d', d_832139544) AS d_832139544,
  PARSE_DATE('%Y-%m-%d', d_517311251) AS d_517311251,
  PARSE_DATE('%Y-%m-%d', d_914594314) AS d_914594314,
  PARSE_DATE('%Y-%m-%d', d_173836415_d_266600170_d_561681068) AS d_173836415_d_266600170_d_561681068,
  PARSE_DATE('%Y-%m-%d', d_173836415_d_266600170_d_982213346) AS d_173836415_d_266600170_d_982213346,
  PARSE_DATE('%Y-%m-%d', d_173836415_d_266600170_d_822274939) AS d_173836415_d_266600170_d_822274939,
  PARSE_DATE('%Y-%m-%d', d_173836415_d_266600170_d_398645039) AS d_173836415_d_266600170_d_398645039,
  CASE

    -- Step 4: Create the 'age' variable based on conditions
    WHEN state_d_934298480 = 124276120 THEN '40-45'
    WHEN state_d_934298480 = 450985724 THEN '46-50'
    WHEN state_d_934298480 = 363147933 THEN '51-55'
    WHEN state_d_934298480 = 636706443 THEN '56-60'
    WHEN state_d_934298480 = 771230670 THEN '61-65'
  END AS age
FROM `project_ID.dataset_ID.source_table`;

-- Step 5: Define the SQL query to create a view in BQ2
SET sql_query = '''
  CREATE OR REPLACE TABLE `project_ID.dataset_ID.{destination_table}` AS
  SELECT *,
    IF(survey_only = 1 AND blood_only = 1, 1, 0) AS survey_and_blood,
    IF(verified_no_activities = 1, DATE_TRUNC(verified_no_activities_date, WEEK), DATE('1970-01-01')) AS verified_no_activities_date,
    IF(survey_only = 1 AND blood_only = 0, survey_date, DATE('1970-01-01')) AS survey_only_date,
    IF(blood_only = 1 AND survey_only = 0, blood_date, DATE('1970-01-01')) AS blood_only_date,
    IF(blood_only = 1 AND survey_only = 1, s_or_b_date, DATE('1970-01-01')) AS survey_and_blood_date
  FROM `project_ID.dataset_ID.temp_table`
''';

-- Step 6: Execute the SQL query to create a view in BQ2
EXECUTE IMMEDIATE sql_query;
