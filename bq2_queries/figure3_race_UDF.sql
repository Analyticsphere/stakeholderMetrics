CREATE OR REPLACE TABLE `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.figure3_race_UDF` AS
WITH module1_v1_v2 AS (
  SELECT
    Connect_ID,
    D_384191091_D_384191091_D_412790539 AS White,
    D_384191091_D_384191091_D_458435048 AS Black_African_American,
    D_384191091_D_384191091_D_583826374 AS American_Indian_Alaska_Native,
    D_384191091_D_384191091_D_586825330 AS Hawaiian_Pacific_Islander,
    D_384191091_D_384191091_D_973565052 AS Asian,
    D_384191091_D_384191091_D_636411467 AS Hispanic_Latino_Spanish,
    D_384191091_D_384191091_D_706998638 AS Middle_Eastern_North_African,
    D_384191091_D_384191091_D_746038746 AS Other,
    D_384191091_D_384191091_D_807835037 AS Skipped_Question,
    D_384191091_D_747350323,
    2 AS version
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.module1_v2_JP`
  UNION ALL #this union is concat. the 2 datasets (cbind)
  SELECT
    Connect_ID,
    D_384191091_D_384191091_D_412790539 AS White,
    D_384191091_D_384191091_D_458435048 AS Black_African_American,
    D_384191091_D_384191091_D_583826374 AS American_Indian_Alaska_Native,
    D_384191091_D_384191091_D_586825330 AS Hawaiian_Pacific_Islander,
    D_384191091_D_384191091_D_973565052 AS Asian,
    D_384191091_D_384191091_D_636411467 AS Hispanic_Latino_Spanish,
    D_384191091_D_384191091_D_706998638 AS Middle_Eastern_North_African,
    D_384191091_D_384191091_D_746038746 AS Other,
    D_384191091_D_384191091_D_807835037 AS Skipped_Question,
    D_384191091_D_747350323,
    1 AS version
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.module1_v1_JP`
  #of the participants who completed both versions of module 1
  #keep the data only from version 1, exclude the data of the overlap participants from version 1
  WHERE Connect_ID NOT IN UNNEST(`nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.get_common_module1_connect_ids`())   
)

SELECT
  Connect_ID,
  CASE
    WHEN White = '1' AND Black_African_American = '0' AND American_Indian_Alaska_Native = '0' AND Hawaiian_Pacific_Islander = '0' 
         AND Asian = '0' AND Hispanic_Latino_Spanish = '0' AND Middle_Eastern_North_African = '0' AND Other = '0' THEN "White"
    WHEN White = '0' AND Black_African_American = '1' AND American_Indian_Alaska_Native = '0' AND Hawaiian_Pacific_Islander = '0' 
         AND Asian = '0' AND Hispanic_Latino_Spanish = '0' AND Middle_Eastern_North_African = '0' AND Other = '0' THEN "Black, African American, or African"
    WHEN White = '0' AND Black_African_American = '0' AND American_Indian_Alaska_Native = '1' AND Hawaiian_Pacific_Islander = '0' 
         AND Asian = '0' AND Hispanic_Latino_Spanish = '0' AND Middle_Eastern_North_African = '0' AND Other = '0' THEN "American Indian, Alaska Native"
    WHEN White = '0' AND Black_African_American = '0' AND American_Indian_Alaska_Native = '0' AND Hawaiian_Pacific_Islander = '1' 
         AND Asian = '0' AND Hispanic_Latino_Spanish = '0' AND Middle_Eastern_North_African = '0' AND Other = '0' THEN "Hawaiian Pacific Islander"         
    WHEN White = '0' AND Black_African_American = '0' AND American_Indian_Alaska_Native = '0' AND Hawaiian_Pacific_Islander = '0' 
         AND Asian = '1' AND Hispanic_Latino_Spanish = '0' AND Middle_Eastern_North_African = '0' AND Other = '0' THEN "Asian"
    WHEN White = '0' AND Black_African_American = '0' AND American_Indian_Alaska_Native = '0' AND Hawaiian_Pacific_Islander = '0' 
         AND Asian = '0' AND Hispanic_Latino_Spanish = '1' AND Middle_Eastern_North_African = '0' AND Other = '0' THEN "Hispanic, Latino, Spanish"   
    WHEN White = '0' AND Black_African_American = '0' AND American_Indian_Alaska_Native = '0' AND Hawaiian_Pacific_Islander = '0' 
         AND Asian = '0' AND Hispanic_Latino_Spanish = '0' AND Middle_Eastern_North_African = '1' AND Other = '0' THEN "Middle Eastern, North African"   
    WHEN White = '0' AND Black_African_American = '0' AND American_Indian_Alaska_Native = '0' AND Hawaiian_Pacific_Islander = '0' 
         AND Asian = '0' AND Hispanic_Latino_Spanish = '0' AND Middle_Eastern_North_African = '0' AND Other = '1' THEN "Other"   
    WHEN White = '0' AND Black_African_American = '0' AND American_Indian_Alaska_Native = '0' AND Hawaiian_Pacific_Islander = '0' 
         AND Asian = '0' AND Hispanic_Latino_Spanish = '0' AND Middle_Eastern_North_African = '0' AND Other = '0' AND Skipped_Question = '1' THEN "Skipped this question"
    WHEN CAST(White AS INT64) + CAST(Black_African_American AS INT64) + CAST(American_Indian_Alaska_Native AS INT64) + 
         CAST(Hawaiian_Pacific_Islander AS INT64) + CAST(Asian AS INT64) + CAST(Hispanic_Latino_Spanish AS INT64) + 
         CAST(Middle_Eastern_North_African AS INT64) + CAST(Other AS INT64) > 1 THEN "Multi-race"
    ELSE "UNKNOWN"
  END AS Race
FROM module1_v1_v2
WHERE Connect_ID IN UNNEST(`nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.get_consented_verified_connect_ids`());