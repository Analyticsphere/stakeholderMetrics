CREATE OR REPLACE FUNCTION `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.get_consented_verified_connect_ids`()
RETURNS ARRAY<STRING>
AS (
  (  -- Selecting Connect_IDs based on the given conditions
  SELECT ARRAY_AGG(Connect_ID)
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP`
  WHERE d_821247024 = '197316935' AND d_747006172 != '353358909'
  )
);


CREATE OR REPLACE FUNCTION `nih-nci-dceg-connect-bq2-prod.StakeHolderMetrics_RS.get_common_module1_connect_ids`()
RETURNS ARRAY<STRING>
AS (
  (
    SELECT ARRAY_AGG(a.Connect_ID)
    FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.module1_v1_JP` a
    INNER JOIN `nih-nci-dceg-connect-prod-6d04.FlatConnect.module1_v2_JP` b
    ON a.Connect_ID = b.Connect_ID
  )
);