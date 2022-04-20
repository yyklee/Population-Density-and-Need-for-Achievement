-- Cleaning Data For ESS DATA: [Creativity in Europe] --


-- Overview ---------------------------------------------------------------------------------------------
USE YProject;

SELECT *
FROM Europe;


-- Check / Deal with Missing Data ------------------------------------------------------------------------------------

-- key variables: ipsuces
SELECT count(*) FROM Europe
WHERE ipsuces IS NULL OR ipsuces = 9; -- 9: "no answer" 


-- key variables: ipshabt (show ability)
SELECT count(*) FROM Europe
WHERE ipshabt IS NULL OR ipshabt = 9; -- 9: "no answer" 


-- no answer (9) to null

SELECT IF(ipsuces = 9, NULL, ipsuces) as ipsucesr
FROM Europe;

SELECT IF(ipshabt = 9, NULL, ipshabt) as ipshabtr
FROM Europe;


-- Populate Property Address data------------------------------------------------------------------------

Select *
From Europe
Where cntry is null
order by ID;



-- Breaking out "name" into Individual Columns (ESS round, idnumber) -----------------------------------------------

SELECT
SUBSTRING(name, 1, 4) as ESSname
, SUBSTRING(name, 5, LENGTH(name)) as ESSnum
FROM Europe;


-- > Update to separate column (ESSname)
ALTER TABLE Europe
Add ESSname Nvarchar(255);

Update Europe
SET ESSname = SUBSTRING(name, 1, 4);

-- > Update to separate column (ESSnum)
ALTER TABLE Europe
Add PropertySplitCity Nvarchar(255);

Update Europe
SET ESSnum = SUBSTRING(name, 5, LENGTH(name));


-- Change 1 and 2 to "M" and "F" in "gndr" field -------------------------------------------------------------


SELECT DISTINCT(gndr), Count(gndr)
FROM Europe
GROUP BY gndr
ORDER BY 2;
-- (Male = 1, Female = 2 in original data/ Binary)

WITH cte1 AS 
(
SELECT gndr
, CASE WHEN gndr = '1' THEN 'Male'
	   ELSE 'Female'
	   END as gender
       FROM Europe
       
       )
SELECT *
FROM cte1;
-- (new column: gender)


UPDATE Europe
SET gender = CASE WHEN gndr = '1' THEN 'Male'
	   WHEN gndr = '2' THEN 'Female' ELSE gndr
	   END;
-- (updating new column to Europe as gender)



-- Check Duplicates -----------------------------------------------------------------------------------------

WITH u_Europe AS(
Select *,
	ROW_NUMBER() OVER (
	PARTITION BY ID,
				 gndr,
				 age,
				 chld,
                 eduyrs
				 ORDER BY 
                 ID,
                 gndr,
				 age,
				 chld,
                 eduyrs
					) AS row_num

FROM Europe

)
SELECT *
FROM u_Europe
WHERE row_num > 1;


-- No duplicates


-- Delete Unused Columns -------------------------------------------------------------------------------

Select *
From Europe; 


ALTER TABLE Europe
DROP COLUMN nuts1,
DROP COLUMN nuts2;

















