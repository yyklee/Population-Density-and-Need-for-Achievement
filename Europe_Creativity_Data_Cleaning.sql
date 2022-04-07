-- Cleaning Data For ESS DATA: [Creativity in Europe] --


-- Overview ---------------------------------------------------------------------------------------------
USE YProject;

SELECT *
FROM Europe;


-- Check / Deal with Missing Data ------------------------------------------------------------------------------------

-- > 1. key variable: ipcrtiv (important to be creative) 

SELECT * FROM Europe
WHERE ipcrtiv IS NULL; 
-- > There is no null data 
-- > 9: "NO ANSWER" 


-- > 2. Other Variables with Missing Data?  

SELECT * FROM Europe
WHERE happy >= 11; 


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

WITH DD AS (
SELECT gndr
, CASE WHEN gndr = '1' THEN 'Male'
	   ELSE 'Female'
	   END as gender
       FROM Europe
       )
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

From Europe
)

SELECT *
From u_Europe
Where row_num > 1;

-- > No duplicates



-- Delete Unused Columns ---------------------------------------------------------------------------------------------------


Select *
From Europe; 


ALTER TABLE Europe
DROP COLUMN nuts1,
DROP COLUMN nuts2;
















-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

--- Importing Data using OPENROWSET and BULK INSERT	

--  More advanced and looks cooler, but have to configure server appropriately to do correctly
--  Wanted to provide this in case you wanted to try it


--sp_configure 'show advanced options', 1;
--RECONFIGURE;
--GO
--sp_configure 'Ad Hoc Distributed Queries', 1;
--RECONFIGURE;
--GO


--USE PortfolioProject 

--GO 

--EXEC master.dbo.sp_MSset_oledb_prop N'Microsoft.ACE.OLEDB.12.0', N'AllowInProcess', 1 

--GO 

--EXEC master.dbo.sp_MSset_oledb_prop N'Microsoft.ACE.OLEDB.12.0', N'DynamicParameters', 1 

--GO 


---- Using BULK INSERT

--USE PortfolioProject;
--GO
--BULK INSERT nashvilleHousing FROM 'C:\Temp\SQL Server Management Studio\Nashville Housing Data for Data Cleaning Project.csv'
--   WITH (
--      FIELDTERMINATOR = ',',
--      ROWTERMINATOR = '\n'
--);
--GO


---- Using OPENROWSET
--USE PortfolioProject;
--GO
--SELECT * INTO nashvilleHousing
--FROM OPENROWSET('Microsoft.ACE.OLEDB.12.0',
--    'Excel 12.0; Database=C:\Users\alexf\OneDrive\Documents\SQL Server Management Studio\Nashville Housing Data for Data Cleaning Project.csv', [Sheet1$]);
--GO







