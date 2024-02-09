-- Problem set 3, question 5 SQL script

-- Create the table structure into which we will import the csv
CREATE TABLE insurance_data (
    "policyID" INTEGER,
    "statecode" TEXT,
    "county" TEXT,
    "eq_site_limit" REAL,
    "hu_site_limit" REAL,
    "fl_site_limit" REAL,
    "fr_site_limit" REAL,
    "tiv_2011" REAL,
    "tiv_2012" REAL,
    "eq_site_deductible" REAL,
    "hu_site_deductible" REAL,
    "fl_site_deductible" REAL,
    "fr_site_deductible" REAL,
    "point_latitude" REAL,
    "point_longitude" REAL,
    "line" TEXT,
    "construction" TEXT,
    "point_granularity" INTEGER
);

-- Prep SQL for a csv and import the Florida Insurance Data into the created table
.mode csv
.import FL_insurance_sample.csv insurance_data

-- Print out the first 10 rows of the data set
SELECT * FROM insurance_data LIMIT 10;

-- List unique values of the county variable
SELECT DISTINCT county FROM insurance_data;

-- Compute the average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) AS average_appreciation FROM insurance_data;

--  Create a frequency table of the construction variable
SELECT construction, COUNT(*) AS frequency FROM insurance_data GROUP BY construction;

