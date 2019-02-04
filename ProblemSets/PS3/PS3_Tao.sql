CREATE TABLE "insurance" (
PolicyID INTEGER,
Statecode CHAR,
County CHAR,
Eq_site_limit INTEGER,
Hu_site_limit INTEGER,
Fl_site_limit INTEGER,
Fr_site_limit INTEGER,
Tiv_2011 INTEGER,
Tiv_2012 INTEGER,
Eq_site_deductible INTEGER,
Hu_site_deductible INTEGER,
Fl_site_deductible INTEGER,
Fr_site_deductible INTEGER,
Point_latitude INTEGER,
Point_longitude INTEGER,
Line CHAR,
Construction CHAR,
Point_granularity INTEGER
);

.mode csv
.import FL_insurance_sample.csv insurance

Select * from insurance limit 10;

SELECT DISTINCT county FROM insurance;

SELECT AVG(tiv_2012) FROM insurance;
SELECT AVG(tiv_2011) FROM insurance;
SELECT  AVG(tiv_2012-tiv_2011) FROM insurance;

SELECT construction, COUNT(*) FROM insurance GROUP BY construction;

 
