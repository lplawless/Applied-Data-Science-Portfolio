-- Create Views
-- Lauren Lawless
-- IST 659
-- 06/09/2019



SELECT * FROM INFORMATION_SCHEMA.VIEWS WHERE left(TABLE_NAME, 3) != 'vc_'



IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.VIEWS WHERE TABLE_NAME = 'LastUpdate')
BEGIN
	DROP View LastUpdate
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.VIEWS WHERE TABLE_NAME = 'WeeklySummary')
BEGIN
	DROP View WeeklySummary
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.VIEWS WHERE TABLE_NAME = 'EmptyBoxes')
BEGIN
	DROP View EmptyBoxes
END
GO



CREATE VIEW LastUpdate AS (
SELECT convert(DATE, c.StartDateTime) AS ObservationDate
	, bo.BoxNumber
	, bi.CommonName AS Occupant
--	, sum(o.NumberEggs) AS Eggs
--	, sum(o.NumberHatched) AS Hatched
--	, sum(o.NumberFledged) AS Fledged
	, NumberEggs AS Eggs
	, NumberHatched AS Hatched
	, NumberFledged AS Fledged
FROM Observation o
LEFT JOIN Bird bi
ON o.OccupantID = bi.BirdID
JOIN Box bo
ON o.BoxID = bo.BoxID
JOIN Condition c
ON o.ConditionID = c.ConditionID
WHERE c.StartDateTime = (SELECT max(StartDateTime) FROM Condition)
AND o.OccupantID IS NOT NULL
--GROUP BY rollup(convert(DATE, c.StartDateTime), bi.CommonName, bo.BoxNumber)
)
GO



CREATE VIEW WeeklySummary AS (
SELECT convert(DATE, c.StartDateTime) AS "Date"
	, sum(o.NumberEggs) AS TotalEggs
	, sum(o.NumberHatched) AS TotalHatched
	, sum(o.NumberFledged) AS TotalFledged
	, sum(o.NumberEggs+o.NumberHatched+o.NumberFledged) AS TotalFutureBirds
FROM Condition c
JOIN Observation o
ON c.ConditionID = o.ConditionID
GROUP BY convert(DATE, StartDateTime)
)
GO



CREATE VIEW EmptyBoxes AS (
SELECT bo.BoxNumber
	--, bo.Latitude
	--, bo.Longitude
	--, sum(o.NumberEggs) AS TotalEggs
	, sum(CASE WHEN NumberEggs+NumberHatched+NumberFledged = 0 THEN 1 ELSE 0 END) AS WeeksEmpty
FROM Observation o
JOIN (SELECT ConditionID
		FROM Condition
		WHERE datepart(YEAR, StartDateTime) = datepart(YEAR, getdate())-1
	) c
ON o.ConditionID = c.ConditionID
JOIN Box bo
ON o.BoxID = bo.BoxID
GROUP BY bo.BoxNumber
--HAVING sum(CASE WHEN NumberEggs+NumberHatched+NumberFledged = 0 THEN 1 ELSE 0 END) >= 6
)
GO