-- Stored Procedures Test Cases
-- Lauren Lawless
-- IST 659
-- 06/10/2019



-- 1 Test AddBirdBasic
EXECUTE AddBirdBasic 'TEST', 'Test Bird', 'Testicus Birdicus', 'Testimontin', 'Testimonti' -- pass

-- test ERRORs (BirdCode, CommonName, ScientificName)
EXECUTE AddBirdBasic 'TEST', 'Test Bird1', 'Testicus Birdicus1', 'Testimontin', 'Testimonti' -- fail: BirdCode

EXECUTE AddBirdBasic 'TiST', 'Test Bird', 'Testicus Birdicus1', 'Testimontin', 'Testimonti' -- fail: CommonName

EXECUTE AddBirdBasic 'TiST', 'Test Bird1', 'Testicus Birdicus', 'Testimontin', 'Testimonti' -- fail: ScientificName

EXECUTE AddBirdBasic 'TiST', 'Test Bird1', 'Testicus Birdicus1', 'Testimontin', 'Testimonti' -- pass

SELECT * FROM Bird

DELETE FROM Bird WHERE BirdCode = 'TiST'



-- 2 Test AddBirdExtras
-- pass some attributes; leave some blank
EXECUTE AddBirdExtras 'TEST', 'litter box', 'cavity', 'sleep', 'extinct in the wild', 1, NULL, NULL, NULL, 4, NULL, NULL, NULL -- pass

SELECT * FROM Bird

-- overwrite already filled attributes (Nesting, HatchedDaysLow); leave old attributes by passing NULL (ClutchSizeLow)
EXECUTE AddBirdExtras 'TEST', 'litter box', 'cat tower', 'sleep', 'extinct in the wild', NULL, NULL, 2, NULL, 5, NULL, NULL, NULL -- pass

SELECT * FROM Bird

-- test ERROR
EXECUTE AddBirdExtras 'TiST', 'habitat', 'nesting', 'behavior', 'conservation', 1, 2, 3, 4, 5, 6, 7, 8 -- fail: BirdCode



-- 3 Test AddBirdLong
EXECUTE AddBirdLong 'TIST', 'Test Bird 1', 'Testicus Birdicus 1', 'Testimontin', 'Testimonti', 'arctic tundra', 'field', 'aquatic hunter', 'climate threatened', 1,2,1,2,1,2,1,2 -- pass

-- test ERRORs (BirdCode, CommonName, ScientificName)
EXECUTE AddBirdLong 'TIST', 'Test Bird 2', 'Testicus Birdicus 2', 'Testimontin', 'Testimonti', 'arctic tundra', 'field', 'aquatic hunter', 'climate threatened', 1,2,1,2,1,2,1,2 -- fail: BirdCode

EXECUTE AddBirdLong 'TAST', 'Test Bird 1', 'Testicus Birdicus 2', 'Testimontin', 'Testimonti', 'arctic tundra', 'field', 'aquatic hunter', 'climate threatened', 1,2,1,2,1,2,1,2 -- fail: CommonName

EXECUTE AddBirdLong 'TAST', 'Test Bird 2', 'Testicus Birdicus 1', 'Testimontin', 'Testimonti', 'arctic tundra', 'field', 'aquatic hunter', 'climate threatened', 1,2,1,2,1,2,1,2 -- fail: ScientificName

EXECUTE AddBirdLong 'TAST', 'Test Bird 2', 'Testicus Birdicus 2', 'Testimontin', 'Testimonti', 'arctic tundra', 'field', 'aquatic hunter', 'climate threatened', 1,2,1,2,1,2,1,2 -- pass

SELECT * FROM Bird

DELETE FROM Bird WHERE BirdCode IN ('TAST', 'TIST')



-- 4 Test AddBirdBodyColorPatternCombination
EXECUTE AddBirdBodyColorPatternCombination 'TEST', 'M', 'purple', 'feet' -- pass

EXECUTE AddBirdBodyColorPatternCombination 'TEST', 'M', 'white', 'wing tips' -- pass

SELECT * FROM Color ORDER BY ColorID
SELECT * FROM BodyPattern ORDER BY BodyPatternID
SELECT bbcp.* FROM BirdBodyColorPattern bbcp JOIN Bird bi ON bbcp.BirdID = bi.BirdID WHERE bi.BirdCode = 'TEST'

-- test ERRORs (BirdCode, Sex)
EXECUTE AddBirdBodyColorPatternCombination 'TaST', 'M', 'purple', 'feet' -- fail: BirdCode

EXECUTE AddBirdBodyColorPatternCombination 'TEST', 's', 'purple', 'feet' -- fail: Sex

EXECUTE AddBirdBodyColorPatternCombination 'TEST', 'M', 'purple', 'feet' -- fail: duplicate

EXECUTE AddBirdBodyColorPatternCombination 'TEST', 'F', 'purple', 'feet' -- pass



-- 5 Test AddBox
SELECT * FROM Box ORDER BY TrailOrder

EXECUTE AddBox 'A12', 4.1298, -12.3949, 30 -- pass

SELECT * FROM Box ORDER BY TrailOrder

-- test ERRORs (BoxNumber, Lat/Long)
EXECUTE AddBox 'A12', 4.1297, -12.3949, 30 -- fail: BoxNumber

EXECUTE AddBox 'A11', 4.1297, -12.3949, 30 -- pass

EXECUTE AddBox 'A10', 4.1297, -12.3949, 30 -- fail: Lat/Long

EXECUTE AddBox 'A10', 4.1297, -12.3950, 30 -- pass

SELECT * FROM Box ORDER BY TrailOrder



-- 6 Test AddCondition
EXECUTE AddCondition '2019', '02', '12', '06:31', '13:20', 28 -- pass
SELECT * FROM Condition

-- test ERRORs (Duplicate Start, duplicate End, duplicate Date, Start before End)
EXECUTE AddCondition '2019', '02', '12', '06:31', '12:20', 28 -- fail: StartDateTime
EXECUTE AddCondition '2019', '02', '12', '05:31', '13:20', 28 -- fail: EndDateTime
EXECUTE AddCondition '2019', '02', '12', '07:31', '14:20', 28 -- fail: Date
EXECUTE AddCondition '2019', '02', '13', '15:31', '12:20', 28 -- fail: Start after End
EXECUTE AddCondition '2019', '02', '13', '11:31', '12:20', 28 -- pass
SELECT * FROM Condition



-- 7 Test AddWeatherCondition
EXECUTE AddWeatherCondition '2019-02-12', 'snow' -- pass

EXECUTE AddWeatherCondition '2019-02-12', 'overcast' -- pass

SELECT * FROM Condition c JOIN WeatherCondition wc ON c.ConditionID = wc.ConditionID JOIN Weather w ON wc.WeatherID = w.WeatherID WHERE c.StartDateTime > '2019-01-01'

-- test ERROR (Date)
EXECUTE AddWeatherCondition '2019-02-14', 'snow' -- fail: Date



-- 8 Test AddObservation
EXECUTE AddObservation '2019-02-12', 'A11', 'TEST', 0, 0, 0 -- pass

-- test ERRORs (Date, BoxNumber, BirdCode, duplicate Date/Box)
EXECUTE AddObservation '2019-02-12', 'A12', 'TEST', NULL, NULL, NULL -- fail: NULL Eggs, Hatched, Fledged

EXECUTE AddObservation '2019-02-14', 'A11', 'TEST', 0, 0, 0 -- fail: Date

EXECUTE AddObservation '2019-02-12', 'A9', 'TEST', 0, 0, 0 -- fail: BoxNumber

EXECUTE AddObservation '2019-02-12', 'A10', 'TiST', 0, 0, 0 -- fail: BirdCode

EXECUTE AddObservation '2019-02-12', 'A11', 'TEST', 0, 0, 0 -- fail: duplicate Date/BoxNumber

EXECUTE AddObservation '2019-02-13', 'A11', 'TEST', 0, 0, 0 -- pass

SELECT * FROM Observation o JOIN Bird bi ON o.OccupantID = bi.BirdID WHERE bi.BirdCode = 'TEST'



-- 9 Test AddObservationNote
EXECUTE AddObservationNote '2019-02-12', 'A11', 'bird wintering in box'

EXECUTE AddObservationNote '2019-02-12', 'A11', 'no eggs'

SELECT o.*
	, bi.ScientificName
	, n.NoteText
FROM Observation o
JOIN ObservationNote obn
ON o.ObservationID = obn.ObservationID
JOIN Note n
ON obn.NoteID = n.NoteID
JOIN Bird bi
ON o.OccupantID = bi.BirdID
WHERE bi.BirdCode = 'TEST'

-- test ERRORs (Date/BoxNumber doesn't exist, note already exists for Observation)
EXECUTE AddObservationNote '2019-02-14', 'A11', 'no eggs' -- fail: Date

EXECUTE AddObservationNote '2019-02-12', 'A9', 'no eggs' -- fail: BoxNumber

EXECUTE AddObservationNote '2019-02-12', 'A11', 'no eggs' -- fail: duplicate Note/Observation



-- 10 Test AddBirdEggColorPatternCombination
EXECUTE AddBirdEggColorPatternCombination 'TEST', 'aubergine', 'stripes' -- pass

EXECUTE AddBirdEggColorPatternCombination 'TEST', 'white', 'base' -- pass

SELECT * FROM Color ORDER BY ColorID
SELECT * FROM EggPattern ORDER BY EggPatternID
SELECT becp.* FROM BirdEggColorPattern becp JOIN Bird bi ON becp.BirdID = bi.BirdID WHERE bi.BirdCode = 'TEST'

-- test ERRORs (BirdCode, Sex)
EXECUTE AddBirdEggColorPatternCombination 'TiST', 'aubergine', 'stripes' -- fail: BirdCode

EXECUTE AddBirdEggColorPatternCombination 'TEST', 'aubergine', 'stripes' -- fail: duplicate



-- 11 Test AddBirdNestMaterial
EXECUTE AddBirdNestMaterialCombination 'TEST', 'feathers' -- pass

EXECUTE AddBirdNestMaterialCombination 'TEST', 'marbles' -- pass

SELECT * FROM NestMaterial ORDER BY NestMaterialID
SELECT bnm.* FROM BirdNestMaterial bnm JOIN Bird bi ON bnm.BirdID = bi.BirdID WHERE bi.BirdCode = 'TEST'

-- test ERRORs (BirdCode, Sex)
EXECUTE AddBirdNestMaterialCombination 'TiST', 'marbles' -- fail: BirdCode

EXECUTE AddBirdNestMaterialCombination 'TEST', 'marbles' -- fail: duplicate



-- 12 delete TEST rows
-- BirdNestMaterial, NestMaterial
DELETE FROM BirdNestMaterial WHERE BirdID IN (SELECT BirdID FROM Bird WHERE BirdCode = 'TEST')
DELETE FROM NestMaterial WHERE NestMaterialID NOT IN (SELECT NestMaterialID FROM BirdNestMaterial)

-- BirdEggColorPattern, BirdBodyColorPattern, Color, BodyPattern, EggPattern
DELETE FROM BirdEggColorPattern WHERE BirdID IN (SELECT BirdID FROM Bird WHERE BirdCode = 'TEST')
DELETE FROM BirdBodyColorPattern WHERE BirdID IN (SELECT BirdID FROM Bird WHERE BirdCode = 'TEST')
DELETE FROM Color WHERE ColorID NOT IN (SELECT BodyColorID FROM BirdBodyColorPattern) AND ColorID NOT IN (SELECT EggColorID FROM BirdEggColorPattern)
DELETE FROM BodyPattern WHERE BodyPatternID NOT IN (SELECT BodyPatternID FROM BirdBodyColorPattern)
DELETE FROM EggPattern WHERE EggPatternID NOT IN (SELECT EggPatternID FROM BirdEggColorPattern)

-- ObservationNote, Note, Observation
DELETE FROM ObservationNote WHERE ObservationID IN (SELECT ObservationID FROM Observation o JOIN Condition c ON o.ConditionID = c.ConditionID WHERE StartDateTime > '2019-01-01')
DELETE FROM Note WHERE NoteID NOT IN (SELECT NoteID FROM ObservationNote)
DELETE FROM Observation WHERE ConditionID IN (SELECT ConditionID FROM Condition WHERE StartDateTime > '2019-01-01')

-- WeatherCondition, Weather, Condition
DELETE FROM WeatherCondition WHERE ConditionID IN (SELECT ConditionID FROM Condition WHERE StartDateTime > '2019-01-01')
DELETE FROM Weather WHERE WeatherID NOT IN (SELECT WeatherID FROM WeatherCondition)
DELETE FROM Condition WHERE StartDateTime > '2019-01-01'

-- Bird
DELETE FROM Bird WHERE BirdCode = 'TEST'



-- 13 Test DeleteBox
EXECUTE DeleteBox 'A12'

SELECT * FROM Box ORDER BY TrailOrder

EXECUTE DeleteBox 'A11'

SELECT * FROM Box ORDER BY TrailOrder

EXECUTE DeleteBox 'A10'

SELECT * FROM Box ORDER BY TrailOrder

-- test ERROR (BoxNumber)
EXECUTE DeleteBox 'Z33' -- fail: BoxNumber