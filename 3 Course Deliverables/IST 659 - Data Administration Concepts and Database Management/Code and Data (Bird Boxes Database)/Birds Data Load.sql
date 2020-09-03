-- Transform and load data from temporary tables
-- Lauren Lawless
-- IST 659
-- 06/09/2019



INSERT INTO Box(BoxNumber, Field, Latitude, Longitude, TrailOrder)
SELECT BoxNumber, Field, Latitude, Longitude, TrailOrder FROM Box$

INSERT INTO Condition(StartDateTime, EndDateTime, TemperatureF)
SELECT StartDateTime, EndDateTime, TemperatureF FROM Condition$

INSERT INTO Bird(BirdCode, CommonName, ScientificName, TaxonomicOrder, TaxonomicFamily, Habitat, Nesting, Behavior, ConservationStatus, ClutchSizeLow, ClutchSizeHigh, IncubationDaysLow, IncubationDaysHigh, HatchedDaysLow, HatchedDaysHigh, BroodsPerYearLow, BroodsPerYearHigh)
SELECT BirdCode, CommonName, ScientificName, TaxonomicOrder, TaxonomicFamily, Habitat, Nesting, Behavior, ConservationStatus, ClutchSizeLow, ClutchSizeHigh, IncubationDaysLow, IncubationDaysHigh, HatchedDaysLow, HatchedDaysHigh, BroodsPerYearLow, BroodsPerYearHigh FROM Bird$

INSERT INTO Note(NoteText)
SELECT NoteText FROM Note$

INSERT INTO Weather(WeatherDescription)
SELECT WeatherDescription FROM Weather$

INSERT INTO BodyPattern(BodyPatternDescription)
SELECT BodyPatternDescription FROM BodyPattern$

INSERT INTO Color(ColorDescription)
SELECT ColorDescription FROM Color$

INSERT INTO EggPattern(EggPatternDescription)
SELECT EggPatternDescription FROM EggPattern$

INSERT INTO NestMaterial(NestMaterialDescription)
SELECT NestMaterialDescription FROM NestMaterial$

INSERT INTO Observation(BoxID, OccupantID, NumberEggs, NumberHatched, NumberFledged, ConditionID)
SELECT BoxID, OccupantID, NumberEggs, NumberHatched, NumberFledged, ConditionID FROM Observation$

INSERT INTO ObservationNote(ObservationID, NoteID)
SELECT ObservationID, NoteID FROM ObservationNote$

INSERT INTO WeatherCondition(ConditionID, WeatherID)
SELECT ConditionID, WeatherID FROM WeatherConditions$

INSERT INTO BirdBodyColorPattern(BirdID, Sex, BodyColorID, BodyPatternID)
SELECT BirdID, Sex, BodyColorID, BodyPatternID FROM BirdBodyColorPattern$

INSERT INTO BirdEggColorPattern(BirdID, EggColorID, EggPatternID)
SELECT BirdID, EggColorID, EggPatternID FROM BirdEggColorPattern$

INSERT INTO BirdNestMaterial(BirdID, NestMaterialID)
SELECT BirdID, NestMaterialID FROM BirdNestMaterial$


--DROP TABLE Bird$
--DROP TABLE BirdBodyColorPattern$
--DROP TABLE BirdEggColorPattern$
--DROP TABLE BirdNestMaterial$
--DROP TABLE BodyPattern$
--DROP TABLE Box$
--DROP TABLE Color$
--DROP TABLE Condition$
--DROP TABLE EggPattern$
--DROP TABLE NestMaterial$
--DROP TABLE Note$
--DROP TABLE Observation$
--DROP TABLE ObservationNote$
--DROP TABLE Weather$
--DROP TABLE WeatherConditions$
