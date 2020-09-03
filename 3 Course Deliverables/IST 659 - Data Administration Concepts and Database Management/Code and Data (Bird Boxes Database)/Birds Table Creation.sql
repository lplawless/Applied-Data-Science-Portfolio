-- Table Creation
-- Lauren Lawless
-- IST 659
-- 06/04/2019



SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE left(TABLE_NAME, 3) != 'vc_' AND TABLE_NAME NOT IN ('Lab_Log', 'Lab_Test') ORDER BY TABLE_NAME

-- Check for table existance and drop them in reverse order
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'BirdNestMaterial')
BEGIN
	DROP TABLE BirdNestMaterial
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'BirdEggColorPattern')
BEGIN
	DROP TABLE BirdEggColorPattern
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'BirdBodyColorPattern')
BEGIN
	DROP TABLE BirdBodyColorPattern
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'WeatherCondition')
BEGIN
	DROP TABLE WeatherCondition
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'ObservationNote')
BEGIN
	DROP TABLE ObservationNote
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'Observation')
BEGIN
	DROP TABLE Observation
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'NestMaterial')
BEGIN
	DROP TABLE NestMaterial
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'EggPattern')
BEGIN
	DROP TABLE EggPattern
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'Color')
BEGIN
	DROP TABLE Color
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'BodyPattern')
BEGIN
	DROP TABLE BodyPattern
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'Weather')
BEGIN
	DROP TABLE Weather
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'Note')
BEGIN
	DROP TABLE Note
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'Bird')
BEGIN
	DROP TABLE Bird
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'Condition')
BEGIN
	DROP TABLE Condition
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = 'Box')
BEGIN
	DROP TABLE Box
END


-- Create tables with non-foreign-key-dependent tables first
CREATE TABLE Box (
	BoxID INT IDENTITY
	, BoxNumber VARCHAR(3) NOT NULL
	, Field CHAR(1) NOT NULL
	, Latitude DECIMAL(18,4) NOT NULL
	, Longitude DECIMAL(18,4) NOT NULL
	, TrailOrder INT NOT NULL
	, CONSTRAINT Box_PK PRIMARY KEY (BoxID)
	, CONSTRAINT Box_U1 UNIQUE (BoxNumber)
	, CONSTRAINT Box_U2 UNIQUE (Latitude, Longitude)
	, CONSTRAINT Box_U3 UNIQUE (TrailOrder)
	, CONSTRAINT Box_CHK1 CHECK (left(BoxNumber, 1) = Field)
);

CREATE TABLE Condition (
	ConditionID INT IDENTITY
	, StartDateTime DATETIME NOT NULL
	, EndDateTime DATETIME NOT NULL
	, TemperatureF INT NOT NULL
	, CONSTRAINT Condition_PK PRIMARY KEY (ConditionID)
	, CONSTRAINT Condition_U1 UNIQUE (StartDateTime)
	, CONSTRAINT Condition_U2 UNIQUE (EndDateTime)
	, CONSTRAINT Condition_CHK1 CHECK (StartDateTime < EndDateTime)
	, CONSTRAINT Condition_CHK2 CHECK (convert(DATE, StartDateTime) = convert(DATE, EndDateTime))
);

CREATE TABLE Bird (
	BirdID INT IDENTITY
	, BirdCode CHAR(4) NOT NULL
	, CommonName VARCHAR(50) NOT NULL
	, ScientificName VARCHAR(50) NOT NULL
	, TaxonomicOrder VARCHAR(30) NOT NULL
	, TaxonomicFamily VARCHAR(30) NOT NULL
	, Habitat VARCHAR(30)
	, Nesting VARCHAR(30)
	, Behavior VARCHAR(30)
	, ConservationStatus VARCHAR(30)
	, ClutchSizeLow INT
	, ClutchSizeHigh INT
	, IncubationDaysLow INT
	, IncubationDaysHigh INT
	, HatchedDaysLow INT
	, HatchedDaysHigh INT
	, BroodsPerYearLow INT
	, BroodsPerYearHigh INT
	, CONSTRAINT Bird_PK PRIMARY KEY (BirdID)
	, CONSTRAINT Bird_U1 UNIQUE (BirdCode)
	, CONSTRAINT Bird_U2 UNIQUE (CommonName)
	, CONSTRAINT Bird_U3 UNIQUE (ScientificName)
);

CREATE TABLE Note (
	NoteID INT IDENTITY
	, NoteText VARCHAR(150) NOT NULL
	, CONSTRAINT Note_PK PRIMARY KEY (NoteID)
	, CONSTRAINT Note_U1 UNIQUE (NoteText)
);

CREATE TABLE Weather (
	WeatherID INT IDENTITY
	, WeatherDescription VARCHAR(30) NOT NULL
	, CONSTRAINT Weather_PK PRIMARY KEY (WeatherID)
	, CONSTRAINT Weather_U1 UNIQUE (WeatherDescription)	
);

CREATE TABLE BodyPattern (
	BodyPatternID INT IDENTITY
	, BodyPatternDescription VARCHAR(20) NOT NULL
	, CONSTRAINT BodyPattern_PK PRIMARY KEY (BodyPatternID)
	, CONSTRAINT BodyPattern_U1 UNIQUE (BodyPatternDescription)	
);

CREATE TABLE Color (
	ColorID INT IDENTITY
	, ColorDescription VARCHAR(20) NOT NULL
	, CONSTRAINT Color_PK PRIMARY KEY (ColorID)
	, CONSTRAINT Color_U1 UNIQUE (ColorDescription)
);

CREATE TABLE EggPattern (
	EggPatternID INT IDENTITY
	, EggPatternDescription VARCHAR(20) NOT NULL
	, CONSTRAINT EggPattern_PK PRIMARY KEY (EggPatternID)
	, CONSTRAINT EggPattern_U1 UNIQUE (EggPatternDescription)
);

CREATE TABLE NestMaterial (
	NestMaterialID INT IDENTITY
	, NestMaterialDescription VARCHAR(20) NOT NULL
	, CONSTRAINT NestMaterial_PK PRIMARY KEY (NestMaterialID)
	, CONSTRAINT NestMaterial_U1 UNIQUE (NestMaterialDescription)
);

CREATE TABLE Observation (
	ObservationID INT IDENTITY
	, BoxID INT NOT NULL
	, OccupantID INT
	, NumberEggs INT DEFAULT 0
	, NumberHatched INT DEFAULT 0
	, NumberFledged INT DEFAULT 0
	, ConditionID INT NOT NULL
	, CONSTRAINT Observation_PK PRIMARY KEY (ObservationID)
	, CONSTRAINT Observation_FK1 FOREIGN KEY (BoxID) REFERENCES Box(BoxID)
	, CONSTRAINT Observation_FK2 FOREIGN KEY (OccupantID) REFERENCES Bird(BirdID)
	, CONSTRAINT Observation_FK3 FOREIGN KEY (ConditionID) REFERENCES Condition(ConditionID)
);

CREATE TABLE ObservationNote (
	ObservationNoteID INT IDENTITY
	, ObservationID INT
	, NoteID INT
	, CONSTRAINT ObservationNote_PK PRIMARY KEY (ObservationNoteID)
	, CONSTRAINT ObservationNote_FK1 FOREIGN KEY (ObservationID) REFERENCES Observation(ObservationID)
	, CONSTRAINT ObservationNote_FK2 FOREIGN KEY (NoteID) REFERENCES Note(NoteID)
	, CONSTRAINT ObservationNote_U1 UNIQUE (ObservationID, NoteID)
);

CREATE TABLE WeatherCondition (
	WeatherConditionID INT IDENTITY
	, ConditionID INT
	, WeatherID INT
	, CONSTRAINT WeatherCondition_PK PRIMARY KEY (WeatherConditionID)
	, CONSTRAINT WeatherCondition_FK1 FOREIGN KEY (ConditionID) REFERENCES Condition(ConditionID)
	, CONSTRAINT WeatherCondition_FK2 FOREIGN KEY (WeatherID) REFERENCES Weather(WeatherID)
	, CONSTRAINT WeatherCondition_U1 UNIQUE (ConditionID, WeatherID)
);

CREATE TABLE BirdBodyColorPattern (
	BirdBodyColorPatternID INT IDENTITY
	, BirdID INT
	, Sex CHAR(1)
	, BodyColorID INT
	, BodyPatternID INT
	, CONSTRAINT BirdBodyColorPattern_PK PRIMARY KEY (BirdBodyColorPatternID)
	, CONSTRAINT BirdBodyColorPattern_FK1 FOREIGN KEY (BirdID) REFERENCES Bird(BirdID)
	, CONSTRAINT BirdBodyColorPattern_FK2 FOREIGN KEY (BodyColorID) REFERENCES Color(ColorID)
	, CONSTRAINT BirdBodyColorPattern_FK3 FOREIGN KEY (BodyPatternID) REFERENCES BodyPattern(BodyPatternID)
	, CONSTRAINT BirdBodyColorPattern_U1 UNIQUE (BirdID, Sex, BodyColorID, BodyPatternID)
	, CONSTRAINT BirdBodyColorPattern_CHK1 CHECK (Sex IN ('F', 'M'))
);

CREATE TABLE BirdEggColorPattern (
	BirdEggColorPatternID INT IDENTITY
	, BirdID INT
	, EggColorID INT
	, EggPatternID INT
	, CONSTRAINT BirdEggColorPattern_PK PRIMARY KEY (BirdEggColorPatternID)
	, CONSTRAINT BirdEggColorPattern_FK1 FOREIGN KEY (BirdID) REFERENCES Bird(BirdID)
	, CONSTRAINT BirdEggColorPattern_FK2 FOREIGN KEY (EggColorID) REFERENCES Color(ColorID)
	, CONSTRAINT BirdEggColorPattern_FK3 FOREIGN KEY (EggPatternID) REFERENCES EggPattern(EggPatternID)
	, CONSTRAINT BirdEggColorPattern_U1 UNIQUE (BirdID, EggColorID, EggPatternID)
);

CREATE TABLE BirdNestMaterial (
	BirdNestMaterialID INT IDENTITY
	, BirdID INT
	, NestMaterialID INT
	, CONSTRAINT BirdNestMaterial_PK PRIMARY KEY (BirdNestMaterialID)
	, CONSTRAINT BirdNestMaterial_FK1 FOREIGN KEY (BirdID) REFERENCES Bird(BirdID)
	, CONSTRAINT BirdNestMaterial_FK2 FOREIGN KEY (NestMaterialID) REFERENCES NestMaterial(NestMaterialID)
	, CONSTRAINT BirdNestMaterial_U1 UNIQUE (BirdID, NestMaterialID)
);