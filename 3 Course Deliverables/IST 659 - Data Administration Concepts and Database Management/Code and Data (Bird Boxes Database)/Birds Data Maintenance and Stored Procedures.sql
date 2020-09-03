-- Data Maintenance/Stored Procedures
-- Lauren Lawless
-- IST 659
-- 06/04/2019



SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND left(ROUTINE_NAME, 3) != 'vc_' ORDER BY ROUTINE_NAME

-- Check for procedure existance and drop them
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddBirdBasic')
BEGIN
	DROP PROCEDURE AddBirdBasic
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddBirdLong')
BEGIN
	DROP PROCEDURE AddBirdLong
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddBirdExtras')
BEGIN
	DROP PROCEDURE AddBirdExtras
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddBirdBodyColorPatternCombination')
BEGIN
	DROP PROCEDURE AddBirdBodyColorPatternCombination
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddBirdEggColorPatternCombination')
BEGIN
	DROP PROCEDURE AddBirdEggColorPatternCombination
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddBirdNestMaterialCombination')
BEGIN
	DROP PROCEDURE AddBirdNestMaterialCombination
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddBox')
BEGIN
	DROP PROCEDURE AddBox
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'DeleteBox')
BEGIN
	DROP PROCEDURE DeleteBox
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddCondition')
BEGIN
	DROP PROCEDURE AddCondition
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddWeatherCondition')
BEGIN
	DROP PROCEDURE AddWeatherCondition
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddObservation')
BEGIN
	DROP PROCEDURE AddObservation
END

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_TYPE = 'PROCEDURE' AND ROUTINE_NAME = 'AddObservationNote')
BEGIN
	DROP PROCEDURE AddObservationNote
END
GO


-- Add new Bird with only required attributes
CREATE PROCEDURE AddBirdBasic(@BirdCode CHAR(4), @CommonName VARCHAR(50), @ScientificName VARCHAR(50), @TaxOrder VARCHAR(30), @TaxFamily VARCHAR(30))
AS
BEGIN

	IF EXISTS (SELECT * FROM Bird WHERE BirdCode = @BirdCode OR CommonName = @CommonName OR ScientificName = @ScientificName)
		BEGIN
			SELECT CASE WHEN BirdCode = @BirdCode THEN 'ERROR: BirdCode already exists'
						WHEN CommonName = @CommonName THEN 'ERROR: CommonName already exists'
						WHEN ScientificName = @ScientificName THEN 'ERROR: ScientificName already exists'
						END
			FROM Bird
			WHERE BirdCode = @BirdCode
			OR CommonName = @CommonName
			OR ScientificName = @ScientificName
		END
	ELSE
		BEGIN
			INSERT INTO Bird (BirdCode, CommonName, ScientificName, TaxonomicOrder, TaxonomicFamily)
			VALUES (@BirdCode, @CommonName, @ScientificName, @TaxOrder, @TaxFamily)
		END
	
	RETURN @@identity
END
GO


-- Add new Bird with all attributes
CREATE PROCEDURE AddBirdLong(@BirdCode CHAR(4), @CommonName VARCHAR(50), @ScientificName VARCHAR(50), @TaxOrder VARCHAR(30), @TaxFamily VARCHAR(30), @Habitat VARCHAR(30), @Nesting VARCHAR(30), @Behavior VARCHAR(30), @ConsStat VARCHAR(30), @ClutchLow INT, @ClutchHigh INT, @IncubLow INT, @IncubHigh INT, @HatchLow INT, @HatchHigh INT, @BroodsLow INT, @BroodsHigh INT)
AS
BEGIN

	IF EXISTS (SELECT * FROM Bird WHERE BirdCode = @BirdCode OR CommonName = @CommonName OR ScientificName = @ScientificName)
		BEGIN
			SELECT CASE WHEN BirdCode = @BirdCode THEN 'ERROR: BirdCode already exists'
						WHEN CommonName = @CommonName THEN 'ERROR: CommonName already exists'
						WHEN ScientificName = @ScientificName THEN 'ERROR: ScientificName already exists'
						END
			FROM Bird
			WHERE BirdCode = @BirdCode
			OR CommonName = @CommonName
			OR ScientificName = @ScientificName
		END
	ELSE
		BEGIN
			INSERT INTO Bird (BirdCode, CommonName, ScientificName, TaxonomicOrder, TaxonomicFamily, Habitat, Nesting, Behavior, ConservationStatus, ClutchSizeLow, ClutchSizeHigh, IncubationDaysLow, IncubationDaysHigh, HatchedDaysLow, HatchedDaysHigh, BroodsPerYearLow, BroodsPerYearHigh)
			VALUES (@BirdCode, @CommonName, @ScientificName, @TaxOrder, @TaxFamily, @Habitat, @Nesting, @Behavior, @ConsStat, @ClutchLow, @ClutchHigh, @IncubLow, @IncubHigh, @HatchLow, @HatchHigh, @BroodsLow, @BroodsHigh)
		END

	RETURN @@identity
END
GO


-- Update extra Bird attributes (after AddBirdBasic)
CREATE PROCEDURE AddBirdExtras(@BirdCode CHAR(4), @Habitat VARCHAR(30), @Nesting VARCHAR(30), @Behavior VARCHAR(30), @ConsStat VARCHAR(30), @ClutchLow INT, @ClutchHigh INT, @IncubLow INT, @IncubHigh INT, @HatchLow INT, @HatchHigh INT, @BroodsLow INT, @BroodsHigh INT)
AS
BEGIN

	IF EXISTS (SELECT * FROM Bird WHERE BirdCode = @BirdCode)
		BEGIN
			UPDATE Bird
			SET Habitat = CASE WHEN @Habitat IS NULL THEN (SELECT Habitat FROM Bird WHERE BirdCode = @BirdCode) ELSE @Habitat END
				, Nesting = CASE WHEN @Nesting IS NULL THEN (SELECT Nesting FROM Bird WHERE BirdCode = @BirdCode) ELSE @Nesting END
				, Behavior = CASE WHEN @Behavior IS NULL THEN (SELECT Behavior FROM Bird WHERE BirdCode = @BirdCode) ELSE @Behavior END
				, ConservationStatus = CASE WHEN @ConsStat IS NULL THEN (SELECT ConservationStatus FROM Bird WHERE BirdCode = @BirdCode) ELSE @ConsStat END
				, ClutchSizeLow = CASE WHEN @ClutchLow IS NULL THEN (SELECT ClutchSizeLow FROM Bird WHERE BirdCode = @BirdCode) ELSE @ClutchLow END
				, ClutchSizeHigh = CASE WHEN @ClutchHigh IS NULL THEN (SELECT ClutchSizeHigh FROM Bird WHERE BirdCode = @BirdCode) ELSE @ClutchHigh END
				, IncubationDaysLow = CASE WHEN @IncubLow IS NULL THEN (SELECT IncubationDaysLow FROM Bird WHERE BirdCode = @BirdCode) ELSE @IncubLow END
				, IncubationDaysHigh = CASE WHEN @IncubHigh IS NULL THEN (SELECT IncubationDaysHigh FROM Bird WHERE BirdCode = @BirdCode) ELSE @IncubHigh END
				, HatchedDaysLow = CASE WHEN @HatchLow IS NULL THEN (SELECT HatchedDaysLow FROM Bird WHERE BirdCode = @BirdCode) ELSE @HatchLow END
				, HatchedDaysHigh = CASE WHEN @HatchHigh IS NULL THEN (SELECT HatchedDaysHigh FROM Bird WHERE BirdCode = @BirdCode) ELSE @HatchHigh END
				, BroodsPerYearLow = CASE WHEN @BroodsLow IS NULL THEN (SELECT BroodsPerYearLow FROM Bird WHERE BirdCode = @BirdCode) ELSE @BroodsLow END
				, BroodsPerYearHigh = CASE WHEN @BroodsHigh IS NULL THEN (SELECT BroodsPerYearHigh FROM Bird WHERE BirdCode = @BirdCode) ELSE @BroodsHigh END
			WHERE BirdCode = @BirdCode
		END
	ELSE
		BEGIN
			SELECT 'ERROR: BirdCode does not exist'
		END
	
	RETURN @@identity
END
GO


-- Add Bird Body Color Pattern combinations
CREATE PROCEDURE AddBirdBodyColorPatternCombination(@BirdCode CHAR(4), @Sex CHAR(1), @Color VARCHAR(20), @Pattern VARCHAR(20))
AS
BEGIN

	DECLARE @BirdID INT
		, @ColorID INT
		, @PatternID INT

	IF @Sex IN ('F', 'M')
		BEGIN
			IF EXISTS (SELECT * FROM Bird WHERE BirdCode = @BirdCode)
				BEGIN
					SELECT @BirdID = BirdID
					FROM Bird
					WHERE BirdCode = @BirdCode
					
					IF EXISTS (SELECT * FROM Color WHERE ColorDescription = @Color)
						BEGIN
							SELECT @ColorID = ColorID
							FROM Color
							WHERE ColorDescription = @Color
									
							IF EXISTS (SELECT * FROM BodyPattern WHERE BodyPatternDescription = @Pattern)
								BEGIN
									SELECT @PatternID = BodyPatternID
									FROM BodyPattern
									WHERE BodyPatternDescription = @Pattern
									
									IF EXISTS (SELECT * FROM BirdBodyColorPattern WHERE BirdID = @BirdID AND Sex = @Sex AND BodyColorID = @ColorID AND BodyPatternID = @PatternID)
										BEGIN
											SELECT 'ERROR: That BirdCode, Sex, Color, and Pattern (body) combination already exists'
										END
									ELSE
										BEGIN
											INSERT INTO BirdBodyColorPattern(BirdID, Sex, BodyColorID, BodyPatternID)
											VALUES (@BirdID, @Sex, @ColorID, @PatternID)
										END
								END
							ELSE
								BEGIN
									INSERT INTO BodyPattern(BodyPatternDescription)
									VALUES (@Pattern)
									
									SELECT @PatternID = BodyPatternID
									FROM BodyPattern
									WHERE BodyPatternDescription = @Pattern
									
									INSERT INTO BirdBodyColorPattern(BirdID, Sex, BodyColorID, BodyPatternID)
									VALUES (@BirdID, @Sex, @ColorID, @PatternID)
								END
						END
					ELSE
						BEGIN
							INSERT INTO Color(ColorDescription)
							VALUES (@Color)
							
							SELECT @ColorID = ColorID
							FROM Color
							WHERE ColorDescription = @Color
							
							IF EXISTS (SELECT * FROM BodyPattern WHERE BodyPatternDescription = @Pattern)
								BEGIN
									SELECT @PatternID = BodyPatternID
									FROM BodyPattern
									WHERE BodyPatternDescription = @Pattern
									
									INSERT INTO BirdBodyColorPattern(BirdID, Sex, BodyColorID, BodyPatternID)
									VALUES (@BirdID, @Sex, @ColorID, @PatternID)
								END
							ELSE
								BEGIN
									INSERT INTO BodyPattern(BodyPatternDescription)
									VALUES (@Pattern)
									
									SELECT @PatternID = BodyPatternID
									FROM BodyPattern
									WHERE BodyPatternDescription = @Pattern
									
									INSERT INTO BirdBodyColorPattern(BirdID, Sex, BodyColorID, BodyPatternID)
									VALUES (@BirdID, @Sex, @ColorID, @PatternID)
								END
						END
				END
			ELSE
				BEGIN
					SELECT 'ERROR: BirdCode does not exist'
				END
		END
	ELSE
		BEGIN
			SELECT 'ERROR: Sex must be either F or M'
		END
		
	RETURN @@identity
END
GO


-- Add Bird Egg Color Pattern combinations
CREATE PROCEDURE AddBirdEggColorPatternCombination(@BirdCode CHAR(4), @Color VARCHAR(20), @Pattern VARCHAR(20))
AS
BEGIN

	DECLARE @BirdID INT
		, @ColorID INT
		, @PatternID INT

	IF EXISTS (SELECT * FROM Bird WHERE BirdCode = @BirdCode)
		BEGIN
			SELECT @BirdID = BirdID
			FROM Bird
			WHERE BirdCode = @BirdCode
			
			IF EXISTS (SELECT * FROM Color WHERE ColorDescription = @Color)
				BEGIN
					SELECT @ColorID = ColorID
					FROM Color
					WHERE ColorDescription = @Color
							
					IF EXISTS (SELECT * FROM EggPattern WHERE EggPatternDescription = @Pattern)
						BEGIN
							SELECT @PatternID = EggPatternID
							FROM EggPattern
							WHERE EggPatternDescription = @Pattern
							
							IF EXISTS (SELECT * FROM BirdEggColorPattern WHERE BirdID = @BirdID AND EggColorID = @ColorID AND EggPatternID = @PatternID)
								BEGIN
									SELECT 'ERROR: That BirdCode, Color, and Pattern (egg) combination already exists'
								END
							ELSE
								BEGIN
									INSERT INTO BirdEggColorPattern(BirdID, EggColorID, EggPatternID)
									VALUES (@BirdID, @ColorID, @PatternID)
								END
						END
					ELSE
						BEGIN
							INSERT INTO EggPattern(EggPatternDescription)
							VALUES (@Pattern)
							
							SELECT @PatternID = EggPatternID
							FROM EggPattern
							WHERE EggPatternDescription = @Pattern
							
							INSERT INTO BirdEggColorPattern(BirdID, EggColorID, EggPatternID)
							VALUES (@BirdID, @ColorID, @PatternID)
						END
				END
			ELSE
				BEGIN
					INSERT INTO Color(ColorDescription)
					VALUES (@Color)
					
					SELECT @ColorID = ColorID
					FROM Color
					WHERE ColorDescription = @Color
					
					IF EXISTS (SELECT * FROM EggPattern WHERE EggPatternDescription = @Pattern)
						BEGIN
							SELECT @PatternID = EggPatternID
							FROM EggPattern
							WHERE EggPatternDescription = @Pattern

							INSERT INTO BirdEggColorPattern(BirdID, EggColorID, EggPatternID)
							VALUES (@BirdID, @ColorID, @PatternID)
						END
					ELSE
						BEGIN
							INSERT INTO EggPattern(EggPatternDescription)
							VALUES (@Pattern)
							
							SELECT @PatternID = EggPatternID
							FROM EggPattern
							WHERE EggPatternDescription = @Pattern
							
							INSERT INTO BirdEggColorPattern(BirdID, EggColorID, EggPatternID)
							VALUES (@BirdID, @ColorID, @PatternID)
						END
				END
		END
	ELSE
		BEGIN
			SELECT 'ERROR: BirdCode does not exist'
		END
	
	RETURN @@identity
END
GO


-- Add Bird Nest Material combinations
CREATE PROCEDURE AddBirdNestMaterialCombination(@BirdCode CHAR(4), @Material VARCHAR(20))
AS
BEGIN

	DECLARE @BirdID INT
		, @MaterialID INT
		
	IF EXISTS (SELECT * FROM Bird WHERE BirdCode = @BirdCode)
		BEGIN
			SELECT @BirdID = BirdID
			FROM Bird
			WHERE BirdCode = @BirdCode
			
			IF EXISTS (SELECT * FROM NestMaterial WHERE NestMaterialDescription = @Material)
				BEGIN
					SELECT @MaterialID = NestMaterialID
					FROM NestMaterial
					WHERE NestMaterialDescription = @Material
							
					IF EXISTS (SELECT * FROM BirdNestMaterial WHERE BirdID = @BirdID AND NestMaterialID = @MaterialID)
						BEGIN
							SELECT 'ERROR: That BirdCode and NestMaterial combination already exists'
						END
					ELSE
						BEGIN
							INSERT INTO BirdNestMaterial(BirdID, NestMaterialID)
							VALUES (@BirdID, @MaterialID)
						END
				END
			ELSE
				BEGIN
					INSERT INTO NestMaterial(NestMaterialDescription)
					VALUES (@Material)
					
					SELECT @MaterialID = NestMaterialID
					FROM NestMaterial
					WHERE NestMaterialDescription = @Material
					
					INSERT INTO BirdNestMaterial(BirdID, NestMaterialID)
					VALUES (@BirdID, @MaterialID)
				END
		END
	ELSE
		BEGIN
			SELECT 'ERROR: BirdCode does not exist'
		END
	
	RETURN @@identity
END
GO


-- Add new Box
CREATE PROCEDURE AddBox(@BoxNumber VARCHAR(3), @Latitude DECIMAL(18,4), @Longitude DECIMAL(18,4), @TrailOrder INT)
AS
BEGIN
	
	DECLARE @Field CHAR(1)
	SELECT @Field = LEFT(@BoxNumber, 1)
	
	IF EXISTS (SELECT * FROM Box WHERE BoxNumber = @BoxNumber OR (Latitude = @Latitude AND Longitude = @Longitude))
		BEGIN
			SELECT CASE WHEN BoxNumber = @BoxNumber THEN 'ERROR: BoxNumber already exists'
					WHEN Latitude = @Latitude AND Longitude = @Longitude THEN 'ERROR: Latitude and Longitude combination already exists'
					END
			FROM Box
			WHERE BoxNumber = @BoxNumber
			OR (Latitude = @Latitude AND Longitude = @Longitude)
		END
	ELSE
		BEGIN
			UPDATE Box
			SET TrailOrder = TrailOrder+1
			WHERE TrailOrder >= @TrailOrder
			
			INSERT INTO Box(BoxNumber, Field, Latitude, Longitude, TrailOrder)
			VALUES (@BoxNumber, @Field, @Latitude, @Longitude, @TrailOrder)
		END
	
	RETURN @@identity
END
GO


-- Delete Box
CREATE PROCEDURE DeleteBox(@BoxNumber VARCHAR(3))
AS
BEGIN

	IF EXISTS (SELECT * FROM Box WHERE BoxNumber = @BoxNumber)
		BEGIN
			DECLARE @TrailOrder INT
			SELECT @TrailOrder = TrailOrder
			FROM Box
			WHERE BoxNumber = @BoxNumber
			
			DELETE FROM Box WHERE BoxNumber = @BoxNumber
			
			UPDATE Box
			SET TrailOrder = TrailOrder-1
			WHERE TrailOrder > @TrailOrder
		END
	ELSE
		BEGIN
			SELECT 'ERROR: BoxNumber does not exist'
		END
END
GO


-- Add new Condition
CREATE PROCEDURE AddCondition (@Year CHAR(4), @Month CHAR(2), @Date CHAR(2), @StartTime CHAR(5), @EndTime CHAR(5), @Temp INT)
AS
BEGIN
	
	DECLARE @StartDateTime DATETIME
		, @EndDateTime DATETIME
	SELECT @StartDateTime = convert(DATETIME, @Year+'-'+@Month+'-'+@Date+' '+@StartTime+':00')
		, @EndDateTime = convert(DATETIME, @Year+'-'+@Month+'-'+@Date+' '+@EndTime+':00')
	
	IF @StartDateTime < @EndDateTime
		BEGIN
			IF EXISTS (SELECT * FROM Condition WHERE StartDateTime = @StartDateTime OR EndDateTime = @EndDateTime OR convert(DATE, StartDateTime) = convert(DATE, @StartDateTime))
				BEGIN
					SELECT CASE WHEN StartDateTime = @StartDateTime THEN 'ERROR: StartTime already exists'
								WHEN EndDateTime = @EndDateTime THEN 'ERROR: EndTime already exists'
								WHEN convert(DATE, StartDateTime) = convert(DATE, @StartDateTime) THEN 'ERROR: Condition already exists for Date given'
								END
					FROM Condition
					WHERE StartDateTime = @StartDateTime
					OR EndDateTime = @EndDateTime
					OR convert(DATE, StartDateTime) = convert(DATE, @StartDateTime)
				END
			ELSE
				BEGIN
					INSERT INTO Condition (StartDateTime, EndDateTime, TemperatureF)
					VALUES (@StartDateTime, @EndDateTime, @Temp)
				END
		END
	ELSE
		BEGIN
			SELECT 'ERROR: StartTime is after EndTime'
		END
	
	RETURN @@identity
END
GO


-- Add Weather Condition
CREATE PROCEDURE AddWeatherCondition (@Date DATE, @Weather VARCHAR(30))
AS
BEGIN
	
	DECLARE @ConditionID INT
		, @WeatherID INT
	
	IF EXISTS (SELECT * FROM Condition WHERE convert(DATE, StartDateTime) = @Date)
		BEGIN
			SELECT @ConditionID = ConditionID
			FROM Condition
			WHERE convert(DATE, StartDateTime) = @Date
			
			IF EXISTS (SELECT * FROM Weather WHERE WeatherDescription = @Weather)
				BEGIN
					SELECT @WeatherID = WeatherID
					FROM Weather
					WHERE WeatherDescription = @Weather
					
						IF EXISTS (SELECT * FROM WeatherCondition WHERE WeatherID = @WeatherID AND ConditionID = @ConditionID)
							BEGIN
								SELECT 'ERROR: Condition and Weather combination already exists'
							END
						ELSE
							BEGIN
								INSERT INTO WeatherCondition(ConditionID, WeatherID)
								VALUES (@ConditionID, @WeatherID)
							END
				END
			ELSE
				BEGIN
					INSERT INTO Weather(WeatherDescription)
					VALUES (@Weather)
					
					SELECT @WeatherID = WeatherID
					FROM Weather
					WHERE WeatherDescription = @Weather
					
					INSERT INTO WeatherCondition(ConditionID, WeatherID)
					VALUES (@ConditionID, @WeatherID)
				END
		END
	ELSE
		BEGIN
			SELECT 'ERROR: Condition for does not exist for given Date'
		END
			
	RETURN @@identity
END
GO


-- Add new Observation
CREATE PROCEDURE AddObservation (@Date DATE, @BoxNumber VARCHAR(3), @OccupantCode CHAR(4), @nEggs INT, @nHatched INT, @nFledged INT)
AS
BEGIN
	
	DECLARE @BoxID INT
		, @ConditionID INT
		, @OccupantID INT

	IF EXISTS (SELECT * FROM Box WHERE BoxNumber = @BoxNumber)
		BEGIN
			SELECT @BoxID = BoxID
			FROM Box
			WHERE BoxNumber = @BoxNumber
			
			IF EXISTS (SELECT * FROM Condition WHERE convert(DATE, StartDateTime) = @Date)
				BEGIN
					SELECT @ConditionID = ConditionID
					FROM Condition
					WHERE convert(DATE, StartDateTime) = @Date
					
					IF EXISTS (SELECT * FROM Observation WHERE (BoxID = @BoxID AND ConditionID = @ConditionID))
						BEGIN
							SELECT 'ERROR: Observation for this BoxNumber and Date combination has been recorded'
						END
					ELSE
						IF EXISTS (SELECT * FROM Bird WHERE BirdCode = @OccupantCode)
							BEGIN
								IF @nEggs IS NULL OR @nHatched IS NULL OR @nFledged IS NULL
									BEGIN
										SELECT 'ERROR: NumberEggs, NumberHatched, and NumberFledged cannot be NULL'
									END
								ELSE
									BEGIN
										SELECT @OccupantID = BirdID
										FROM Bird
										WHERE BirdCode = @OccupantCode
								
										INSERT INTO Observation(BoxID, OccupantID, NumberEggs, NumberHatched, NumberFledged, ConditionID)
										VALUES (@BoxID, @OccupantID, @nEggs, @nHatched, @nFledged, @ConditionID)
									END
							END
						ELSE
							BEGIN
								SELECT 'ERROR: BirdCode does not exist'
							END
				END
			ELSE
				BEGIN
					SELECT 'ERROR: Conditions do not exist for Date provided'
				END
		END
	ELSE
		BEGIN
			SELECT 'ERROR: BoxNumber does not exist'
		END
	
	RETURN @@identity
END
GO


-- Add Observation Note
CREATE PROCEDURE AddObservationNote (@Date DATE, @BoxNumber VARCHAR(3), @Note VARCHAR(150))
AS
BEGIN
	
	DECLARE @BoxID INT
		, @ConditionID INT
		, @ObservationID INT
		, @NoteID INT
		
	IF EXISTS (SELECT * FROM Box WHERE BoxNumber = @BoxNumber)
		BEGIN
			SELECT @BoxID = BoxID
			FROM Box
			WHERE BoxNumber = @BoxNumber
			
			IF EXISTS (SELECT * FROM Condition WHERE convert(DATE, StartDateTime) = @Date)
				BEGIN
					SELECT @ConditionID = ConditionID
					FROM Condition
					WHERE convert(DATE, StartDateTime) = @Date
					
					IF EXISTS (SELECT * FROM Observation WHERE (BoxID = @BoxID AND ConditionID = @ConditionID))
						BEGIN
							SELECT @ObservationID = ObservationID
							FROM Observation
							WHERE ConditionID = @ConditionID
							AND BoxID = @BoxID
							
							IF EXISTS (SELECT * FROM Note WHERE NoteText = @Note)
								BEGIN
									SELECT @NoteID = NoteID
									FROM Note
									WHERE NoteText = @Note

									IF EXISTS (SELECT * FROM ObservationNote WHERE ObservationID = @ObservationID AND NoteID = @NoteID)
										BEGIN
											SELECT 'ERROR: Observation and Note combination already exists'
										END
									ELSE
										BEGIN
											INSERT INTO ObservationNote(ObservationID, NoteID)
											VALUES (@ObservationID, @NoteID)
										END											
								END
							ELSE
								BEGIN
									INSERT INTO Note(NoteText)
									VALUES (@Note)
									
									SELECT @NoteID = NoteID
									FROM Note
									WHERE NoteText = @Note
									
									INSERT INTO ObservationNote(ObservationID, NoteID)
									VALUES (@ObservationID, @NoteID)
								END
						END
					ELSE
						BEGIN
							SELECT 'ERROR: Observation has not been recorded for this Date and BoxNumber'
						END
				END
			ELSE
				BEGIN
					SELECT 'ERROR: Conditions do not exist for Date provided'
				END
		END
	ELSE
		BEGIN
			SELECT 'ERROR: BoxNumber does not exist'
		END
	
	RETURN @@identity
END
GO