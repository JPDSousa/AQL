CREATE LWW TABLE Universities (
	WorldRank INT PRIMARY KEY ,
	Institution VARCHAR ,
	NationalRank VARCHAR
);
CREATE TABLE Student (
	Name VARCHAR PRIMARY KEY,
	Age INT,
	YearsLeft COUNTER_INT CHECK > 0
)
