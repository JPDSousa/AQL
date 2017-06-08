CREATE LWW TABLE Universities (
	WorldRank INT PRIMARY KEY ,
	Institution VARCHAR ,
	NationalRank VARCHAR
);
CREATE TABLE Student (
	id INT PRIMARY KEY,
	Name VARCHAR,
	Age INT,
	YearsLeft COUNTER_INT CHECK GREATER 0
);
