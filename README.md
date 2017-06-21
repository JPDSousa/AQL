# AQL

An SQL-like interface for [AntidoteDB](https://github.com/SyncFree/antidote).

## Installation

AQL used through Docker (recommended), or directly through rebar3.

### Docker

To use AQL through Docker simply pull the image from Docker Hub
[here](https://hub.docker.com/r/jpdsousa/aql_antidote/). This starts an antidote
server (uses [this](https://hub.docker.com/r/mweber/antidotedb/) image) on
background and runs an AQL instance of top.

The image starts the shell in
[development mode](https://www.rebar3.org/v3/docs/commands#section-shell),
as this is still a demo image.

This is the recommended way of running AQL.

### Rebar3
This project can also be installed "manually" through rebar3. In order to do so,
clone this repository and open a terminal in the project folder.

First of all, you must have one (or more) running instances of AntidoteDB.

Run `$ make compile` to download all the required dependencies.

```
    $ make compile
```

Then, to start the shell in development mode run the following command:

```
    $ make shell
```

## Getting started

AQL is an SQL-variant, designed to work with AntidoteDB API.

### API

The AQL API is pretty straightforward. There is a main module called
`aqlparser` with a method `parse`, which takes a tuple. The two options
available are:
```Erlang
{str, AQLCommand}
```
Which parses the `AQLCommand` and outputs the result.
```Erlang
{file, AQLFile}
```
Which takes a path to a file (no specific format) and parses its content as an
AQL command.

## AQL Docs

AQL supports multiple SQL-like operations such as:
* Data definition
  * CREATE TABLE
* Data manipulation
  * SELECT
  * INSERT
  * UPDATE

AQL supports a limited set of types:
* VARCHAR - common text data type (similar to SQL's VARCHAR)
* INTEGER/INT - common integer data type (similar to SQL's INTEGER)
* BOOLEAN - common boolean
* COUNTER_INT - integer counter, with bounds
(based on AntidoteDB's
[Bounded Counter](http://www.gsd.inesc-id.pt/~rodrigo/srds15.pdf))

### CREATE TABLE

Creates a new table. If the table already exists the new table will overwrite it
 (any concurrent conflicts will be resolved with a *Last Writer Wins* CRP).

```SQL
CREATE TABLE Student (
	id INT PRIMARY KEY,
	Name VARCHAR,
	Age INT DEFAULT 18,
	YearsLeft COUNTER_INT CHECK GREATER 0,
	Passport_id INTEGER FOREIGN KEY REFERENCES Passport(id)
);
```

#### Primary keys

The primary key constraint must be specified after the column which is to be
set as the primary key (multiple columns as primary keys are not supported).
Any datatype can be a primary key.

Primary keys only guarantee uniqueness. Although, if two rows with the same 
primary key are inserted (either concurrently or not), both insertions will be
merged, and all columns columns will also be merged according to its datatypes.

#### Check Constraints

AQL also supports constraints on counters (`counter_int`). Custom conditions are
currently not supported for this datatype. This means that all `counter_int`
columns will have a `> 0` constraint.

#### Default Values

You can also define a default value for a record (not allowed in primary keys).
Default values are used when no value is specified for a record.

#### Foreign Keys

Foreign keys allow users to create custom relations between elements of different
tables. To create a foreign key relation simply add to the column that will be 
the foreign key: `FOREIGN KEY REFERENCES _parentTable_(_parentColumn_)`, where 
`_parentTable_` is the parent table name (e.g. `Passport`) and `_parentColumn_` is
the parent column name (e.g. `id`). All foreign keys must point to columns with a 
unique constraint, which is only guaranteed in primary keys.

### SELECT

SELECT is the main read operation in AQL (similar to SQL). The operation issues
a read operation in the database engine (AntidoteDB).

```SQL
SELECT * FROM Persons WHERE PersonID = 20;
```

*Note:* This operation only supports reads per primary key (like the one in the
 example). This is a temporary limitation.

### INSERT

Inserts new values in a table. If a value with the primary key already exists it
 will be overwritten.

```SQL
INSERT INTO (PersonID, LastName, FirstName, Address, City) VALUES (10, 'Last1', 'First1', 'Local1', 'City1')
```

The table columns must always be specified.

### UPDATE

Updates an already-existent row on the specified table.

```SQL
UPDATE Persons
SET FirstName ASSIGN 'First2'
WHERE PersonID = 1
```

Updates all rows in table `Persons` where `PersonID` has value 1. The update
sets column `FirstName` to value `'First2'`. The operation keyword (e.g.
`ASSIGN`) depends on the AQL datatype:
* *VARCHAR*
  * `ASSIGN` - sets the column(s) of type `VARCHAR` to the value specified.
* *INTEGER*
  * `ASSIGN` - sets the column(s) of type `INTEGER` or `INT` to the value specified.
* *COUNTER_INT*
  * `INCREMENT` - increments the column(s) of type `COUNTER_INT` by the value specified.
  * `DECREMENT` - decrements the column(s) of type `COUNTER_INT` by the value specified.

Just like in a SELECT operation, the where clause can only filter primary keys.
