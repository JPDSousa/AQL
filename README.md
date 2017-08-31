# AQL
[![Build Status](https://travis-ci.org/JPDSousa/AQL.svg?branch=master)](https://travis-ci.org/JPDSousa/AQL)

An SQL-like interface for [AntidoteDB](https://github.com/SyncFree/antidote).

## Installation

AQL can be used through Docker (recommended), or directly through rebar3.

### Docker

To use AQL through Docker simply pull the image from Docker Hub
[here](https://hub.docker.com/r/jpdsousa/aql_antidote/). If docker can't 
pull the lastest version of the image, do it manually by:

```
    docker pull jpdsousa/aql_antidote:<latest-version-available>
```

This image starts an antidoteDB server 
(uses [this](https://hub.docker.com/r/mweber/antidotedb/) image) on
background and runs an AQL instance of top.

This is the recommended way of running AQL, at least for single-client use.

### Rebar3
This project can also be installed "manually" through rebar3. In order to do so,
clone this repository:

```
    $ git clone https://github.com/JPDSousa/AQL
```

Open a terminal in the project folder (`cd AQL`) and then compile the project:

```
    $ make compile
```

Now, to run the client, you must have one (or more) running instances of AntidoteDB.
To start in shell mode run the following command:

```
    $ make shell
```

You can also start in development mode with:
```
    $ make dev
```

## Getting started

AQL is an SQL-variant, designed to work with AntidoteDB API.

### Shell
AQL provides a shell mode, which is the easiest way to use the
client. In shell mode, you'll see a prompt like this:
```
    AQL>
```
Use the prompt to input AQL statements.

### API

The AQL API is pretty straightforward. There is a main module called
`aqlparser` with a method `parse`, which takes a tuple as well as the AntidoteDB node long name. The two options
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

For instance, to show all existing tables in the database through the API, use:
```Erlang
aqlparser:parse({str, "SHOW TABLES"}, 'antidote@127.0.0.1').
```

## AQL Docs

AQL supports multiple SQL-like operations such as:
* Data definition
  * CREATE TABLE
* Data manipulation
  * SELECT
  * INSERT
  * UPDATE
  * DELETE
* Admin
  * SHOW TABLES/INDEX

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
CREATE @AW TABLE Student (
	id INT PRIMARY KEY,
	Name VARCHAR,
	Age INT DEFAULT 18,
	YearsLeft COUNTER_INT CHECK GREATER 0,
	Passport_id INTEGER FOREIGN KEY @FR REFERENCES Passport(id)
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
  * `INCREMENT` - increments the column(s) of type `INTEGER` by the value specified.
  * `DECREMENT` - decrements the column(s) of type `INTEGER` by the value specified.
* *COUNTER_INT*
  * `INCREMENT` - increments the column(s) of type `COUNTER_INT` by the value specified.
  * `DECREMENT` - decrements the column(s) of type `COUNTER_INT` by the value specified.
* *BOOLEAN*
  * `ENABLE`- sets the boolean value to `true`
  * `DISABLE`- sets the boolean value to `false`
  
Just like in a SELECT operation, the where clause can only filter primary keys.
  
### DELETE

Deletes a set of records from the specified table.

```SQL
DELETE FROM Persons Where PersonID = 2004525
```

Just like in a SELECT operation, the where clause can only filter primary keys.
If the `WHERE`clause is absent, all the records in the table are deleted.
