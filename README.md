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

Run `$ rebar3 compile` to download all the required dependencies.

```
    $ rebar3 compile
```

Then, to start the shell in development mode run the following command:
```
    $ rebar3 shell --name=NODE_NAME --setcookie ANTIDOTE_COOKIE
```
Where NODE_NAME is the full node name (the host must be the same of one of the
AntidoteDB nodes), and the cookie is the
[erlang cookie](http://erlang.org/doc/reference_manual/distributed.html)
specified (AntidoteDB's default cookie is `antidote`).

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
CREATE TABLE Persons (
    PersonID int PRIMARY KEY,
    LastName varchar,
    FirstName varchar,
    Address varchar,
    City varchar
);
```

The primary key constraint must be specified after the column which is to be
set as the primary key (multiple columns as primary keys are not supported).
Any datatype can be a primary key.

AQL also supports conditions on counters (`counter_int`). Custom conditions are
 currently not supported for this datatype. This means that all `counter_int`
 columns will have a `> 0` constraint.

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
SET FirstName = 'First2'
WHERE PersonID = 1
```

Update is still very unstable. It is still only directed at bounded counter
updates, but it will be soon expanded to all data types. *Use with caution*.
