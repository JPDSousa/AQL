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

AQL is an SQL-variant, design to work with AntidoteDB API.

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

Creates a new table

```SQL
CREATE TABLE Persons (
    PersonID int PRIMARY KEY,
    LastName varchar,
    FirstName varchar,
    Address varchar,
    City varchar
);
```

The primary key constraint must be specified after the collumn which is to be
set as the primary key (multiple collumns as primary keys are not supported).
