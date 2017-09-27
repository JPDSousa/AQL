Example 1
==========

This example provides a basic use case for AQL, with a specific database schema. The schema is build under a music theme, with *artists*, *albums* and *tracks*. The schema is defined through the following queries:

**queries image**

This schema creates the following tables:

**raw schema image**

The syntax is quite similar to SQL. One of the most remarkable changes is the annotation `@AW`, `@RW`, `@FR` and `@IR`. Thiese annotations define the behaviour off concurrent operations over the same record.
