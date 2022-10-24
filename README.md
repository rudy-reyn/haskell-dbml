# HDbml
## Haskell Database Markup Language

This is a Haskell implementation of DBML (Database Markup Language) - a DSL used for defining
database schemas and structures. The reference implementation is written in Javascript and can be
found [here](https://github.com/holistics/dbml).
Documentation for the reference implementation can be found at [dbml.org](https://www.dbml.org).

So for, this implementation can parse and translate DBML table definitions to a generic SQL
dialect CREATE TABLE statement, most closely representing SQLite, while also preserving notes/comments.
The goal is to write transpilers to and from various SQL dialects, support more SQL statements, and
to potentially be able use this as an ORM.
I am also interested in building a superset of DBML to support other the functionalities of other
database language categories outside of data definition
(DDL) such as DML and DQL.
