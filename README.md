# json-relational-mapper
streams big json files into a relational database with a given schema and mapping

## Building and Running the program
For now you still have to compile the program yourself, for which you need cabal. One way to install it with [ghcup](https://www.haskell.org/ghcup/install/)

After you installed it, run:
```
cabal build
cabal run json-relation-mapper -- --config /path/to/your/dbconfig.yml --data /path/to/your/data.jsonl
```

## Configuration
Take a look at the [db_map.yml](https://github.com/FabiusII/json-relation-mapper/blob/main/db_map.yml) file for an example configuration in relation to the data file [order-states.jsonl](https://github.com/FabiusII/json-relation-mapper/blob/main/order-states.jsonl). You have to specify the database connection parameters, the schema of your relational database and the mapping from json paths to table columns.
Data files should be in jsonlines format, i.e. one json object per line. 

## Additional notes
* Currently only postgresql is supported as a database backend. <br>
* The program uses streaming to handle large json files that do not fit into memory. <br>
* The program creates tables if they do not exist yet. <br>
