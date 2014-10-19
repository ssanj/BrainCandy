Database Setup
==============

This example works with mySQL. If you need to use another database please update
the schema and seed data as necessary. Also you may need to tweek the SQL in 
the src/DB.hs file.

1. create the schemas in data/schema.ddl
2. import the sample data in data/seed.sql or add your own

Configuration
=============

1. Create a config.yml file as per the example in data/config.yml
2. Export a system varibable BRAINCANDY_CONFIG_DIR to point to your config.yml


How to build
============

1. cabal update
2. cabal sandbox init
3. cabal install --only-dependencies
4. cabal configure
5. cabal run

Hit the web page at: http://localhost:3000
