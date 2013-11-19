Location Data Importer
======================

Repo to hold code to import data sets used to construct location APIs.


Basic usage

(1) Use SBT to build project (from project root)

    $ ./sbt
    > assembly

(2) Run jar (from project root)

    java -jar target/scala-2.10/location-data-importer.jar


Contents

(1) Documentation

    docs/ contains an example csv file [example.csv] and all the address base premium pdf documentation.