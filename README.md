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


Classifications:

(1) Whats residential? TODO  is it just R vs everything else?

Address Formats:

(1) Standard: British Standard BS7666. "These addresses are used to provide an accurate geographic locator for an object to aid for example, service delivery, asset management, or command and control operations. They also represent the legal form of addresses as created under street naming and numbering legislation."