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

LA Names

https://geoportal.statistics.gov.uk/geoportal/catalog/search/resource/details.page?uuid=%7BD7F9B24A-8964-4FA1-8F0C-1EC85313DD3F%7D

Process:

- Get file list from supplied path
    -> path checked for
        - existence
        - access permissions
        - has files
        - all files are .csv

- Process all files for streets ahead of processing BLPU - streets are across files

- File processed singularly
    -> File rows checked by type for correct number of columns and all mandatory fields (ours not OS's) present
        - errors on a file means file not persisted (row check not data integrity check)

    -> Whole file processed into
        - list containing 6 types of object, BLPU, LPI, Classification, Organisation, Street and StreetDescriptor -> as per OS data model (these are reduced to fields we are intereseted in)
        - List then used to create address wrapper objects by UPRN -> these contain BLPU, LPI, Classification and Organisation.
        - Streets and Street Descriptors are transformed into maps of usrn to object -> these are a many to one to BLPU.

    -> Rules applied to transform address wrappers into addresses
        - rules include state, type, active, old etc

    -> File applied to DB in one go using mongo batch insert


Valid Address:

    - BLPU must be
        -> Logical status approved (not historical or provisional)
        ->

Assumptions:

    -> Suffix on LPI (SAO or PAO) not relevant if no corresponding numbers
    -> filtered out addresses that cannot receive post