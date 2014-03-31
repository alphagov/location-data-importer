Location Data Importer
======================

Location Data Importer is a scala application that imports Ordinance Survey data into a mongo database. Primarily it is used to back the location-api which provides address and local authority lookps from postcode / GSS Code.

### Ordinance Survey

This is backed by two Ordinance Survey products:

    - Address Base Premimum: This provides all the details of actual addresses.
    - Code Point: Provides a mapping between postcode and a number of ONS statistical codes, these codes are used to identify Local Authorities, countries and electoral wards.

### Basic usage

The importer compiles to a jar:

(1) Use SBT to build project (from project root)

    $ ./sbt
    > assembly

(2) Run jar (from project root)

    $ java -jar target/scala-2.10/location-data-importer.jar

The jar runs with command line options. Help is available:

     $ java -jar target/scala-2.10/location-data-importer.jar --help

Example run:

    $ java -jar target/scala-2.10/location-data-importer.jar -a docs/examples/addressbase/ -c docs/examples/codepoint/

Mongo:

    - uses a mongo database: installed via ./scripts/mongo/mongo-replica-set.sh

### Command Line Options

    Usage: Location Data Importer [options]

      -a <value> | --addresses <value>
            Location of address base files files
      -c <value> | --codepoint <value>
            Location of code point files)
      -r | --removeReport
            Remove the reports. (Default don't)
      -p <value> | --password <value>
            Password for the mongo (default none)
      -u <value> | --username <value>
            Username for the mongo (default none)
      --help
            use -d or -dir to identify source directory containing files to parse
      --version
            0.1

* addresses: Directory containing address base premimum files. (provide path)

* codepoint: Directory containing codepoint files. (provide path)

* removeReport: Should we remove the report files prior to starting.

* password / --username: Mongo credentials if required.

### Directory Structure

* docs. Contains various useful documents regarding Ordinance Survey Products
    - user guide for code point.
    - user gurdes for boundary line.
    - number of address base premimum documentation, incuding introductory powerpoint.
    - example CSV files for an address base file and a code point file.
    

### Process:

(1) Code point files parsedCsvLine and inserted into the Mongo DB. This is done upfront as we make a reference table to query address postcodes against to discover GSS codes.

    - Example document:

    {
        "_id" : ObjectId("52b1e7c38de22313daeedc38"),
        "postcode" : "ab101aa",
        "country" : "S92000003",
        "district" : "S12000033",
        "ward" : "S13002483"
    }

(2) Address base files parsedCsvLine for Streets and inserted into the Mongo DB.

    - The persisted street objects are an join between the Address Base Premimum notions of
        - Street
        - Street Description

    - This is done as streets exist in a single Address Base file, however may be referenced by several. Consequently we process these first and use the collection as a lookup table as we construct the more complex address objects.

    - Example document:

        {
            "_id" : ObjectId("52b1e85b8de22313db0943d2"),
            "usrn" : "21870044",
            "streetDescription" : "NIPPER ALLEY",
            "townName" : "KINGSTON UPON THAMES",
            "administrativeArea" : "KINGSTON UPON THAMES",
            "recordType" : "officiallyDesignated",
            "state" : "open",
            "surface" : "metalled",
            "classification" : "allVehicles",
            "file" : "TQ1565.csv"
        }

(3) Address base files are then processed a second time to build up addresses.

    - Address base files contain several types of row, we care about:
        - BLPU 
            Required – a BLPU is defined as a real-world object that is an ‘area of land, property or structure of fixed location having uniform occupation, ownership or function’. The BLPU is the core element of AddressBase Premium. In essence, a BLPU associates a real-world object on the ground to a UPRN.
        - LPI 
            Required – an LPI is a structured text entry that identifies a BLPU.
        - Organisation 
            (Optional. Company at an address)
        - Classification 
            (Required. Current use of the property, i.e. residential or commercial, to some level of details)

    Each CSV file denotes an area. Each file will contain many rows (can be 10,000s), and will contain rows of each of the types defined above. The uploader will load the entire file into memory and try and associate the BLPU with the appropriate LPI, Organisation and Classification. This is done by means of a UPRN which acts a primary key across these data types.

    - Filtering BLPUs
        - We don't treat all BLPUs as valid, and some will be skipped according to the following rules:
        (1) BLPU has an end date. We only want current BLPUs
        (2) No associated LPI available.
        (3) No active LPI available.
        (4) No classification available.

(4) Persisting Addresses

    - Once a list of AddressWrappers (an object containing the BLPU, LPI, Classification and optional Organisation) are created we iterate through this list constructing the Address format we want to persist. 

    - We query the code points and street collections in mongo by postcode and USRN (unique street reference number, present on an LPI) respectively to aquire the details of the Country and Local Authority (codepoint) and street (street) 

    - Any AddressWrapper that lacks either a street or code point document will be skipped.

    - Example JSON:

        {
            "_id" : ObjectId("52b20cbb03642ce9d53816ba"),
            "houseName" : "PO BOX NUMBER 1487",
            "postcode" : "kt13aw",
            "gssCode" : "E09000021",
            "countryCode" : "E92000001",
            "createdAt" : ISODate("2013-12-18T20:58:09.804Z"),
            "presentation" : {
                "property" : "PO BOX NUMBER 1487",
                "streetAddress" : "HOGSMILL LANE",
                "town" : "KINGSTON UPON THAMES",
                "postcode" : "KT1 3AW",
                "uprn" : "10015033889"
            },
            "location" : {
                "x" : 518841.2,
                "y" : 168688.3
            },
            "details" : {
                "blpuCreatedAt" : ISODate("2010-01-16T00:00:00Z"),
                "blpuUpdatedAt" : ISODate("2010-01-16T00:00:00Z"),
                "classification" : "OR03",
                "state" : "approved",
                "isPostalAddress" : false,
                "isCommercial" : true,
                "isResidential" : false,
                "usrn" : "21880163",
                "file" : "TQ1565.csv",
                "organisation" : "MEDICAL ASSOCIATES LTD"
            }
        }

(5) Processing files

    - Directories supplied by the user on the command line are checked for having only CSV files. An error is anything else is present.

    - Files are checked for validity on a per row basis - for correct number of columns per type, and for having the mandatory fields requred to create the above data structures.

(6) Logging

    - Two files are made in /tmp

    (1) location-data-file-list.txt: This will list all files processed, reason (code point, street or address) and time taken to process.

    (2) location-data-import-report.txt: This lists all errors in processing. Current error list is:
        - Invalid BLPU (is expired for example)
        - No street for BLPU
        - No Code point entry for BLPU
        - Row failed to parse
        - No LPI for BLPU
        - No active LPI for BLPU
        - No active street for BLPU
        - No street for BLPU
        - No classification for BLPU

        This file will list the filename, the UPRN and the reason to allow for investigation.


### Boundary line
[Boundary line](http://www.ordnancesurvey.co.uk/business-and-government/products/boundary-line.html) is the polygon data set outlining the various administrative boundaries in the UK. This is required to give the correct granular mapping between an address and the area in which it sits.


Boundary line comes as shape files. Processing shape files into mongo can be done with [GDAL](http://www.gdal.org/).

        brew install gdal
        ogr2ogr -f "GeoJSON" ~/unit.json unitary_electoral_division_region.shp unitary_electoral_division_region
        ogr2ogr -f "GeoJSON" ~/district.json district_borough_unitary_region.shp district_borough_unitary_region

Data sets currently used are:

        unitary_electoral_division_region
        district_borough_unitary_region

