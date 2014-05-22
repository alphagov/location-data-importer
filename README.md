Location Data Importer
======================

Location Data Importer is a scala application that imports Ordinance Survey data into a mongo database. Primarily it is used to back the locate-api which provides address and local authority lookups from postcode / GSS Code.

Issued under an MIT license, see LICENSE file.

### Ordinance Survey

This is backed by two Ordinance Survey products:

    - Address Base Premium: This provides all the details of actual addresses.
    - Code Point: Provides a mapping between postcode and a number of ONS statistical codes, these codes are used to identify Local Authorities, countries and electoral wards.

Additionally we use the ONS Local Authority dataset, manually linked to the OS local custodian dataset to provide extra validation of the Address to Local Authority resolution.

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

A script in the root of the project runs the above:

    $ ./run-examples.sh

Mongo:

    - uses a mongo database: installed via ./scripts/mongo/mongo-replica-set.sh

### Command Line Options

    Usage: Location Data Importer [options]

       -a <value> | --addresses <value>
             Location of address base files files
       -c <value> | --codepoint <value>
             Location of code point files)
       -o | --addressOnly
             Only do address import stage. (Default process all stages)
       -p <value> | --password <value>
             Password for the mongo (default none)
       -u <value> | --username <value>
             Username for the mongo (default none)
      --version
            0.1

* addresses: Directory containing address base premium files. (provide path)

* codepoint: Directory containing code point files. (provide path)

* addressOnly: If the codepoint and street phases are complete, just run the address phase

* password / --username: Mongo credentials if required.

### Directory Structure

* docs. Contains various useful documents regarding Ordinance Survey Products
    - user guide for code point.
    - user gurdes for boundary line.
    - number of address base premimum documentation, incuding introductory powerpoint.
    - examples CSV files for an address base file and a code point file.

* scripts: mongo set up scripts

* test-cases: OS supplied test cases for checking quality of generated addresses

* testdata: Test files for application test suite

### Process:

(1) Code point files parsed and inserted into the Mongo DB. This is done upfront as we make a reference table to query address postcodes against to discover GSS codes.

    - Example document:

    {
        "_id" : ObjectId("52b1e7c38de22313daeedc38"),
        "postcode" : "ab101aa",
        "country" : "Scotland",
        "gssCode" : "S12000033",
        "name" : "Grampian"
    }

(2) Address base files are parsed for Street data and inserted into the Mongo DB.

    - The persisted street objects are a join between the Address Base Premium notions of
        - Street
        - Street Description

    - This is done as streets exist in a single Address Base file, however may be referenced by address objects across several. Consequently we process these first and use the collection as a lookup table as we construct the more complex address objects.

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
        - DeliveryPoint
            (Optional) Used to verify postcode on the BLPU. If there is a discrepency the DeliveryPoint postcode is used.

    Each CSV file denotes an area. Each file will contain many rows (can be 10,000s), and will contain rows of each of the types defined above. The uploader will load the entire file into memory and try and associate the BLPU with the appropriate LPI, Organisation and Classification. This is done by means of a UPRN which acts a primary key across these data types.

    - Filtering BLPUs
        - We don't treat all BLPUs as valid, and some will be skipped according to the following rules:
        (1) BLPU has an end date. We only want current BLPUs
        (2) No associated LPI available.
        (3) No active LPI available.
        (4) No classification available.
        (5) No matching CodePoint entry
        (6) No matching active street

(4) Persisting Addresses

    - Once a list of AddressWrappers (an object containing the BLPU, LPI, Classification and optional Organisation) are created we iterate through this list constructing the Address format we want to persist. 

    - We query the code points and street collections in mongo by postcode and USRN (unique street reference number, present on an LPI) respectively to aquire the details of the Country and Local Authority (codepoint) and street (street) 

    - Example JSON:

        {
            "_id" : ObjectId("533d9b8d8de2c5f30a429e3c"),
            "postcode" : "kt13aw",
            "gssCode" : "E09000021",
            "country" : "England",
            "createdAt" : ISODate("2014-04-03T17:34:03.990Z"),
            "presentation" : {
                "property" : "Po Box Number 1487",
                "street" : "Hogsmill Lane",
                "town" : "Kingston Upon Thames",
                "postcode" : "KT1 3AW",
                "uprn" : "10015033889"
            },
            "location" : {
                "lat" : 51.404780948504616,
                "long" : -0.2928459753727967
            },
            "details" : {
                "blpuCreatedAt" : ISODate("2010-01-16T00:00:00Z"),
                "blpuUpdatedAt" : ISODate("2010-01-16T00:00:00Z"),
                "classification" : "OR03",
                "state" : "approved",
                "isPostalAddress" : false,
                "isCommercial" : false,
                "isResidential" : false,
                "usrn" : "21880163",
                "file" : "TQ1565.csv",
                "organisation" : "Medical Associates Ltd",
                "primaryClassification" : "Other",
                "secondaryClassification" : "Royal Mail Infrastructure"
            },
            "ordering" : {
                "paoText" : "poboxnumber1487"
            }
}
(5) Processing files

    - Directories supplied by the user on the command line are checked for having only CSV files. An error is anything else is present.

    - Files are checked for validity on a per row basis - for correct number of columns per type, and for having the mandatory fields required to create the above data structures.

(6) Logging

    Uses Java logging - logback.xml to configure

(7) Credentials

    Credentials for APIs are not created  by the application. In order to create tokens for the API clients:

        db.authorizationToken.insert({token:"Me6ZGsSKqkVLNLS9fzYBvrGCQF4"})

        db.authorizationToken.ensureIndex({token:1})

    Locate API has endpoints for making new tokens, but needs a first token to get going.

### Boundary line
[Boundary line](http://www.ordnancesurvey.co.uk/business-and-government/products/boundary-line.html) is the polygon data set outlining the various administrative boundaries in the UK.
This is required to give the correct granular mapping between an address and the area in which it sits.

Currently this is not used as part of the import - custodian codes in AddressBase and code point files are used only.

Boundary line comes as shape files. Processing shape files into mongo can be done with [GDAL](http://www.gdal.org/).

        brew install gdal
        ogr2ogr -f "GeoJSON" ~/unit.json unitary_electoral_division_region.shp unitary_electoral_division_region
        ogr2ogr -f "GeoJSON" ~/district.json district_borough_unitary_region.shp district_borough_unitary_region

Data sets currently used are: district_borough_unitary_region

The docs directory contains a geojson file containing the data which can be imported into mongo with the following command:

    mongoimport --db locate --collection authorityBoundaries district-quarter.geojson



