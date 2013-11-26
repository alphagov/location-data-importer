#!/bin/bash

cachedir=~/mongo/download-cache
mongodir=~/mongo/mongo-workspace/location
mongoversion=2.2.4

echo Stopping mongodb
killall mongos > /dev/null 2>&1
killall mongod > /dev/null 2>&1
killall mongo > /dev/null 2>&1

case $1 in
	clean)
		echo "Cleaning mongo install"
		# Remove all mongo workspace directories excluding the logs folder (saves having to continually re-link this
		# when configuring vagrant).
		find $mongodir/ -type d ! -iregex ".*[logs|/$]" | xargs rm -Rf
		# Remove mongo workspace files i.e. pid
		find $mongodir/ -type f | xargs rm -f
		# Clean the actual log files whilst leaving the folder intact
		rm -rf $mongodir/logs/*.log
		;;
    reset)
        echo "Cleaning cache"
        rm -rf $cachedir
        echo "Cleaning mongo install"
        rm -rf $mongodir
esac

function error_exit
{
    echo "FATAL: ${1:-"Unknown Error"}" 1>&2
    exit 1
}
mkdir -p $cachedir
mkdir -p $mongodir/db/node1
mkdir -p $mongodir/db/node2
mkdir -p $mongodir/db/node3
mkdir -p $mongodir/logs

case `uname` in
	Linux)
		mongoFile=mongodb-linux-x86_64-${mongoversion}.tgz
		mongoPath=mongodb-linux-x86_64-${mongoversion}
		mongoOs="linux"
		;;
	
	Darwin)
		mongoFile=mongodb-osx-x86_64-${mongoversion}.tgz
		mongoPath=mongodb-osx-x86_64-${mongoversion}
		mongoOs="osx"
		;;
			
	*)
		echo "Unsupported OS"
		exit 1
esac
		
if [ ! -f "$cachedir/$mongoFile" ]; then
        destMongofile=$cachedir/$mongoFile
	echo "Downloading http://fastdl.mongodb.org/$mongoOs/$mongoFile"
	curl -o $destMongofile "http://fastdl.mongodb.org/$mongoOs/$mongoFile" 
fi	

if [ ! -d "$mongodir/$mongoPath" ]; then
    echo "Extracting to $mongodir"
	tar -C $mongodir -xzf $cachedir/$mongoFile
fi

echo "Starting mongodb node-1"
$mongodir/$mongoPath/bin/mongod --dbpath $mongodir/db/node1 \
	--logpath $mongodir/logs/mongodb-node1.log \
	-vvvvv \
	--smallfiles \
	--rest \
	--pidfilepath $mongodir/mongo.pid \
	--fork \
	 --notablescan \
	--directoryperdb \
	--replSet gds-replica-set \
	--port 27017

echo "Wait 10 secs to let the nodes start up"
for i in {1..10}
do
	echo $[10 - $i]
	sleep 1
done
		
echo "Init the replica set"
$mongodir/$mongoPath/bin/mongo admin --eval 'printjson(db.runCommand({"replSetInitiate" : {"_id" : "gds-replica-set", "members" : [ { "_id" : 1, "host" : "localhost:27017" }]}}))' || error_exit "Failed to init replica set"

echo "Wait 30 secs for the replica set"
for i in {1..30}
do
	echo $[30 - $i]
	sleep 1
done

echo DONE
	
