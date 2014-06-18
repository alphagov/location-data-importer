#!/usr/bin/python

import sys
import uuid
import re

POSTCODE = "postcode"
UPRN = "uprn"

class BLPU:

	def __init__(self, line):
		self.line = line
		
	def dump(self):
		return line

class LPI:

	def __init__(self, line):
		self.line = line
		
	def dump(self):
		return line

class Organisation:

	def __init__(self, line):
		self.line = line
		
	def dump(self):
		return line		

class Classification:

	def __init__(self, line):
		self.line = line
		
	def dump(self):
		return line	

class Street:

	def __init__(self, line):
		self.line = line
		
	def dump(self):
		return line				

class StreetDescriptor:

	def __init__(self, line):
		self.line = line
		
	def dump(self):
		return line	

class DeliveryPoint:

	def __init__(self, line):
		self.line = line
		
	def dump(self):
		return line			

def process_lines(lines):
	for line in lines:
		print(BLPU(line).line)

def is_a_uprn(v):
	r = re.compile('^\d{12}$')
	return r.match(v)

def file_lines(filename):
	lines = open(filename).readlines()
	return lines

if (len(sys.argv) <= 2):
	print("usage: filename uprn|postcode")
	sys.exit(1)

filename = sys.argv[1]
print("file: " + filename)

identifier = sys.argv[2]
print("identifier: " + identifier)

identifier_type = UPRN if is_a_uprn(identifier) else POSTCODE
print("identifier_type: " + identifier_type)

process_lines(file_lines(filename))