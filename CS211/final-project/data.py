"""
Module to deal with parsing data from the Excel spreadsheet
(converted into .csv files for convenience)
"""

import csv
from itertools import chain

if __name__ != "__main__":
    print("Processing data...")

# Initialising companies
companies = []
f = open('data/companies.csv', 'r')
try:
    reader = csv.reader(f)
    for row in reader:
        companies.append(row)
    companies = list(chain.from_iterable(companies))
finally:
    f.close()

# Initializing cost of stock per company
stock_cost = {}
f = open('data/stock_value.csv', 'r')
try:
    reader = csv.reader(f)
    for row in reader:  # will only have one row anyway
        for company, val in zip(companies, row):
            stock_cost[company] = int(val)
finally:
    f.close()

# Initializing dates (from old to new)
dates = []
f = open('data/Dates.csv', 'r')
try:
    reader = csv.reader(f)
    for row in reader:
        dates.append(row)
    dates = list(chain.from_iterable(dates))
finally:
    f.close()

# Initializing dictionary which takes a company and a date
# and returns the percentage change in that stock
stock_change = {}
f = open('data/Data.csv', 'r')
try:
    reader = csv.reader(f)
    for row, date in zip(reader, dates):
        for company, change in zip(companies, row):
            stock_change[company, date] = float(change)
finally:
    f.close()

magic_pairs = \
    [(2.1749694806214279, 'ED', 'HRL'),
     (2.1887847019046189, 'HRL', 'SO'),
     (2.2481038210531077, 'HRL', 'WEC'),
     (2.2631404563487645, 'BCR', 'HRL'),
     (2.2716093241817181, 'ED', 'GIS'),
     (2.2721823363601161, 'HRL', 'JNJ'),
     (2.2796788860401684, 'CLX', 'HRL'),
     (2.2822266670538482, 'HRL', 'SAI'),
     (2.293461478691909, 'HRL', 'KMB'),
     (2.2986676525372411, 'HRL', 'XEL'),
     (2.3109827660441518, 'BCR', 'GIS'),
     (2.311121745206, 'GIS', 'HRL'),
     (2.3163415363451949, 'GIS', 'WEC'),
     (2.3182672426539224, 'GIS', 'SO'),
     (2.3188306982323659, 'BCR', 'ED'),
     (2.3201184194753468, 'CPB', 'ED'),
     (2.3210978733481307, 'ED', 'MKC'),
     (2.3256684024153378, 'HRL', 'WMT'),
     (2.3264048962024333, 'ABT', 'HRL'),
     (2.3269988901657519, 'CLX', 'JNJ'),
     (2.3297725013737409, 'CLX', 'SO'),
     (2.3304693356984441, 'CLX', 'ED'),
     (2.3309115252879828, 'ED', 'JNJ'),
     (2.3314013526514454, 'BCR', 'CLX'),
     (2.3315646781425201, 'CPB', 'HRL'),
     (2.3327585052182025, 'CLX', 'GIS'),
     (2.3340137098627709, 'HRL', 'LH'),
     (2.3366949561022459, 'GIS', 'SAI'),
     (2.3369549022561715, 'CPB', 'WEC'),
     (2.3389064533353814, 'BCR', 'MKC'),
     (2.3403232114844021, 'CPB', 'SAI'),
     (2.3418340366863029, 'JNJ', 'MKC'),
     (2.3418396197104303, 'CPB', 'SO'),
     (2.3426822546745898, 'GIS', 'JNJ'),
     (2.3444483017765361, 'SAI', 'SO'),
     (2.3462213903920013, 'HRL', 'MCD'),
     (2.3473352900215354, 'CLX', 'WEC'),
     (2.3489742607434079, 'BDX', 'HRL'),
     (2.349006471080513, 'ED', 'SAI'),
     (2.3490136248880904, 'ED', 'K'),
     (2.3498862349205587, 'JNJ', 'SO'),
     (2.3499926605111519, 'BCR', 'SO'),
     (2.3513154078120091, 'CPB', 'JNJ'),
     (2.3573252289304727, 'JNJ', 'WEC'),
     (2.3577333432962764, 'BCR', 'JNJ'),
     (2.3579381706086227, 'KMB', 'WEC'),
     (2.3584363398736774, 'MKC', 'WEC'),
     (2.3601190271461046, 'HRL', 'PG'),
     (2.362001410237212, 'ED', 'KMB'),
     (2.3620584838406407, 'MKC', 'SO'),
     (2.36382555497386, 'CAG', 'ED'),
     (2.3647205930569037, 'BCR', 'CPB'),
     (2.3678949979356441, 'JNJ', 'SAI'),
     (2.3692127567022205, 'MKC', 'SAI'),
     (2.3715044752351959, 'KMB', 'SO'),
     (2.3725019835171137, 'CAG', 'SO'),
     (2.3739225682259639, 'BCR', 'WEC'),
     (2.3758130244799842, 'HRL', 'MKC'),
     (2.3760443927737076, 'BCR', 'KMB'),
     (2.3767728382882289, 'HRL', 'PEP'),
     (2.3781482632134954, 'GIS', 'KMB'),
     (2.3783850307229284, 'CLX', 'CPB'),
     (2.3791831402508912, 'ED', 'WMT'),
     (2.3809996727736342, 'DUK', 'HRL'),
     (2.3817007047916356, 'WEC', 'WMT'),
     (2.384156912268383, 'CLX', 'MKC'),
     (2.3847832511451168, 'JNJ', 'KMB'),
     (2.3850757749342661, 'LH', 'MKC'),
     (2.385415217818732, 'GIS', 'WMT'),
     (2.3864891618823321, 'GIS', 'XEL'),
     (2.3903201288195977, 'K', 'SAI'),
     (2.3910958234751507, 'KMB', 'SAI'),
     (2.3924948601069169, 'KMB', 'MKC'),
     (2.3937867260369283, 'CPB', 'MKC'),
     (2.3964798817592685, 'CAG', 'JNJ'),
     (2.3978971680644752, 'SAI', 'WEC'),
     (2.3981065743089123, 'ABT', 'CLX'),
     (2.3986151307135164, 'BDX', 'ED'),
     (2.3994293993699038, 'K', 'SO'),
     (2.400003261444152, 'CLX', 'LH'),
     (2.4007188256432026, 'GIS', 'MCD'),
     (2.4019077351470575, 'K', 'WEC'),
     (2.4022228813450424, 'BDX', 'CLX'),
     (2.4032938177442547, 'BDX', 'SO'),
     (2.4050697437028359, 'CPB', 'XEL'),
     (2.4051153826603482, 'CPB', 'KMB'),
     (2.4059638366075009, 'CPB', 'LH'),
     (2.4067570714311333, 'BCR', 'SAI'),
     (2.4082731061076958, 'CLX', 'SAI'),
     (2.4085383752228293, 'GIS', 'LH'),
     (2.4096161216986713, 'BCR', 'K'),
     (2.4096663544143495, 'ED', 'LH'),
     (2.4097234907371154, 'MKC', 'WMT'),
     (2.4101339814451705, 'JNJ', 'K'),
     (2.4117538828275769, 'CPB', 'WMT'),
     (2.4118436082720156, 'ABT', 'ED'),
     (2.4120571837452354, 'LH', 'SO'),
     (2.4135928138694855, 'ABT', 'KMB'),
     (2.4151372342909361, 'CAG', 'WEC'),
     (2.4173025303781355, 'BDX', 'GIS')]
