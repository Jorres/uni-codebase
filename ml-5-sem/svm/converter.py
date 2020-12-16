import csv
import random

is_geyser = False

if is_geyser:
    f = open('geyser.csv', 'r')
else:
    f = open('chips.csv', 'r')

reader = csv.reader(f)

first = True
data = []
for row in reader:
    if not first:
        if is_geyser:
            row[0] = str(float(row[0]) / 23)
            row[1] = str(float(row[1]) / 6)
        else:
            row[0] = str(float(row[0]))
            row[1] = str(float(row[1]))
        row[len(row) - 1] = '1' if row[len(row) - 1] == 'P' else '-1'
        data.append(' '.join(row))  
    first = False

print(len(data))
random.shuffle(data)
for line in data:
    print(line)
