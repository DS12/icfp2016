import json
from pprint import pprint
import time
import os

with open("hashes.txt") as f:
    hashes = f.readlines()

for i, val in enumerate(hashes):
    time.sleep(1)
    print("curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' 'http://2016sv.icfpcontest.org/api/blob/" + val[:-2] +"' > p" + str(i) + ".txt")
    os.system("curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' 'http://2016sv.icfpcontest.org/api/blob/" + val[:-2] +"' > p" + str(i) + ".txt")