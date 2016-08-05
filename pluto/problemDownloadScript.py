import json
from pprint import pprint
import time
import os
import operator
import requests

header = {'Expect': '', 'X-API-Key': '69-69f9cd695ba4358b4b56209a54c821db',}
a = requests.get('http://2016sv.icfpcontest.org/api/snapshot/list', headers=header)
b = json.loads(a.text)
newlist = sorted(b["snapshots"], key=lambda k: k['snapshot_time'])
newlist.reverse()
ha = newlist[0]["snapshot_hash"]
time.sleep(2)
print ha
yolo =  requests.get('http://2016sv.icfpcontest.org/api/blob/'+str(ha), headers=header)
print yolo.text
probs = json.loads(yolo.text)
print probs
hashes = list()
for problem in probs["problems"]:
    hashes.append(problem["problem_spec_hash"])

f = open('hashes.txt','w')
for hs in hashes:
    f.write("%s\n" % hs)

for i, val in enumerate(hashes):
    time.sleep(1)
    print("curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' 'http://2016sv.icfpcontest.org/api/blob/" + val[:-2] +"' > problems/"+val[:-2]+".txt")
    os.system("curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' 'http://2016sv.icfpcontest.org/api/blob/" + val[:-2] +"' > problems/" +val[:-2] + ".txt")