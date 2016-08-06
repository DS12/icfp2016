import json
from pprint import pprint
import time
import os
import operator
import requests

header = {'Expect': '', 'X-API-Key': '69-69f9cd695ba4358b4b56209a54c821db',}
snapshot = requests.get('http://2016sv.icfpcontest.org/api/snapshot/list', headers=header)
snapshotList = json.loads(snapshot.text)
newlist = sorted(snapshotList["snapshots"], key=lambda k: k['snapshot_time'])
newlist.reverse()
latestSnap = newlist[0]["snapshot_hash"]
time.sleep(2)
latestReq =  requests.get('http://2016sv.icfpcontest.org/api/blob/'+str(latestSnap), headers=header)


problems = json.loads(latestReq.text)["problems"]
problemIdHash = list()
problemId = list()

for problem in problems:
    problemId.append(problem["problem_id"])
    problemIdHash.append((problem["problem_spec_hash"],problem["problem_id"]))



def remove(x):
    return x[:-4]
downloadedProblems = map(remove,os.listdir("./problems"))

newProblemsId = [x for x in problemId if str(x) not in downloadedProblems]
print newProblemsId

for i in problemIdHash:
    if(i[1] in newProblemsId):
        time.sleep(1.2)
        print(str(i[0]) +"' > problems/" + str(i[1]) + ".txt")
        os.system("curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' 'http://2016sv.icfpcontest.org/api/blob/" + str(i[0]) +"' > problems/" + str(i[1]) + ".txt")

