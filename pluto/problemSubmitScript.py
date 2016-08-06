import json
from pprint import pprint
import time
import os
import operator
import requests

f = open('hashes.txt','r')
for prob in f.readlines():
    i = prob[:-1]
    time.sleep(2)
    os.system("curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' -F 'problem_id="+str(i)+"' -F 'solution_spec=@solutions/"+str(i)+".txt' 'http://2016sv.icfpcontest.org/api/solution/submit' > results/"+str(i)+".txt")
