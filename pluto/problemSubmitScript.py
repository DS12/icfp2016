import json
from pprint import pprint
import time
import os
import operator
import requests

def remove(x):
    return x[:-4]

a = map(remove,os.open("./solutionsRect"))


for prob in a:
    i = prob[:-1]
    time.sleep(2)
    print("curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' -F 'problem_id="+str(i)+"' -F 'solution_spec=@solutions/"+str(i)+".txt' 'http://2016sv.icfpcontest.org/api/solution/submit' > results/"+str(i)+".txt")
