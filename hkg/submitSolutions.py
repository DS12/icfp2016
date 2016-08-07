import json
from pprint import pprint
import time
import os
import operator
import requests

numProblems = 200
submissionList = list()


solutionPath = "./GitStuff/icfp2016/hkg/solutions/"
for i in range(1, numProblems, 1):
    solution = str(i) ++ ".txt"
    os.system("curl --compressed -L -H Expect: -H 'X-API-Key: 54-2308a3a7aa9d971f3c5beba539565513' - F 'problem_id='" ++ str(i) ++ "solution_spec=" ++ solutionPath ++ solution ++ "'http://2016sv.icfpcontest.org/api/solution/submit'")