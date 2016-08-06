import json
from pprint import pprint
import time
import os
import operator
import requests

baseTime = 1470441600
diffTime = 3600
uploadTime = baseTime + diffTime*5
for i in range(13):
    os.system("curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' -F 'solution_spec=@uploads/"+ str(i) +".txt' -F 'publish_time="+str(uploadTime)+"' 'http://2016sv.icfpcontest.org/api/problem/submit'")
    uploadTime = uploadTime + 3600
