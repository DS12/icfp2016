import os
import time


def remove(x):
    return x[:-4]


solutions = set(os.listdir("./solutionsRect"))
already_submitted = set(os.listdir("./results"))

print "Already submitted problems:", len(already_submitted)
print "Solved problems:", len(solutions)
print "New solutions to submit:", len(solutions - already_submitted)

ids = map(remove, solutions - already_submitted)

for i in ids:
    time.sleep(1.25)
    os.system(
        "curl --compressed -L -H Expect: -H 'X-API-Key: 69-69f9cd695ba4358b4b56209a54c821db' -F 'problem_id=" + str(
            i) + "' -F 'solution_spec=@solutionsRect/" + str(
            i) + ".txt' 'http://2016sv.icfpcontest.org/api/solution/submit' > results/" + str(i) + ".txt"
    )
