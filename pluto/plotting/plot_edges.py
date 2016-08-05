import matplotlib.pyplot as plt
import sys

plt.figure(figsize=(5, 5))
plot_margin = 0.05

print "WHOA"

for line in sys.stdin:
    print "WHOA"
    print line

    fx, fy, tx, ty = [float(x.strip()) for x in line.split(" ") if len(x) > 0]

    plt.plot([fx, tx], [fy, ty], '-')

plt.gca().set_aspect('equal', adjustable='box')
x0, x1, y0, y1 = plt.axis()
plt.axis((x0 - plot_margin, x1 + plot_margin, y0 - plot_margin, y1 + plot_margin))
plt.show()
