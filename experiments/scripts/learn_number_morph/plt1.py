

import numpy as np
from matplotlib import pyplot as plt


# These are the "Tableau 20" colors as RGB.  
tableau20 = [(31, 119, 180), (174, 199, 232), (255, 127, 14), (255, 187, 120),  
             (44, 160, 44), (152, 223, 138), (214, 39, 40), (255, 152, 150),  
             (148, 103, 189), (197, 176, 213), (140, 86, 75), (196, 156, 148),  
             (227, 119, 194), (247, 182, 210), (127, 127, 127), (199, 199, 199),  
             (188, 189, 34), (219, 219, 141), (23, 190, 207), (158, 218, 229)]  
  
# Scale the RGB values to the [0, 1] range, which is the format matplotlib accepts.  
for i in range(len(tableau20)):  
    r, g, b = tableau20[i]  
    tableau20[i] = (r / 255., g / 255., b / 255.)  

# read in data from data.txt
data = eval(open('data1_20_first50.py').readlines()[0])
data = np.array(data)[:,::-1]

fig = plt.figure()
ax = fig.add_subplot(111)
for i, row in enumerate(data.T): 
    ax.plot(row, 
            label=i, 
            color=tableau20[i],
            linewidth=2
    )
    
ax.set_ylim(-80, 0)
ax.legend()
plt.show()


