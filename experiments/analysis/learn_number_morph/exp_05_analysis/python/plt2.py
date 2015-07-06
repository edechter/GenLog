

import numpy as np
from matplotlib import pyplot as plt

import glob
import json

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

files = glob.glob('out_gl*json')

min_val = -70

fig = plt.figure()

num_files = len(files)
for i, file in enumerate(files):
    print file
    ax = fig.add_subplot(num_files, 1, i+1)

    obj = json.load(open(file, 'r'))
    xs = np.array([[x['number'], x['loglikelihood']] for x in obj])
    xs[xs < min_val]=min_val
    ax.bar(xs[:,0], -xs[:,1], color=tableau20[np.mod(i, 20)])
    ax.set_yticks([])
    ax.set_xticks(range(1, 100))




