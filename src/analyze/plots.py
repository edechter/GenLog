#
# Plotting tools for GenLog experiments
# ----------------------------------------------------------------------


import numpy as np
from matplotlib import pyplot as plt

import pandas as pd

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


def plot_loglikelihood_lines(data, ymin=-100, ax=None):
    if ax is None:
        fig = plt.figure()
        ax = fig.add_subplot(111)

    for i, row in enumerate(data): 
        ax.plot(row, 
                label=i, 
                color=tableau20[i],
                linewidth=2
            )
        
    ax.set_ylim(-80, 0)
    ax.legend(pos='lower right')
    return ax
    
def plot_loglikelihood_raster(data, ymin=-100, ax=None):
    if ax is None:
        fig = plt.figure()
        ax = fig.add_subplot(111)
        
    im = ax.pcolormesh(data, vmin=-100, vmax=1, cmap=plt.cm.Blues)
    plt.colorbar(im)
    ax.set_yticks([i+0.5 for i in range(data.shape[0])])
    ax.set_yticklabels([i for i in range(1, data.shape[0]+1)])
    return ax

  



if __name__=="__main__":
    # read in data from data.txt
    data = eval(open('data1_20_first50.py').readlines()[0])
    data = np.array(data)[:, ::-1].T
    

    fig, ax = plt.subplots(nrows=1, figsize=(6, 6))
    plot_loglikelihood_raster(data, ax=ax)

    plt.show()
