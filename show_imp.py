# Script to render MNIST data
#
# Usage:
# python <script_name> <file_path> <sample_index>

import sys

import numpy as np
import matplotlib.pyplot as plt

def main():

    file_path = sys.argv[1]
    index = int(sys.argv[2])

    data = np.loadtxt(file_path, delimiter=',', dtype=np.int32)
    data = np.asarray([np.array_split(lst, 28) for lst in data])

    plt.matshow(data[index])
    plt.show()

if __name__ == '__main__':
    main()
