
import sys, os
sys.path.append(os.getcwd())
from script.plot_helper import getResult
import numpy as np


resultDir = os.path.dirname(os.path.abspath(__file__))
shape = [4, 4, 8]
param = ["CPU_OOO_ROB_size", "CPU-simuCycle"]
num_test = 128


result = getResult(resultDir)
time = np.array(result["time"])
index = np.array(result["index"])
model = np.array(result["model"])


print(index[:])
print("\n\n")
print(model[:])
print("\n\n")
print(time)
print("\n\n")

