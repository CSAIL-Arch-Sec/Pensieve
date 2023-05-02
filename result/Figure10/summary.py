
import sys, os
sys.path.append(os.getcwd())
from script.summary_helper import summary
import numpy as np


resultDir = os.path.dirname(os.path.abspath(__file__))
num_test = 128
shape = [4, 4, 8]

summary(resultDir, num_test, shape)

