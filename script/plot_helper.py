
import json
import numpy as np


def getResult(resultDir):
  with open(resultDir + "/summary.json",'r') as f:
    result = json.load(f, object_hook=lambda d: {int(k) if k.lstrip('-').isdigit() else k: v for k, v in d.items()})
  return result

