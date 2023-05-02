
import json
import numpy as np

PRINT_FAIL = False


def summary(resultDir, num_test, shape):

  def getTime(fileName):
    try:
      with open(fileName) as f:
        for line in f.readlines():
          words = line.split()
          if len(words) > 0 and words[0] == "cpu":
            return int(int(words[5]) / 1000)
    except:
      return -1
    if PRINT_FAIL:
      print(fileName + " FAIL!!!")
    return -1

  def getModel(fileName):
    finished = False
    try:
      with open(fileName) as f:
        for line in f.readlines():
          words = line.split()
          if len(words) > 0 and words[0] == "cpu":
            finished = True
          if len(words) > 0 and words[0] == "Find":
            return 1 # find attack program
    
    except:
      return -1 # smt have not finished

    if not finished:
      return -1 # smt have not finished

    return 0 # is secure


  # STEP1: Collect
  result = {}
  time = []
  model = []
  for i in range(num_test):
    time.append(getTime(resultDir + "/runbatch-%d.out" % i))
    model.append(getModel(resultDir + "/runbatch-%d.out" % i))
  result["time"] = np.reshape(time, shape).tolist()
  result["model"] = np.reshape(model, shape).tolist()
  result["index"] = np.reshape(range(num_test), shape).tolist()


  # STEP2: Write to Buffer
  with open(resultDir + "/summary.json", "w") as f:
    json.dump(result, f)
  return result

