
import sys, os
sys.path.append(os.getcwd())
from run import get_experiment_list


if __name__ == "__main__":
  from script.run_helper_local import runBatch
  runBatch(os.path.dirname(os.path.abspath(__file__)), get_experiment_list())

