
import sys, os
sys.path.append(os.getcwd())


def get_experiment_list():
  param_saved_params_list = ["spectre", "DoM", "invisiSpec", "GhostMinion"]
  param_ROB_size_list = [2, 4, 8, 16]
  param_CPU_simuCycle_list = [3, 4, 5, 6, 7, 8, 9, 10]

  experiment_list = []
  inputId = 0
  for param_saved_params in param_saved_params_list:
    for param_ROB_size in param_ROB_size_list:
      for param_CPU_simuCycle in param_CPU_simuCycle_list:
        inputParam = "++arg --param-debug-assert ++arg 0 " \
                   + "++arg --param-saved-params ++arg %s " % param_saved_params \
                   + "++arg --param-ROB-size ++arg %d " % param_ROB_size \
                   + "++arg --param-CPU-simuCycle ++arg %d " % param_CPU_simuCycle

        experiment_list.append({"inputParam": inputParam, "inputId": str(inputId)})
        inputId += 1
  print("[experiment_list]:", experiment_list)
  return experiment_list


if __name__ == "__main__":
  from script.run_helper_local import runBatch
  runBatch(os.path.dirname(os.path.abspath(__file__)), get_experiment_list())

