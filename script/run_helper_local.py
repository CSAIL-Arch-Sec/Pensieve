

def initClient():
  from dask.distributed import Client, LocalCluster
  import time

  # STEP1 choose a cluster
  cluster = LocalCluster(threads_per_worker=1)
  # cluster = LocalCluster(threads_per_worker=1, n_workers=16)

  time.sleep(1)
  print(cluster)


  # STEP2 setup a client
  client = Client(cluster)

  return client


def runBatch(resultDir, experiment_list):
  import os, time
  # STEP1 init the cluster or multiProcess
  client = initClient()


  # STEP2 mark start time
  startTime = time.time()


  # STEP3 run them
  futureList = []
  for experiment in experiment_list:
    inputParam = experiment["inputParam"]
    inputId = experiment["inputId"]
    cmd = "raco test --timeout 604800 %s %s/src/main_veriSpec.rkt >> %s/runbatch-%s.out" % \
          (inputParam, os.getcwd(), resultDir, inputId)
    futureList.append(client.submit(os.system, cmd))

  for i, future in enumerate(futureList):
    future.result()
    print("----------> Finish %d/%d Simu, After %f minutes" % \
      (i+1, len(experiment_list), (time.time() - startTime)/60))

