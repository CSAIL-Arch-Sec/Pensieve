
if __name__ == "__main__":

  #sudo renice -n -20 -u yuhengy
  command = "raco test src/main_veriSpec.rkt"
  #command = "raco symtrace src/main_veriSpec.rkt"
  #command = "raco symprofile src/main_veriSpec.rkt"
  print("[command to run]: ", command)

  import os
  os.system(command)

