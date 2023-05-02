
import os


if __name__ == "__main__":

  command = "cp -r . Pensieve"
  print("[command to run]: ", command)
  os.system(command)

  command = "rm -rf Pensieve/.git"
  print("[command to run]: ", command)
  os.system(command)

  command = "find Pensieve -name \".DS_Store\" -print -delete"
  print("[command to run]: ", command)
  os.system(command)

  command = "zip -r Pensieve.zip Pensieve"
  print("[command to run]: ", command)
  os.system(command)

  command = "rm -rf Pensieve"
  print("[command to run]: ", command)
  os.system(command)

