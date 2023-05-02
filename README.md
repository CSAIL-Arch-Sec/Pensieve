
# Pensieve

Pensieve is a security evaluation framework targeting early-stage microarchitectural defenses against speculative execution attacks.

Please check our ISCA 2023 paper, [Pensieve: Microarchitectural Modeling for Security Evaluation](https://people.csail.mit.edu/mengjia/data/Pensieve_ISCA_23.pdf) for more details.






## Environment Setup

### Docker

1. Run `docker-compose up -d` to build up the container.
2. Run `docker-compose exec env bash` to login to the container.
3. Run `docker-compose stop` to pause and `docker-compose up -d` to relaunch the container.
4. Run `docker-compose down --rmi all` to clean up. (`docker system prune` can further clean up the cache, including your cache for other projects.)






### Physical Machine

Please refer to `Dockerfile` to install racket, rosette, boolector, and dask (or HTcondor).






## Run

1. Enter the repository folder. (Mounted at `/vagrant` in the container or virtual machine)
2. Alwyas run any command in this folder.
3. We have a few pre-set parameters to analyze and find counterexamples:
  - Spectre attack on Baseline: `raco test ++arg --param-saved-params ++arg spectre ++arg --param-saved-sizes ++arg spectre src/main_veriSpec.rkt`
  - Interference attack on DoM: `raco test ++arg --param-saved-params ++arg DoM ++arg --param-saved-sizes ++arg DoM src/main_veriSpec.rkt`
  - New interference attack variant on GhostMinion: `raco test ++arg --param-saved-params ++arg GhostMinion ++arg --param-saved-sizes ++arg GhostMinion src/main_veriSpec.rkt`
4. You can further customize the parameters in `lib/param.rkt`.

We also provide scripts to run bash experiments.
Here are commands to reproduce performance result (Figure 10) in the paper:

```
# You will reproduce two files in this folder
cp -r result/Figure10 result/artifact_eval
rm result/artifact_eval/summary.json
rm result/artifact_eval/performance.pdf

# This command will take ~7 days in background
(python3 -u result/artifact_eval/run_local.py > terminal.log 2>&1 &)

# Plot a partial figure after ~10 hours
python3 result/artifact_eval/summary.py
python3 result/artifact_eval/plot.py

# Re-run two lines of commands above
# after ~7 days for a whole figure
```






## Understand the Terminal Output

### Workflow

File `src/main_veriSpec.rkt` is the high level workflow of the code. It will evaluate the design by following steps:

- Symbolically simulate the system with rosette framework. It prints "Finish Symbolic Execution" to the terminal after this step.
- Query the SMT solver to try to find a counterexample. It prints "Finish STM Solver".
- If no counterexample is found, it will print "No Counterexample".
- If a counterexample is found, it will print "Find Counterexample" and continue to provide the content of the counterexample by following steps:
  - By knowing the concrete instance of symbolic values and uninterpreted functions, it concretely simulates the system twice with two different secrets. During these concrete simulations, it prints out execution trace specified by `param-debug-print-on` in the code base. After this step, it prints out "Finish SMT Result Evaluation".
  - It provides a summary of the instruction sequence of the counterexample, the initial architecture state, and the final states of the simulation of ISA model and uarch model.

You can check these steps with `src/main_veriSpec.rkt` and search for `param-debug-print-on` in the code base if you want to customize the printouts.






## ISA

|      | op   | rs1  | rs2  | rd   |                                 |
| ---- | ---- | ---- | ---- | ---- | ------------------------------- |
| Li   | 0    |      | X    |      | Reg[rd] <- rs1                  |
| Add  | 1    |      |      |      | Reg[rd] <- Reg[rs1] + Reg[rs2]  |
| Mul  | 2    |      |      |      | Reg[rd] <- Reg[rs1] * Reg[rs2]  |
| Ld   | 3    |      | X    |      | Reg[rd] <- Mem[Reg[rs1]]        |
| St   | 4    |      |      | X    | Mem[Reg[rs1]] <- Reg[rs2]       |
| Br   | 5    |      |      | X    | If (Reg[rs2]==0) PC <- PC + rs1 |
| Jmp  | 6    |      | X    | X    | PC <- Reg[rs1]                  |






## Understand the Code and Verify Your Own Design

### General File Organization:

```
main_veriSpec.rkt
|
|-- ISASimulator.rkt
|   |-- sym-state/*
|   |-- inst.rkt
|   |-- decode.rkt
|
|-- CPU/CPU.rkt
    |-- sym-state/*
    |-- CPU/rename.rkt
    |-- CPU/ROB.rkt
    |-- evalF
    |   |-- CPU/inFetchScoreBoard.rkt
    |   |-- abs-module/brPred.rkt
    |   |-- abs-module/absFifo2.rkt
    |   |-- CPU/decode.rkt
    |-- evalE
    |   |-- CPU/issue.rkt
    |       |-- CPU/alu.rkt
    |       |   |-- abs-module/absArbiter.rkt
    |       |   |-- abs-module/absFifo.rkt
    |       |   |-- abs-module/absBufferGM.rkt
    |       |-- CPU/cache.rkt
    |           |-- abs-module/absArbiter.rkt
    |           |-- abs-module/absFifo.rkt
    |           |-- abs-module/absBufferGM.rkt
    |-- evalE
```





### Abbreviations:

- brPred: branch predictor
- rf: register file
- timFct: timing factors
- obsv: observation
- FDelay: Fetch Delay






### More Documents on the way ...

