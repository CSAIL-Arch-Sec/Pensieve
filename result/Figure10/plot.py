
import sys, os
sys.path.append(os.getcwd())
from script.plot_helper import getResult
import numpy as np


resultDir = os.path.dirname(os.path.abspath(__file__))
shape = [4, 4, 8]
param = ["saved_params", "CPU_OOO_ROB_size", "CPU-simuCycle"]
num_test = 128


result = getResult(resultDir)
time = np.array(result["time"])
index = np.array(result["index"])
model = np.array(result["model"])

connect_last_with_dashed = [ \
  [False, False, False, False], \
  [False, False, False, False], \
  [False, False, False, False], \
  [False, False, False, True], \
]




def plot_arch(ax, yy, mm, connect_last_with_dashed):
  x_base = 4
  x_end = 10
  x = range(x_base, x_end)
  yy = yy[::-1]
  yy = [[1 if data==0 else data for data in y] for y in yy]
  yy = [[data / 60.0 for data in y] for y in yy]
  mm = mm[::-1]
  connect_last_with_dashed = connect_last_with_dashed[::-1]
  linecolors = ["lightpink", "dodgerblue", "green", "black"][::-1]
  labels = ["ROB Size = 2", "ROB Size = 4", "ROB Size = 8", "ROB Size = 16"][::-1]


  unfinisheds = [[] for _ in yy]
  for (y, m, color, unfinished) in zip(yy, mm, linecolors, unfinisheds):
    secure_x = []
    secure_y = []
    attack_x = []
    attack_y = []
    for (data, data_model, data_x) in zip(y, m, x):
      if data_model==0:
        secure_x.append(data_x)
        secure_y.append(data)
      elif data_model==1:
        attack_x.append(data_x)
        attack_y.append(data)
      else:
        unfinished.append(data_x - x_base)

    ax.scatter(secure_x, secure_y, marker="o", s=320, alpha=0.7, color=color,edgecolor='None')
    ax.scatter(attack_x, attack_y, marker="x", s=280, lw=3, alpha=0.7, color=color,edgecolor='None')
  

  for (y, linecolor, label, unfinished, dashed) in \
    zip(yy, linecolors, labels, unfinisheds, connect_last_with_dashed):
    x_temp = list(x).copy()
    y_temp = y.copy()
    for i in unfinished[::-1]:
      del x_temp[i]
      del y_temp[i]

    if dashed:
      ax.plot(x_temp[:3], y_temp[:3], color=linecolor, linewidth=3, label=label)
      ax.plot(x_temp[2:4], y_temp[2:4], color=linecolor, linewidth=3, linestyle="dashed")
      ax.plot(x_temp[3:], y_temp[3:], color=linecolor, linewidth=3, label=label)
    else:
      ax.plot(x_temp, y_temp, color=linecolor, linewidth=3, label=label)


  ax.set_ylim([0.01, 20000])
  ax.set_yscale("log")
  ax.set_yticks([0.1, 1, 10, 100, 1000, 10000], ["0.1", "1", "10", "100", r"$10^3$", r"$10^4$"])
  ax.grid(axis='y', linestyle='--', alpha=0.6)
  ax.set_xlabel("Simulated Cycle", fontsize=22)
  ax.tick_params(axis='x', labelsize=18)
  ax.tick_params(axis='y', labelsize=16)


import matplotlib
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42
gs = gridspec.GridSpec(3, 4, width_ratios=[1, 1, 1, 1], height_ratios=[1, 8, 1])
fig = plt.figure(figsize=(28,5))
ax0 = fig.add_subplot(gs[1, 0])
ax1 = fig.add_subplot(gs[1, 1])
ax2 = fig.add_subplot(gs[1, 2])
ax3 = fig.add_subplot(gs[1, 3])

time_to_plot = [[y[1:-1] for y in yy] for yy in result["time"]]
model_to_plot = [[y[1:-1] for y in yy] for yy in result["model"]]
plot_arch(ax0, time_to_plot[0], model_to_plot[0], connect_last_with_dashed[0])
plot_arch(ax1, time_to_plot[1], model_to_plot[1], connect_last_with_dashed[1])
plot_arch(ax2, time_to_plot[2], model_to_plot[2], connect_last_with_dashed[2])
plot_arch(ax3, time_to_plot[3], model_to_plot[3], connect_last_with_dashed[3])


ax0.set_ylabel("Checking Time (min)", fontsize=22)
ax0.yaxis.set_label_coords(-0.15, 0.47)
legend = ax0.legend(loc="lower left", bbox_to_anchor=(-0.018, 1.01), ncol=4, fontsize=20)
#legend = ax0.legend(loc="lower left", bbox_to_anchor=(4.62, 0.0), ncol=1, fontsize=20)
ax0.add_artist(legend)
label1 = plt.scatter([], [], marker="o", s=320, alpha=0.7, color="black",edgecolor='None', label="no counterexample")
label2 = plt.scatter([], [], marker="x", s=280, lw=3, alpha=0.7, color="black",edgecolor='None', label="counterexample")
ax0.legend(handles=(label1, label2), loc="lower left", bbox_to_anchor=(2.73, 1.01), ncol=2, fontsize=20)
#ax0.legend(handles=(label1, label2), loc="lower left", bbox_to_anchor=(4.62, 0.65), ncol=1, fontsize=20)


ax0.text(0.02, -0.36, "(a) Baseline O3 Processor", fontsize=22, fontweight="bold", transform=ax0.transAxes)
ax1.text(0.16, -0.36, "(b) Delay-on-miss", fontsize=22, fontweight="bold", transform=ax1.transAxes)
ax2.text(0.04, -0.36, "(c) Invisible Speculation", fontsize=22, fontweight="bold", transform=ax2.transAxes)
ax3.text(0.20, -0.36, "(d) GhostMinion", fontsize=22, fontweight="bold", transform=ax3.transAxes)



plt.savefig(resultDir + "/performance.pdf", bbox_inches="tight")


print(index[:])
print("\n\n")
print(model[:])
print("\n\n")
print(time[:])

