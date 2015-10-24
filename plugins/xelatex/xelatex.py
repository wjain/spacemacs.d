#!/usr/bin/python
import sys
import subprocess

xelatex_cmd = ["xelatex"]
xelatex_cmd.append("-interaction")
xelatex_cmd.append("nonstopmode")

for i in range(1, len(sys.argv)):
    xelatex_cmd.append(sys.argv[i])

subprocess.call(xelatex_cmd)

exit(0)
