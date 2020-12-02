#!/bin/python
import subprocess
import sys

cmd = ['docker', 'cp', f'{sys.argv[1]}', f'9e:/Cosette/tmp.cos']
subprocess.run(cmd)

cmd = ['docker', 'exec', '-w', '/Cosette', '9e', 'python', 'solve.py', f'tmp.cos']
subprocess.run(cmd)