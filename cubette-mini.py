#!/usr/bin/env python3

from __future__ import print_function

"""
Follow a supplied temperature/pressure program,
logging process and reference temps all the way

Turn bath on manually and push the computer button.

"""
## ATTENTION: this script is set up for Windows

# the serial port to which the NESLAB waterbath is connected
from config import portBATH,portPRES,stateFILE
bathport = portBATH
pumpport = portPRES

from os import getcwd
import sys
import argparse
import time
import numpy as np
from pandas import read_csv
import neslabrte as neslab
import isco260D as isco
import subprocess

def parse_args():
	parser = argparse.ArgumentParser(description='Control Pressure and Temperature')
	parser.add_argument('-b', '--debug', action='store_true', help='Enable debug output')
	return parser.parse_args()

def logstate(bath="",pump="",hand_log=""):
	data = {
			"clock": time.strftime("%Y%m%d %H%M%S"), 
			"watch": int(time.time() - time_start),
			"tint": bath.temp_get_int(),
			"pres": pump.press_get(),
			}
	pres=data.get("pres")
	tint=data.get("tint")
	hand_log.write('\t'.join([str(data[key]) for key in sorted(data.keys())]) + '\n')

opt=parse_args()

delaytime = 1
longdelay = 2 

# start a timestamped datalog file
file_log = getcwd() + r"/data/plog_" + time.strftime("%Y%m%dT%H%M") + ".tsv"
		
print(f"Reading states from {stateFILE}", file=sys.stderr)
states = read_csv(stateFILE, sep='\t')
if opt.debug:
	print("States\n",states,file=sys.stderr)
# open output file

timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
print(f"Starting run at {timestamp}")

with open(file_log, 'a+', buffering=1) as hand_log:

	# write header
	# header = sorted(["clock", "watch", "temp_int", "temp_ext", "press_act"] + list(states.head()))
	# hand_log.write('\t'.join(header) + '\n')
	
	# init water bath
	bath = neslab.NeslabController(port=bathport)
	time.sleep(1)

	if opt.debug:
		for x in bath.status_get():
			print(x)

	# init ISCO pump
	# Pretty sure the default destination is 6, so changing here
	pump = isco.ISCOController(port=pumpport,source=0,dest=6)
	pump.run()
	pump.maxflow(15)

	## run experiment
	
	# start experiment timer (i.e. stopwatch)
	time_start = time.time() # in seconds
	
	# iterate over test states
	oldint=0
	for i,state in states.iterrows():
		temp=state.Temp
		pres=state.Press
		timeinterval = state.Time

		time_state = time.time()
		tint = bath.temp_get_int()
		print(f"Setting temp to {temp}. Currently {tint}", file=sys.stderr)
		
		# SET The temperature and wait for it to reach the target
		bath.temp_set(temp)
		while not tint==temp: 
			tint=bath.tint()
			newint = int(time.time() - time_state)
			if newint != oldint and newint % 10 == 0: # report temp
				oldint=int(time.time() - time_state)
				print(f"T:{tint}")
				logstate(bath=bath,pump=pump,hand_log=hand_log)


		print(f"Temp has reached {tint} deg C", file=sys.stderr)
	
		# Wait a couple minutes to account for overshoot
		print(f"Waiting {longdelay/60:.1f} minutes...", file=sys.stderr)
		time.sleep(longdelay)
		# set bath temperature persistently

		time_state=time.time()
		# set pressure persistently
		print(f"Setting press to {pres}", file=sys.stderr)
		pump.press_set(pres)
		while not pump.press_get() == pres: 
			newint = int(time.time() - time_state)
			if newint != oldint and newint % 10 == 0: # report temp
				oldint=int(time.time() - time_state)
				try:
					pre=pump.press_get()
					print(f"P:{pre}")
				except IndexError:
					print(f"can't read pressure. set to {pres}")

		print(f"Pressure reached: {pres}", file=sys.stderr)
		# wait data for the prescribed period

		print(f"Waiting {longdelay/60:.1f} minutes...", file=sys.stderr)
		time.sleep(longdelay)  

		logstate(bath=bath,pump=pump,hand_log=hand_log)
		ipres = int(pres)
		itint = (f"{tint:.1f}")
		scommand = f'/Users/haddock/repos/spectackler2/spectro_and_led.py -w -e "_P{ipres}_T{itint}" -t 1000'

		for i in range(3):
			print(f"Taking spectrum {i+1}", file=sys.stderr)
			subprocess.run(scommand,bufsize=1,shell=True)
			time.sleep(delaytime)

		logstate(bath=bath,pump=pump,hand_log=hand_log)
		# shut down hardware
		b
	timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
	print(f"All done! {timestamp}", file=sys.stderr)
	pump.stop()
	bath.on(False)
