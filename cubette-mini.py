#!/usr/bin/env python3

from __future__ import print_function

"""
Follow a supplied temperature/pressure program,
logging process and reference temps all the way

Turn bath on manually and push the computer button.

"""
## ATTENTION: This script is set up for Unix

# Define serial port to which the NESLAB waterbath and ISCO 260D syringe pump are connected
# Load statefile for temperature/pressure experiment
from config import portBATH,portPRES,stateFILE
bathport = portBATH
pumpport = portPRES

from os import getcwd, makedirs
import os
import sys
import argparse
import time
import numpy as np
from pandas import read_csv
import neslabrte as neslab
import isco260D as isco
import subprocess
from datetime import datetime


def parse_args():
	parser = argparse.ArgumentParser(description='Control Pressure and Temperature')
	parser.add_argument('-b', '--debug', action='store_true', help='Enable debug output')
	return parser.parse_args()

#-----------------------------------
# Log file
#-----------------------------------

log_header = ["clock", "watch", "tint", "pres", "spectrum"]

def logstate(bath=None,pump=None,hand_log=None, spectrum=False):
	data = {
			"clock": time.strftime("%Y%m%d %H%M%S"), 
			"watch": int(time.time() - time_start),
			"tint": bath.temp_get_int() if bath else None,
			"pres": pump.press_get() if pump else None,
			"spectrum": "TRUE" if spectrum else "FALSE"
			}
	hand_log.write('\t'.join(str(data[key]) for key in log_header) + '\n')

#-----------------------------------
# Main script
#-----------------------------------
opt=parse_args()

delaytime = 1 # 60 seconds but set to be shorter for testing, delay between spectral measurements

# read statefile		
print(f"Reading states from {stateFILE}", file=sys.stderr)
states = read_csv(stateFILE, sep='\t')
if opt.debug:
	print("States\n",states,file=sys.stderr)

#-----------------------------------
# Create experiment folder with daily numbering
#-----------------------------------

today = datetime.now().strftime("%Y%m%d")
data_dir = getcwd() + "/data"

# list existing experiment folders for today
existing_today = [d for d in os.listdir(data_dir)
		  if d.startswith("Experiment_") and today in d]

exp_num = len(existing_today) + 1
experiment_timestamp = datetime.now().strftime("%Y%m%dT%H%M%S")
experiment_folder = os.path.join(data_dir, f"Experiment_{exp_num}_{experiment_timestamp}")
makedirs(experiment_folder, exist_ok=True)

if opt.debug:
	print(f"Experiment folder: {experiment_folder}", file=sys.stderr)

# save log file inside experiment folder
file_log = os.path.join(experiment_folder, f"plog_{experiment_timestamp}.tsv")

timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
print(f"Starting run at {timestamp}")

with open(file_log, 'a+', buffering=1) as hand_log:

	# write header
	hand_log.write('\t'.join(log_header) + '\n')
	
	# init water bath
	bath = neslab.NeslabController(port=bathport)
	time.sleep(1)

	if opt.debug:
		for x in bath.status_get():
			print(x)

	# init ISCO pump
	# default destination on pump is 6
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
		
		# Set the temperature and wait for it to reach the target
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
		print(f"Waiting {timeinterval/60:.1f} minutes...", file=sys.stderr)
		time.sleep(timeinterval)
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
		print(f"Waiting {timeinterval/60:.1f} minutes...", file=sys.stderr)
		time.sleep(timeinterval)  

		# log state before taking spectra
		logstate(bath=bath,pump=pump,hand_log=hand_log,spectrum=False)

		# take spectra, running spectro_and_led.py
		ipres = int(pres)
		itint = f"{tint:.1f}"
		# run spectro_and_led.py, saving spectra inside experiment folder
		ipres = int(pres)
		itint = f"{tint:.1f}"

		# take 3 spectra, each with a unique filename so they do not overwrite
		for j in range(3):
			# create a unique filename: timestamp with microseconds + index
			spec_ts = datetime.now().strftime("%Y%m%dT%H%M%S%f")
			spec_filename = os.path.join(
				experiment_folder,
				f"spectrum_{spec_ts}_idx{j+1}_P{ipres}_T{itint}.tsv"
			)

			# call spectro_and_led.py with the full output filename
			scommand = (
				f'python3 /Users/haddock/repos/spectackler2/spectro_and_led.py '
				f'-w -e "{spec_filename}" -t 1000'
			)

			print(f"Taking spectrum {j+1}", file=sys.stderr)
			logstate(bath=bath, pump=pump, hand_log=hand_log, spectrum=True)  # spectrum start
			subprocess.run(scommand, bufsize=1, shell=True)
			time.sleep(delaytime)
			logstate(bath=bath, pump=pump, hand_log=hand_log, spectrum=False)  # spectrum end
		
	# shut down hardware
	timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
	print(f"All done! {timestamp}", file=sys.stderr)
	pump.stop()
	bath.on(False)
