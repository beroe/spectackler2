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
from spec_config import portBATH,portPRES,stateFILE
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

delaytime = 60 # delay between spectral measurements (seconds)

# Read statefile
print(f"Reading states from {stateFILE}", file=sys.stderr)
states = read_csv(stateFILE, sep='\t')
if opt.debug:
	print("States\n",states,file=sys.stderr)

#-----------------------------------
# Create experiment folder with daily numbering
#-----------------------------------

today = datetime.now().strftime("%Y%m%d")
data_dir = getcwd() + "/data"

# List existing experiment folders for today
existing_today = [d for d in os.listdir(data_dir)
		  if d.startswith("Experiment_") and today in d]

exp_num = len(existing_today) + 1
experiment_timestamp = datetime.now().strftime("%Y%m%dT%H%M%S")
experiment_folder = os.path.join(data_dir, f"Experiment_{exp_num}_{experiment_timestamp}")
makedirs(experiment_folder, exist_ok=True)

if opt.debug:
	print(f"Experiment folder: {experiment_folder}", file=sys.stderr)

# Save log file inside experiment folder
file_log = os.path.join(experiment_folder, f"plog_{experiment_timestamp}.tsv")

timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
print(f"Starting run at {timestamp}")

#-----------------------------------
# Run experiment
#-----------------------------------

with open(file_log, 'a+', buffering=1) as hand_log:

	# Write headers to log file
	hand_log.write('\t'.join(log_header) + '\n')
	
	# Initialize Neslab water bath
	bath = neslab.NeslabController(port=bathport)
	time.sleep(1)

	if opt.debug:
		for x in bath.status_get():
			print(x)

	# Initialize ISCO pump
	# default destination on pump is 6
	pump = isco.ISCOController(port=pumpport,source=0,dest=6)
	pump.run()
	pump.maxflow(15) #Set flow rate on pump
	
	# Start experiment timer
	time_start = time.time() # in seconds
	
	# iterate over test states
	oldint=0
	prev_temp=None
	prev_press=None

	for i,state in states.iterrows():
		temp=state.Temp
		pres=state.Press
		timeinterval = state.Time
		press_wait = getattr(state, "Press_time", 0)
		temp_wait = getattr(state, "Temp_time", 0)

		# ---- Temperature handling ----
		if prev_temp is None or temp != prev_temp:
			print(f"Setting temp to {temp}. Currently {bath.temp_get_int()}", file=sys.stderr)
			bath.temp_set(temp)

			time_state = time.time()
			tint = bath.temp_get_int()
			while not tint == temp:
				tint = bath.tint()
				newint = int(time.time() - time_state)
				if newint != oldint and newint % 10 == 0:
					oldint = int(time.time() - time_state)
					print(f"T: {tint} °C")
					logstate(bath=bath, pump=pump, hand_log=hand_log)

			if temp_wait > 0:
				print(f"Waiting additional {temp_wait} s for temperature stabilization", file=sys.stderr)
				time.sleep(temp_wait)
		else:
			print(f"Temperature unchanged ({temp} °C), skipping wait", file=sys.stderr)
		prev_temp = temp

		# ---- Pressure handling ----
		if prev_press is None or pres != prev_press:
			print(f"Setting pressure to {pres} psi", file=sys.stderr)
			pump.press_set(pres)

			time_state = time.time()
			while not pump.press_get() == pres:
				newint = int(time.time() - time_state)
				if newint != oldint and newint % 10 == 0:
					oldint = int(time.time() - time_state)
					try:
						pre = pump.press_get()
						print(f"P: {pre} psi")
					except IndexError:
						print(f"Can't read pressure. Set to {pres} psi")

			if press_wait > 0:
				print(f"Waiting additional {press_wait} s for pressure stabilization", file=sys.stderr)
				time.sleep(press_wait)
		else:
			print(f"Pressure unchanged ({pres} psi), skipping wait", file=sys.stderr)
		prev_press = pres

		# Log state before taking spectra
		logstate(bath=bath,pump=pump,hand_log=hand_log,spectrum=False)

		# Take 3 spectra, each with a unique filename so they do not overwrite
		ipres = int(pres)
		itint = f"{tint:.1f}"
		for j in range(3):
			# Create a unique filename: timestamp with microseconds + index
			spec_ts = datetime.now().strftime("%Y%m%dT%H%M%S%f")
			spec_filename = os.path.join(
				experiment_folder,
				f"spectrum_{spec_ts}_idx{j+1}_P{ipres}_T{itint}.tsv"
			)

			# Call spectro_and_led.py with the full output filename
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
