#!/usr/bin/env python3

from __future__ import print_function

"""
Follow a supplied temperature/pressure program,
logging process and reference temps all the way.

Turn bath on manually and push the computer button.
"""

# Define serial port to which the NESLAB waterbath and ISCO 260D syringe pump are connected
from spec_config import portBATH, portPRES, stateFILE
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


# -----------------------------------
# Log file
# -----------------------------------

log_header = ["clock", "watch", "tint", "pres", "spectrum"]

def logstate(bath=None, pump=None, hand_log=None, spectrum=False):
    data = {
        "clock": time.strftime("%Y%m%d %H%M%S"),
        "watch": int(time.time() - time_start),
        "tint": bath.temp_get_int() if bath else None,
        "pres": pump.press_get() if pump else None,
        "spectrum": "TRUE" if spectrum else "FALSE"
    }
    hand_log.write('\t'.join(str(data[key]) for key in log_header) + '\n')


# -----------------------------------
# Main script
# -----------------------------------
opt = parse_args()

delaytime = 10  # seconds between spectral measurements
n_reps = 3 # number of spectra measured per P/T step

# read statefile
print(f"Reading states from {stateFILE}", file=sys.stderr)
states = read_csv(stateFILE, sep='\t')
if opt.debug:
    print("States\n", states, file=sys.stderr)

# -----------------------------------
# Create experiment folder with daily numbering
# -----------------------------------

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
    pump = isco.ISCOController(port=pumpport, source=0, dest=6)
    pump.run()
    pump.maxflow(15)

    # start experiment timer
    global time_start
    time_start = time.time()

    oldint = 0

    # iterate over test states
    for i, state in states.iterrows():
        temp = state.Temp
        pres = state.Press
        press_wait = state.Press_time
        temp_wait = state.Temp_time

        time_state = time.time()
        tint = bath.temp_get_int()
        print(f"Setting temp to {temp}. Currently {tint}", file=sys.stderr)

        # --- Set temperature ---
        bath.temp_set(temp)
        while tint != temp:
            tint = bath.tint()
            newint = int(time.time() - time_state)
            if newint != oldint and newint % 10 == 0:  # report temp every 10s
                oldint = int(time.time() - time_state)
                print(f"T:{tint}")
                logstate(bath=bath, pump=pump, hand_log=hand_log)
        print(f"Temp has reached {tint} °C", file=sys.stderr)

        # --- Wait after temperature change only ---
        if temp_wait > 0:
            print(f"Waiting {temp_wait/60:.1f} minutes for temperature stabilization...", file=sys.stderr)
            time.sleep(temp_wait)

        # --- Set pressure ---
        print(f"Setting press to {pres}", file=sys.stderr)
        time_state = time.time()
        pump.press_set(pres)
        while pump.press_get() != pres:
            newint = int(time.time() - time_state)
            if newint != oldint and newint % 10 == 0:  # report pressure every 10s
                oldint = int(time.time() - time_state)
                try:
                    pre = pump.press_get()
                    print(f"P:{pre}")
                except IndexError:
                    print(f"Can't read pressure. Target = {pres}")
        print(f"Pressure reached: {pres}", file=sys.stderr)

        # --- Wait after pressure change only ---
        if press_wait > 0:
            print(f"Waiting {press_wait/60:.1f} minutes for pressure stabilization...", file=sys.stderr)
            time.sleep(press_wait)

        # --- Log state and take spectra ---
        logstate(bath=bath, pump=pump, hand_log=hand_log, spectrum=False)

        ipres = int(pres)
        itint = f"{tint:.1f}"
        for j in range(n_reps):
            spec_ts = datetime.now().strftime("%Y%m%dT%H%M%S%f")
            spec_filename = os.path.join(
                experiment_folder,
                f"spectrum_{spec_ts}_idx{j+1}_P{ipres}_T{itint}.tsv"
            )

            scommand = (
                f'python3 /Users/haddock/repos/spectackler2/spectro_and_led.py '
                f'-w -e "{spec_filename}" -t 200' #Adjust -t for integration time (in ms)
            )

            print(f"Taking spectrum {j+1}/{n_reps}", file=sys.stderr)
            logstate(bath=bath, pump=pump, hand_log=hand_log, spectrum=True)
            subprocess.run(scommand, bufsize=1, shell=True)
            logstate(bath=bath, pump=pump, hand_log=hand_log, spectrum=False)
            
            # wait delaytime if taking multiple measurements
            if n_reps > 1 and j < n_reps - 1:
                time.sleep(delaytime)

    # shut down hardware
    timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
    print(f"All done! {timestamp}", file=sys.stderr)
    pump.stop()
    bath.on(False)