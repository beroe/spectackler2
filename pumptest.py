#! /usr/bin/env python3
from os import getcwd
import sys
import traceback
import time
import numpy as np
import pandas as pd
import neslabrte as neslab
import isco260D as isco


pumpport = "/dev/tty.usbserial-FT4IVKAO0"

pump = isco.ISCOController(port=pumpport,source=0,dest=1)
if pump:
	print("Found pump",file=sys.stderr)
pump.run()
CurrentPress = pump.press_set({"A":""})
print("Current pressure",CurrentPress)
# pump.press_set("5")
time.sleep(5)
CurrentPress = pump.press_set()
print("New pressure",CurrentPress)

pump.stop()