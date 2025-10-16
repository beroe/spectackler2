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

pump = isco.ISCOController(port=pumpport,source=0,dest=6)
if pump:
	print("Found pump",file=sys.stderr)
pump.run()
CurrentPress = pump.press_set({"A":""})
print("Current pressure",CurrentPress)
pump.maxflow(10)
pump.press_set(10)
time.sleep(5)
CurrentPress = pump.press_set()
print("New pressure",CurrentPress)

pump.stop()


'''
pump.gg()  
b'R0B5= 00025,-01574,-01748,-01461,-01497,-01505,-01496,-01489,11111001, 0000162855,0098586030,R,R,-, 0000000000,000000m437,S,R,N, 0000000000,000000g655,S,R,N, 0000162855, 00025,00000000,66\r'
pump.press_set()
5.0
pump.press_set(15)
True
pump.press_set()
15.0
pump.run()
'''