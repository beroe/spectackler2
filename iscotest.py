#!/usr/bin/env python3

"""
initialize ISCO pump, then provide user with an interactive console for debugging
"""

import isco260D as isco
from time import sleep
import traceback

def gg2psi(gg_str):
    "get PSI value from G& get all status string"
    return float(gg_str.decode("utf-8").split('=')[1].split(',')[0]) / 5

pump = isco.ISCOController(port="/dev/cu.usbserial-FT4IVKAO0",source=0,dest=6)

print('''Example commands:
   press_set()
   disconnect()
   press_get()
   flow_get()
''')

pump.remote()
print(pump.gg())
KeepGoing = True
while KeepGoing:
    cmd = input("pump.")
    if cmd == "x":
        pump.disconnect()
        KeepGoing=False
    else:
        try:
            ret = eval("pump.{}".format(cmd))
            print(ret)
        except:
            print("didn't work")

