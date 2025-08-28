#!/usr/bin/env python3

"""
initialize WATERBATH, then provide user with an interactive console for debugging
"""

import neslabrte as neslab
from time import sleep
import traceback

def gg2psi(gg_str):
    "get PSI value from G& get all status string"
    return float(gg_str.decode("utf-8").split('=')[1].split(',')[0]) / 5

bathport = "/dev/tty.usbserial-FTV5C58R0"
bath = neslab.NeslabController(port=bathport)
print("Found bath:",bath)
print('''Example commands:
   temp_set()
   bath.temp_get_int()
   bath.temp_get_ext()
   disconnect()
   status_get()
   on() # circuator
''')
KeepGoing = True
while KeepGoing:
    cmd = input("bath.")
    if cmd == "x":
        bath.disconnect()
        KeepGoing=False
    else:
        try:
            ret = eval("bath.{}".format(cmd))
            print(ret)
        except:
            traceback.print_exc()


