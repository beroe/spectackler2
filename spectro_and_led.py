#!/usr/bin/env python3
# -*- coding: utf-8 -*-

""" Turn on LED with serial command, then run brizzy to save one spectrum, then turn off LED.
v 0.4 added temperature control and made delay just at the start
v 0.3 add repeat and delay before capture
v 0.2 autoreveal arduino port
Requirements:
pip3 install pyserial
pip3 install seabreeze
 
"""
import serial
import asyncio
import glob
import os
import sys
import time
import argparse
import seabreeze.spectrometers as sb
import seabreeze
from datetime import datetime
from spec_config import portARDU
# TODO: get portARDU from a glob.glob

def parse_args():
	parser = argparse.ArgumentParser(description='Control LED and capture spectrum from Ocean Optics spectrometer')
	parser.add_argument('-b', '--debug', action='store_true', help='Enable debug output')
	parser.add_argument('-w', '--write', action='store_true', help='Write spectrum data to file instead of stdout')
	parser.add_argument('-i', '--invert', action='store_false', help='Normal LED control logic (default: 0=on, 1=off)', default=True)
	parser.add_argument('-e','--extra',type=str, default="",help='append to file name')
	parser.add_argument('-t', '--time', type=int, default=1000, help='Integration time in milliseconds (default: 1000)')
	parser.add_argument('-d', '--delay', type=int, default=0, help='Wait before taking spectrum')
	parser.add_argument('-r', '--repeat', type=int, default=1, help='Number of times to repeat the measurement max 100 (default: 1)')
	parser.add_argument('-c', '--cool', type=float, default=-1, help='TEC setpoint in °C for QE spectrometers (default: -1, disabled)')

	return parser.parse_args()

async def switch_led(state="1", debug=False,invert=False):
	'''arduino should be running serial_to_LED.ino
	probably should re-make the circuit with a better transistor
 	LED controlled is backwards, so these need to be reversed if not controlling directly'''
	Port=glob.glob("/dev/tty.usbmodem*")
	# TODO look up usbmodem number automatically
	ARDUINOPORT = '/dev/tty.usbserial-018D7F64'
	ARDUINOPORT = '/dev/tty.usbmodem143301'  # Arduino micro
	if len(Port) == 1:
		ARDUINOPORT = Port[0]
		print (f"Auto-discovered Arduino Port: {ARDUINOPORT}", file=sys.stderr)
	else:
		ARDUINOPORT = portARDU

	""" Turn on the LED by sending a command to the Arduino. 
	SEE BELOW TO DO IT AT python3 prompt"""
	if invert:
		state = "0" if state == "1" else "1"
  
	try:
		ser = serial.Serial(ARDUINOPORT, 9600, timeout=1)
		await asyncio.sleep(0.1)  # Wait for the serial connection to initialize
		ser.write(bytes(f"{state}",'utf-8'))  # Send command to turn on LED
		if debug:
			print(f"Sent command {state} to turn {'on' if state == '1' else 'off'} the LED", file=sys.stderr)
		await asyncio.sleep(0.1)  # Wait for the command to be processed
		ser.close()  # Close the serial connection
	except serial.SerialException as e:
		print(f"Error: {e}", file=sys.stderr)
  
def set_cooling(spec, setpoint, debug=False):
	tec=False
	if 'QE' not in spec.model:
		if debug:print(f"Cooling not supported for {spec.model}, skipping.", file=sys.stderr)
		return
	tec = spec.f.thermo_electric
	tec.enable_tec(True)
	tec.set_temperature_setpoint_degrees_celsius(setpoint)
	time.sleep(2)
	actual = tec.read_temperature_degrees_celsius()
	print(f"TEC setpoint: {setpoint:.1f} °C, current temperature: {actual:.1f} °C", file=sys.stderr)
	return tec

def find_spectrometer(debug=False):
	devices = sb.list_devices()
	if len(devices) == 0:
		raise IOError("No Ocean Optics devices found.")
	if debug:
		print("Found a spectrometer: {}".format(devices[0]), file=sys.stderr)
	spec = sb.Spectrometer(devices[0])
	if debug:
		print(f"Device info: {spec}", file=sys.stderr)
	return spec

async def capture_spectrum(spec, integration_time=1000, debug=False): # in milliseconds
	print('\a',file=sys.stderr) # makes a beep when delay is over
	print(f"Taking spectrum with {integration_time} ms integration time", file=sys.stderr)
	spec.integration_time_micros(int(integration_time*1000))
	x = spec.wavelengths()
	y = spec.intensities()
	return x, y

async def main():
	"""Main async function that coordinates LED control and spectrum capture"""
	args = parse_args()
	tecobj = False

	try:
		spec = find_spectrometer(debug=args.debug)
		if args.cool >= 0:
			tecobj = set_cooling(spec, args.cool, debug=args.debug)

		if args.delay:
			print(f"Waiting {args.delay} seconds before starting", file=sys.stderr)
			time.sleep(args.delay)
	
		for i in range(args.repeat):
  		# Turn on the LED if not repeating.
			if args.repeat ==1:
				await switch_led("1", debug=args.debug, invert=args.invert)
				await asyncio.sleep(0.1)  # LED stabilization time

			# Capture spectrum if enabled
			myA = None
			myA = await capture_spectrum(spec, integration_time=args.time, debug=args.debug)
			if not args.write and args.repeat == 1 and myA is not None:
				for a,b in zip(myA[0], myA[1]):
					print(f"{a:.2f}\t{b:.2f}")
			
			if args.repeat == 1:
				await asyncio.sleep(0.1)  # LED stabilization time
				# Turn off the LED
				await switch_led("0", debug=args.debug, invert=args.invert)
			
			# Write to file if enabled and spectrum was captured
			timestamp = datetime.now().strftime("%Y_%b_%d+%H_%M_%S")
			if os.path.isabs(args.extra):
				FileName = args.extra
				# ensure directory exists
				os.makedirs(os.path.dirname(FileName), exist_ok=True)
			else:
				if args.repeat > 1:
					FileName = os.path.join("data", f"spectrum_{timestamp}_{args.extra}_R{i+1:02d}.tsv")
				else:
					FileName = os.path.join("data", f"spectrum_{timestamp}_{args.extra}.tsv")
				os.makedirs("data", exist_ok=True)

			if args.write and myA is not None:
				with open(FileName, "w") as f:
					f.write("Wavelength\tIntensity\n")
					for a, b in zip(myA[0], myA[1]):
						f.write(f"{a:.2f}\t{b:.2f}\n")
				print(f"Wrote spectrum to {FileName}", file=sys.stderr)
		if tecobj:
			actual = tecobj.read_temperature_degrees_celsius()
			print(f"Final temperature: {actual:.1f} °C", file=sys.stderr)
		spec.close()
	except Exception as e:
		print(f"Error in main loop: {e}", file=sys.stderr)

# Run the async main function
if __name__ == "__main__":
	asyncio.run(main())	
  
  
""" 
ARDUINO SKETCH TO UPLOAD FOR LED SUPPORT: Upload to Arduino using Arduino IDE

const int ledPin = 9;
const int ledPinDirect = 10;
int inByte;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  pinMode(ledPin, OUTPUT);
  pinMode(ledPinDirect, OUTPUT);
  digitalWrite(ledPin, HIGH);
  digitalWrite(ledPinDirect, LOW);
}

void loop() {
  // put your main code here, to run repeatedly:
  if (Serial.available()) {
    // get incoming byte:
  inByte = Serial.read();
  switch (inByte) {
    case '0':
      digitalWrite(ledPin, LOW);
      digitalWrite(ledPinDirect, HIGH);
      break;
    case '1':
      digitalWrite(ledPin,HIGH);
      digitalWrite(ledPinDirect, LOW);
      break;
  }
  }
}
"""

"""
import serial
ARDUINOPORT = '/dev/tty.usbserial-018D7F64'
ser = serial.Serial(ARDUINOPORT, 9600, timeout=1)
state='1'  
ser.write(bytes(f"{state}",'utf-8'))  # Send command to turn on LED



"""
