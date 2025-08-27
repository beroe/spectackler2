#!/usr/bin/env python3
# -*- coding: utf-8 -*-

""" Turn on LED with serial command, then run brizzy to save one spectrum, then turn off LED.
"""
import serial
import asyncio
import os
import sys
import argparse
import seabreeze.spectrometers as sb
import seabreeze
from datetime import datetime

def parse_args():
	parser = argparse.ArgumentParser(description='Control LED and capture spectrum from Ocean Optics spectrometer')
	parser.add_argument('-b', '--debug', action='store_true', help='Enable debug output')
	parser.add_argument('-w', '--write', action='store_true', help='Write spectrum data to file instead of stdout')
	parser.add_argument('-i', '--invert', action='store_false', help='Normal LED control logic (default: 0=on, 1=off)', default=True)
	parser.add_argument('-t', '--time', type=int, default=1000, help='Integration time in milliseconds (default: 1000)')

	return parser.parse_args()

async def switch_led(state="1", debug=False,invert=False):
	'''arduino should be running serial_to_LED.ino
	probably should re-make the circuit with a better transistor
 	LED controlled is backwards, so these need to be reversed if not controlling directly'''

	ARDUINOPORT = '/dev/tty.usbserial-018D7F64'
	ARDUINOPORT = '/dev/tty.usbmodem11101'  # Arduino micro

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
  
async def capture_spectrum(integration_time=1000, debug=False): # in milliseconds
	devices = sb.list_devices()
	if len(devices) == 0:
		raise IOError("""No Ocean Optics devices found.""")
	elif debug:
		print("Found a spectrometer: {}".format(devices[0]),file=sys.stderr)
	
	spec = sb.Spectrometer(devices[0])
	if debug:
		print(f"Device info: {spec}", file=sys.stderr)

	print(f"Taking spectrum with {integration_time} ms integration time", file=sys.stderr)
	spec.integration_time_micros(int(integration_time*1000))
	x = spec.wavelengths()
	y = spec.intensities()
	spec.close()
	return x, y

async def main():
	"""Main async function that coordinates LED control and spectrum capture"""
	args = parse_args()
	
	try:
		# Turn on the LED
		await switch_led("1", debug=args.debug, invert=args.invert)
		await asyncio.sleep(0.1)  # LED stabilization time
		
		# Capture spectrum if enabled
		myA = None
		# Commented out for testing - uncomment the next three lines to enable spectrum capture
		myA = await capture_spectrum(integration_time=args.time, debug=args.debug)
		if not args.write:
			for a,b in zip(myA[0], myA[1]):
				print(f"{a:.2f}\t{b:.2f}")
		
		await asyncio.sleep(0.1)  # LED stabilization time
		# Turn off the LED
		await switch_led("0", debug=args.debug, invert=args.invert)
		
		# Write to file if enabled and spectrum was captured
		timestamp = datetime.now().strftime("%Y_%b_%d+%H_%M_%S")
		FileName = f"data/spectrum_{timestamp}.tsv"
		if args.write and myA is not None:
			with open(FileName, "w") as f:
				f.write("Wavelength\tIntensity\n")
				for a, b in zip(myA[0], myA[1]):
					f.write(f"{a:.2f}\t{b:.2f}\n")
			print(f"Wrote spectrum to {FileName}", file=sys.stderr)
					
	except Exception as e:
		print(f"Error in main loop: {e}", file=sys.stderr)

# Run the async main function
if __name__ == "__main__":
	asyncio.run(main())	
  
  
""" 
ARDUINO SKETCH TO UPLOAD FOR LED SUPPORT
  const int ledPin = 9;
int inByte;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  pinMode(ledPin, OUTPUT);
}

void loop() {
  // put your main code here, to run repeatedly:
  if (Serial.available()) {
	  // get incoming byte:
	inByte = Serial.read();
	switch (inByte) {
	  case '0':
	    digitalWrite(ledPin, LOW);
	    break;
	  case '1':
	    digitalWrite(ledPin,HIGH);
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