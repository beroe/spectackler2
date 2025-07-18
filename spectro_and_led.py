#!/usr/bin/env python3
# -*- coding: utf-8 -*-

""" Turn on LED with serial command, then run brizzy to save one spectrum, then turn off LED.
"""
import serial
import time	
import os
import sys
import seabreeze.spectrometers as sb
import seabreeze

# ARDUINOPORT = '/dev/tty.usbmodem13401'  # Change this to your Arduino's port
# ARDUINOPORT = '/dev/tty.usbserial-018D7F64'  # Change this to your Arduino's port

global DEBUG
DEBUG = True
WriteFile = False

def switch_led(state="1"):
	'''arduino should be running serial_to_LED.ino
 	probably should re-make the circuit with a better transistor'''
  
	ARDUINOPORT = '/dev/tty.usbserial-018D7F64'
	""" Turn on the LED by sending a command to the Arduino. 
 	SEE BELOW TO DO IT AT python3 prompt"""
	try:
		ser = serial.Serial(ARDUINOPORT, 9600, timeout=1)
		time.sleep(0.1)  # Wait for the serial connection to initialize
		ser.write(bytes(f"{state}",'utf-8'))  # Send command to turn on LED
		if DEBUG:
			print(f"Sent command {state} to turn {'on' if state == '1' else 'off'} the LED", file=sys.stderr)
		time.sleep(0.1)  # Wait for the command to be processed
		ser.close()  # Close the serial connection
	except serial.SerialException as e:
		print(f"Error: {e}", file=sys.stderr)
  
def capture_spectrum(integration_time=1000): # in milliseconds
	devices = sb.list_devices()
	if len(devices) == 0:
		raise IOError("""No Ocean Optics devices found.""")
	print("Found this device: {}".format(devices[0]),file=sys.stderr)
	
	spec = sb.Spectrometer(devices[0])
	if DEBUG:
		print(f"Device info: {spec}", file=sys.stderr)

	spec.integration_time_micros(int(integration_time*1000))
	x = spec.wavelengths()
	y = spec.intensities()
	spec.close()
	return x, y

switch_led("1")  # Turn on the LED
time.sleep(0.1)
time.sleep(1)  

# Commented out for testing
# myA = capture_spectrum()
# for a,b in zip(myA[0], myA[1]):
# 	print(f"{a:.2f}\t{b:.2f}")
switch_led("0")  # Turn off the LED

FileName = "spectrum.txt"
if WriteFile:
	with open(FileName, "w") as f:
		for a, b in zip(myA[0], myA[1]):
			f.write(f"{a:.2f}\t{b:.2f}\n")	
  
  
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