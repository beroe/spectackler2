# spectackler2

This is a stripped down clone of the octopode [spectackler repository](https://github.com/octopode/spectackler) for taking spectrophotometric measurements under pressure and temperature control.

Currently all these scripts must be run from within the repository directory saved on your local computer. 
e.g., to run `cubette-mini.py` type `./cubette-mini.py` in the terminal from within the spectackler2 repository.

## Temperature and pressure control script

`cubette-mini.py` automates the control of a **Thermo NESLAB RTE10 water bath** and an **ISCO 260D syringe pump** according to a defined temperature/pressure program. It records all measured data and takes spectral measurements using an **OceanOptics USB2000 spectrometer**. The main parent code from `spectackler` is `viscotheque_lite.py`. 

The program:
1. Reads a temperature/pressure schedule from a statefile (defined in `spectro_config.py`)
2. Iteratively sets each target temperature and pressure
3. Waits for both instroments to stabilize
4. Runs an external spectral acquisition script (`spectro_and_led.py`) to take 3 spectrum measurements at each pressure/temperature step, which saves a spectrum for each measurement labeled with the current temperature and pressure
5. Logs all readings (temperature, pressure, spectrum activity) to a time-stamped `.tsv` file

### Notes ###
* Ensure both instruments are powered ON and connected before starting
    * Run `bathtest.py` in the terminal and turn the bath on with `bath.on(True)`, then power on the bath, and then press COMPUTER to set up remote control.
    * Power on both the syringe pump and attached pump controller, and press RUN on the controller to start the pump. Ensure the baud rate is 9600 and the unit ID is 6 (default).

## Configuration ##

`spectro_config.py` contains serial port assignments for the water bath and syringe pump, along with the statefile path. Adjust the path to the statefile containing the experimental matrix you want to test. The input parameters for the experiment are input via this statefile, and should be a `.tsv` file with Press (set pressure), Temp (set temperature), Press_time (wait time between pressure steps, in seconds), and Temp_time (wait time between temperature steps, in seconds) as columns.

## Control modules ##
* `neslabrte.py`: NESLAB RTE10 water bath controller (see octopode [spectackler](https://github.com/octopode/spectackler))
* `isco260D.py`: ISCO 260D syringe pump controller (see octopode [spectackler](https://github.com/octopode/spectackler))
* `spectro_and_led.py`: Spectral acquisition script, that controls an LED connected to an Arduino and an Ocean Optics spectrometer

## LED and Spectrometer control script ##

`spectro_and_led.py` coordinates an **Arduino-controlled LED** with an **OceanOptics USB2000 spectrometer** to capture spectra under controlled illumination. It performs measurements by:
1. Turning on the LED via serial command to the Arduino
2. Capturing a spectrum using the `seabreeze` Python library
3. Turning off the LED after the spectrum is captured
4. Saving the data to a time-stamped `.tsv` file (`-w, -write` option) or to the console (default)

This script can be run standalone or be called by another program (e.g., `cubette-mini.py`).

### Command line arguments ###
* `-b`, `--debug`: Prints extra diagnostic output
* `-w`, `--write`: Writes spectrum data to a `.tsv` file instead of in console. Data are saved as: `data/spectrum_YYYY_Mmm_DD+HH_MM_SS<extra>.tsv`
* `-i`, `--invert`: Inverts LED logic (default: 0=on, 1=off)
* `-e`,`--extra`: Appends extra text about current pressure/temperature in output filename (e.g., _P1000_T5.tsv)
* `-t`, `--time`: Set integration time (in ms)

### Arduino set up ###

To enable LED control from Python, upload this sketch to your Arduino using the Arduino IDE:

```cpp
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
```

## Test modules ##
* `bathtest.py`
* `pumptest.py`
* `iscotest.py`

See octopode [spectackler](https://github.com/octopode/spectackler).




