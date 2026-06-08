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

### Minimum installation (standalone `spectro_and_led.py`) ###

To run only `spectro_and_led.py` (without the temperature/pressure control stack), you need:

1. **Python 3** and two Python packages:
   ```bash
   pip3 install pyserial seabreeze
   ```
2. **(Linux only) USB device permissions** for the `seabreeze` library. On Linux, install the udev rules so the spectrometer is accessible without root:
   ```bash
   seabreeze_os_setup
   ```
   This step is **not needed on macOS or Windows** — the bundled backend works without it.
3. **`spectro_and_led.py` from this repository.** This is the only file required for plain spectrum capture.
4. **(LED only)** If you use the `-l`/`--led` option:
   * Copy `spec_config.py` into the same directory and set `portARDU` to your Arduino's serial port. It is imported lazily, so it is only needed when the LED is enabled and the Arduino port cannot be auto-discovered.
   * Have an **Arduino** running the sketch in the [Arduino set up](#arduino-set-up) section below.
5. *(Optional)* `matplotlib` for the `-v`/`--visual` preview plot:
   ```bash
   pip3 install matplotlib
   ```

Then run it from within that directory, e.g. `./spectro_and_led.py -w`.

### Command line arguments ###
* `-b`, `--debug`: Prints extra diagnostic output
* `-w`, `--write`: Writes spectrum data to a `.tsv` file instead of in console. Data are saved as: `data/spectrum_YYYY_Mmm_DD+HH_MM_SS<extra>.tsv`
* `-i`, `--invert`: Inverts LED logic (default: 0=on, 1=off)
* `-e`,`--extra`: Appends extra text about current pressure/temperature in output filename (e.g., _P1000_T5.tsv)
* `-t`, `--time`: Set integration time (in ms)
* `-v`, `--visual`: Shows a preview plot of the spectra captured in this run (useful with `-r`/`--repeat`). Requires `matplotlib` (`pip3 install matplotlib`); it is imported only when this option is used, so basic spectrum capture has no extra dependency.

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

---

## R plotting and analysis scripts ##

These scripts process the `.tsv` spectral data collected by `cubette-mini.py` and compute **C-Laurdan Generalized Polarization (GP)** — a fluorescence metric for membrane lipid order — across pressure and temperature conditions. Run all scripts from the project root directory (e.g., `Rscript gp_cubette.R`).

### Directory organization ###

The scripts expect input data in `data/` and write all outputs to `figures/` and `results/`. The `figures/` and `results/` subdirectories are created automatically the first time each script runs; only the `data/` tree needs to be set up manually before running.

```
spectackler2/
├── data/
│   ├── raw/
│   │   ├── <sample_name>/               ← one folder per experiment
│   │   │   ├── blank_*.tsv              ← blank spectrum (any name starting with blank_)
│   │   │   ├── spectrum_<ts>_idx<N>_P<psi>_T<temp>.tsv
│   │   │   └── spectrum_<ts>_idx<N>_P<psi>_T<temp>.tsv  ...
│   │   └── <another_sample>/
│   │       └── ...
│   └── cubette_metadata.xlsx            ← sample metadata (see below)
│
├── results/                             ← created by gp_cubette.R
│   └── sg_band/                         ← one subfolder per GP_method
│       └── <sample_name>_GP_values_sg_band.csv
│
└── figures/                             ← created by the plotting scripts
    ├── gp_over_time/                    ← gp_cubette.R (plot_over_time)
    │   └── <sample_name>_GP_over_time_<method_tag>.pdf
    ├── gp_landscape/                    ← gp_cubette.R (plot_landscape)
    │   └── <sample_name>_lgp_landscape_<method_tag>.pdf / .gif
    ├── summary/                         ← gp_summary_figure.R
    │   └── <sample_name>_summary_sg_band.pdf
    ├── gp_compare/                      ← gp_species_compare.R
    │   ├── <prefix>_GPvsPress_allT.pdf
    │   ├── <prefix>_GPvsTemp_allP.pdf
    │   └── treatments/
    │       └── <species>_GPvsPress_allT.pdf  ...
    └── spectra/                         ← spectrum_golayfilt.R
        ├── <sample_name>_diagnostic.pdf
        ├── all_spectra/                 ← when plot_all_steps = TRUE
        └── ln_decomp/                  ← when show_ln_decomposition = TRUE
            └── all_spectra/
```

#### Spectrum file naming ####

`spectro_and_led.py` saves files with the `-e` flag in this format:

```
spectrum_YYYYMMDDTHHMMSS_idx<N>_P<psi>_T<temp>.tsv
```

* `YYYYMMDDTHHMMSS` — acquisition timestamp (e.g., `20260408T143022`)
* `idx<N>` — replicate index within a pressure/temperature step (1, 2, or 3)
* `P<psi>` — pressure in PSI as an integer (e.g., `P14504`); converted to bar by the scripts
* `T<temp>` — temperature in °C (e.g., `T5.0`)

Each `.tsv` has two columns: `Wavelength` (nm) and `Intensity`.

The blank spectrum must match the pattern `blank_*.tsv` and be placed in the same folder as the experiment spectra it corresponds to.

#### Script run order ####

`gp_species_compare.R` and `gp_summary_figure.R` both read the GP CSVs produced by `gp_cubette.R` and will error if those files do not exist. The recommended order is:

1. `spectrum_golayfilt.R` — optional diagnostic, can be run on raw data at any time
2. `gp_cubette.R` — computes GP and writes CSVs and first-pass plots
3. `gp_summary_figure.R` — per-experiment summary page (requires step 2)
4. `gp_species_compare.R` — cross-species comparison (requires step 2)

#### `cubette_metadata.xlsx` ####

Required by `gp_cubette.R` (landscape mode only), `gp_summary_figure.R`, and `gp_species_compare.R`. Each row describes one experiment folder. The expected columns are:

| Column | Description |
|---|---|
| `file_name` | Experiment folder name in `data/raw/` — must match exactly |
| `species` | Species name displayed on plots |
| `sample_no` | Sample identifier (e.g., `D1339bw9`) |
| `group` | Functional group (`lobate`, `beroid`, `cydippid`, etc.) |
| `color` | Hex color code for this sample's line/points (e.g., `#1b9e77`); leave blank to auto-assign |
| `treat` | Treatment applied: `none`, `Protease inhibitor`, or `ProK` |
| `depth` | Collection depth in meters (used to compute native pressure as `depth × 0.1006 + 1.013` bar) |
| `temp` | Collection temperature in °C |
| `depth_fig` | Depth label for figures; use a number, `surface`, or `MBA` (cultured) |
| `col_loc` | Collection location (text, for summary figure header) |
| `col_date` | Collection date |
| `exp_date` | Experiment date |
| `type` | Tissue type (e.g., `whole animal`, `tissue`) |
| `text` | Set to `italic` to italicise the species name in the summary figure header |

### `gp_cubette.R` — Main GP analysis ###

The primary analysis script. Reads raw spectrum files from `data/raw/<sample>/`, optionally subtracts a blank spectrum, computes GP for each measurement, and generates up to two plot types per sample:

* **GP over time** (`plot_over_time = TRUE`): Line plot of GP vs. elapsed experiment time, colour-coded by temperature and annotated with pressure at each step. Saves a PDF to `figures/gp_over_time/` and a CSV of per-spectrum GP values to `results/<GP_method>/`.
* **GP landscape** (`plot_landscape = TRUE`): Pressure/temperature contour heatmap of GP, interpolated using multilevel B-spline approximation (MBA). Optionally annotates the species' native collection depth and temperature with an asterisk. Supports a live-mode refresh during a running experiment and can export an animated GIF showing the landscape build up over time.

**GP calculation methods** (set via `GP_method`):
* `sg_band` *(default, recommended)*: Applies a Savitzky–Golay smoothing filter, then computes GP from the trapezoid-integrated band averages in the blue (420–460 nm) and green (470–510 nm) emission windows.
* `lognormal`: Fits two or three asymmetric log-normal peaks (Siano–Metzler) in wavenumber space and computes GP from the blue vs. green fitted peak area ratio. Higher noise per spectrum; not recommended for routine pipeline use.

**Batch processing**: Set `run_all_samples <- TRUE` to process every subfolder in `data/raw/`, or list specific folder names in `samples`.

---

### `gp_species_compare.R` — Cross-species GP comparison ###

Reads pre-computed GP CSVs produced by `gp_cubette.R` and generates comparison plots across multiple samples and species. Requires `gp_cubette.R` to have been run first.

Produces two plots per run:
* **GP vs. Pressure**: GP averaged (or filtered to a fixed temperature) across pressure steps.
* **GP vs. Temperature**: GP averaged (or filtered to a fixed pressure) across temperature steps.

Each line represents one sample, coloured by species. A species-coloured asterisk marks each animal's native collection pressure or temperature (from `data/cubette_metadata.xlsx`). Key options:
* `groups_include`: Restrict plots to one or more functional groups (e.g., `"lobate"`, `"beroid"`, `"cydippid"`), or set to `"all"` to generate both a combined plot and one plot per group.
* `include_treatments`: When `TRUE`, samples treated with protease inhibitor or ProK are included, with linetype distinguishing treatment from control (treated samples inherit their species' control colour).
* `avg_mode`: `"all"` averages GP across all temperatures (for the pressure plot) or all pressures (for the temperature plot); `"fixed"` filters to a single target temperature or pressure.

Outputs PDFs to `figures/gp_compare/`. Per-species treatment comparison plots are saved to `figures/gp_compare/treatments/`.

---

### `gp_summary_figure.R` — Per-experiment summary figure ###

Assembles a single portrait-format PDF page per experiment combining metadata, GP landscape, GP over time, and emission spectra. Requires `gp_cubette.R` to have been run first (reads pre-computed GP CSVs; does not recalculate GP).

Each page contains:
* **Header**: Species name, sample ID, experiment date, collection depth/temperature/location, tissue type, and GP calculation method.
* **Panel A**: P/T GP landscape heatmap (same MBA interpolation as `gp_cubette.R`), without a colour bar legend.
* **Panel B**: GP over time coloured by temperature, with pressure labels. Shared GP and temperature colour bar legends are placed between panels A/B and the right margin.
* **Panel C**: Start and end emission spectra side-by-side, showing raw and SG-smoothed curves with blue and green band regions shaded.

GP and intensity axis scales can each be set to `"global"` (computed from all experiments), `"auto"` (per sample), or fixed manual limits. Outputs PDFs to `figures/summary/`.

---

### `spectrum_golayfilt.R` — Spectrum diagnostic ###

A diagnostic tool for inspecting individual C-Laurdan emission spectra and comparing GP calculation methods. Unlike the pipeline scripts, this script plots each spectrum file individually without averaging across steps.

For each spectrum (or triplicate-averaged step), it:
* Loads and optionally blank-subtracts the raw `.tsv` file.
* Applies Savitzky–Golay smoothing.
* Computes and prints all 7 GP methods to the console: raw exact (440/490 nm), raw band mean, SG exact, SG auto-detected peaks, SG band mean, SG band integrated, and log-normal.
* Produces a PDF with the raw and SG-smoothed spectrum, shaded band regions, and all 6 non-LN GP values annotated in the caption (`show_method_comparison = TRUE`).
* Optionally produces a second PDF page showing the Siano–Metzler log-normal decomposition with fitted blue, green, and (if 3-peak) intermediate components overlaid (`show_ln_decomposition = TRUE`).

**Plot modes** (set `plot_all_steps`):
* `FALSE` *(default)*: Plots only the first and last spectrum of the experiment.
* `TRUE`: One page per individual spectrum file; output goes to `figures/spectra/all_spectra/`.

Outputs PDFs to `figures/spectra/`.




