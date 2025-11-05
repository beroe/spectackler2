#!/usr/bin/env python3
"""
Plot fluorescence spectra from .tsv files.

Usage:
  python plot_spectrum.py                     # plots all spectrum_*.tsv files in current directory
  python plot_spectrum.py -f spectrum_*.tsv   # plot a single spectrum file
  python plot_spectrum.py -d /path/to/dir     # plot spectra in a specific directory
  python plot_spectrum.py --gif output.gif    # export all spectra as a GIF
  python plot_spectrum.py --limit 400 500     # restrict x-axis wavelength range

File name format:
  spectrum_20251028T171525855534_idx3_P10_T15.0.tsv
    → timestamp: 20251028T171525855534
    → pressure: 10 psi
    → temperature: 15.0 °C
"""

import argparse
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
from pathlib import Path
import re

def compute_gp(df):
    """Compute Generalized Polarization (GP) and return GP plus 440/490 nm points."""
    wl_440 = (df["Wavelength"] - 440).abs().idxmin()
    wl_490 = (df["Wavelength"] - 490).abs().idxmin()
    I440, I490 = df.loc[wl_440, "Intensity"], df.loc[wl_490, "Intensity"]
    gp = (I440 - I490) / (I440 + I490)
    return gp, (440, I440), (490, I490)

def extract_metadata(filename):
    """Extract timestamp, pressure (psi), and temperature (°C) from filename."""
    name = Path(filename).stem
    m = re.search(r"spectrum_(\d{8}T\d+)_idx\d+_P(\d+)_T([0-9.]+)", name)
    if not m:
        return None, None, None
    timestamp, pressure, temp = m.groups()
    return timestamp, float(pressure), float(temp)

def plot_spectrum(ax, spectrum_path, limit=None, y_max=None):
    """Plot a single spectrum with GP, pressure, and temperature info."""
    data = pd.read_csv(spectrum_path, sep="\t")
    data.columns = [c.strip() for c in data.columns]
    data["Wavelength"] = pd.to_numeric(data["Wavelength"], errors="coerce")
    data["Intensity"] = pd.to_numeric(data["Intensity"], errors="coerce")
    data = data.dropna()

    gp, p440, p490 = compute_gp(data)
    ts, pressure, temp = extract_metadata(spectrum_path)

    if limit:
        data = data[(data["Wavelength"] >= limit[0]) & (data["Wavelength"] <= limit[1])]

    ax.clear()
    ax.plot(data["Wavelength"], data["Intensity"], color="blue", label="Spectrum")
    ax.scatter(
        [p440[0], p490[0]],
        [p440[1], p490[1]],
        color="red",
        s=40,
        label="440 nm & 490 nm",
        zorder=10
    )

    ax.set_xlabel("Wavelength (nm)")
    ax.set_ylabel("Intensity (a.u.)")
    if y_max:
        ax.set_ylim(0, y_max * 1.05)

    ax.set_title(
        f"{Path(spectrum_path).name}\n"
        f"P = {pressure if pressure is not None else 'N/A'} psi, "
        f"T = {temp if temp is not None else 'N/A'} °C, "
        f"GP = {gp:.3f}"
    )
    ax.legend()
    ax.grid(True)

def main():
    parser = argparse.ArgumentParser(description="Plot fluorescence spectra (.tsv).")
    parser.add_argument("-f", "--file", help="Path to a single spectrum_*.tsv file")
    parser.add_argument("-d", "--dir", help="Directory containing spectrum_*.tsv files")
    parser.add_argument("--limit", nargs=2, type=float, metavar=("MIN", "MAX"),
                        help="Restrict wavelength range (e.g., 400 500)")
    parser.add_argument("--gif", help="Export all spectra in the directory as a GIF file")
    args = parser.parse_args()

    # Default to current directory if none given
    if not args.file and not args.dir:
        args.dir = "."

    # === SINGLE FILE MODE ===
    if args.file:
        if not os.path.exists(args.file):
            print(f"❌ File not found: {args.file}")
            return
        fig, ax = plt.subplots(figsize=(8, 5))
        plot_spectrum(ax, args.file, limit=args.limit)
        plt.tight_layout()
        plt.show()
        return

    # === DIRECTORY MODE ===
    spectra_files = sorted(
        [os.path.join(args.dir, f) for f in os.listdir(args.dir)
         if f.startswith("spectrum_") and f.endswith(".tsv")]
    )
    if not spectra_files:
        print("❌ No spectrum_*.tsv files found in the directory.")
        return

    print(f"✅ Loaded {len(spectra_files)} spectra from {args.dir}")

    # Compute shared y-axis limit across all spectra
    y_max = 0
    for f in spectra_files:
        df = pd.read_csv(f, sep="\t")
        if "Intensity" in df.columns:
            df["Intensity"] = pd.to_numeric(df["Intensity"], errors="coerce")
            y_max = max(y_max, df["Intensity"].max())

    fig, ax = plt.subplots(figsize=(8, 5))

    # === GIF EXPORT MODE ===
    if args.gif:
        print(f"🎞️  Generating GIF: {args.gif} ...")
        import imageio.v2 as imageio
        frames = []
        for f in spectra_files:
            plot_spectrum(ax, f, limit=args.limit, y_max=y_max)
            fig.canvas.draw()
            buf = np.asarray(fig.canvas.buffer_rgba())
            frames.append(buf.copy())
        imageio.mimsave(args.gif, frames, fps=2)
        print(f"✅ GIF saved as {args.gif}")
        return

    # === INTERACTIVE SCROLL MODE ===
    print("➡️  Use → / ← to scroll through spectra. Close window to quit.")
    index = 0
    plot_spectrum(ax, spectra_files[index], limit=args.limit, y_max=y_max)
    plt.tight_layout()

    def on_key(event):
        nonlocal index
        if event.key == "right":
            index = (index + 1) % len(spectra_files)
        elif event.key == "left":
            index = (index - 1) % len(spectra_files)
        else:
            return
        plot_spectrum(ax, spectra_files[index], limit=args.limit, y_max=y_max)
        plt.draw()

    fig.canvas.mpl_connect("key_press_event", on_key)
    plt.show()

if __name__ == "__main__":
    main()
