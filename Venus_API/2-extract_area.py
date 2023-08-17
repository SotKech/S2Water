#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 12 15:47:27 2023

@author: merleth
"""

# %% Import packages
import pathlib
import zipfile
import geopandas as gpd
from osgeo import gdal
import rasterio
import numpy as np

# %% Architecture of repository

rep = r"C:/Projects/Venus_download"
venus_zip = pathlib.Path(rep).joinpath("venus_zip")
venus_extrait = pathlib.Path(rep).joinpath("venus_extract")
venus_indices = pathlib.Path(rep).joinpath("venus_indices")

# %% Plots extent


def plots_extent(plots_path):
    plots = gpd.read_file(plots_path)
    minx = min(plots.bounds.minx)
    miny = min(plots.bounds.miny)
    maxx = max(plots.bounds.maxx)
    maxy = max(plots.bounds.maxy)
    return (minx, miny, maxx, maxy)

# %% Extract plots extent


path_bands = list(venus_zip.glob('*.zip'))
for path_zip_img in path_bands:
    plots_path = "C:/Projects/S2Water/Data/Berambadi_AOI.geojson"
    # Create a folder for the zip and shorten the name
    name_parts = pathlib.Path(path_zip_img).name.split("_")
    sat = "Venus"
    date = name_parts[1].split("-")[0]
    nom_extrait = f"{sat}_{date}"

    path_extrait = pathlib.Path(venus_extrait).joinpath(nom_extrait)
    pathlib.Path(path_extrait).mkdir(parents=True, exist_ok=True)

    # Extract all image names from zip file
    all_img = [f for f in zipfile.ZipFile(path_zip_img).namelist()
               if 'FRE' in f or 'MG2' in f]
    for f in all_img:
        band = f.split('_')[-1].split('.')[0]
        if band == "XS":
            band = 'MG2'
        basename = pathlib.Path(f).name
        output_path = pathlib.Path(path_extrait).joinpath(f'{sat}_{date}_{band}.tif')
        input_path = pathlib.Path(path_zip_img).joinpath(f)
        print(output_path)
        gdal.Warp(destNameOrDestDS=str(output_path),
                  srcDSOrSrcDSTab="/vsizip/"+str(input_path).replace("\\", "/"),
                  format='GTiff',
                  # cutlineDSName=r"/home/merleth/Documents/Stage_Cesbio/Venus_project/extent_plots/test.shp",
                  # cropToCutline=True,
                  outputBounds=plots_extent(plots_path),
                  dstNodata=-10000)


# %% Apply mask to all bands


def mask_band(path_band_list, venus_indices):
    bands_data = {}
    date = path_band_list[0].name.split("_")[1]
    for i, band in enumerate(path_band_list):
        # print(i)
        # print(band)
        band_name = pathlib.PurePath(band).name
        print(band_name)
        band_num = band_name.split("_")[-1].split(".")[0]
        # print(band_num)
        with rasterio.open(band) as src:
            if i == 0:
                meta = src.meta.copy()
            data = src.read(1)
            bands_data[band_num] = np.where(data == -10000, np.nan, data)
            # print(bands_data)
    mask = np.where(bands_data["MG2"] == 0, 0, np.nan)
    # print(mask)
    del bands_data["MG2"]
    for b in bands_data:
        bands_data[b] = bands_data[b] + mask
        meta.update({"dtype": np.float32,
                     "nodata": np.nan,
                     "compress": "lzw"})
        path_file = pathlib.Path(venus_indices).joinpath(b)
        path_file.mkdir(parents=True, exist_ok=True)
        path_band = pathlib.Path(venus_indices).joinpath(b, f'VNS_{date}_{b}.tif')
        with rasterio.open(path_band, "w", **meta) as dst:
            dst.write(bands_data[b]/1000, 1)


# %% Create all bands without clouds


list_dir = [x for x in pathlib.Path(venus_extrait).iterdir() if x.is_dir()]
for folder in list_dir:
    path_band_list = list(folder.glob('*.tif'))
    mask_band(path_band_list, venus_indices)
