#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 12 15:51:20 2023

@author: merleth
"""

# %% Import packages
import pathlib
import pandas as pd
import geopandas as gpd
import xarray as xr
import rioxarray as rioxr
import spyndex
import numpy as np
from datetime import datetime
from dask.distributed import LocalCluster, Client
from rasterstats import zonal_stats
# %% Architecture of repository

rep = r"/home/merleth/Documents/Stage_Cesbio/Venus_project"
venus_zip = pathlib.Path(rep).joinpath("venus_zip")
venus_extrait = pathlib.Path(rep).joinpath("venus_extrait")
venus_indices = pathlib.Path(rep).joinpath("venus_indices")

# %% Création d'un client (même si ne sert pas pour le moment)
# cluster = LocalCluster(
#    n_workers=7,
#    threads_per_worker=2)
#
#client = Client(cluster)
#print(f"Le dashboard est accessible à l'url : {client.dashboard_link}")

# %% Create one dataset with all bands and index


def read_index():
    list_venus_extrait = list(venus_indices.glob("B[0-9]*"))
    dict_da = {}
    for path_index in list_venus_extrait:
        list_tif = list(path_index.glob('*.tif'))
        # print(list_tif)
        list_da = []
        for path in list_tif:
            da = rioxr.open_rasterio(path,
                                     parallel=True
                                     # chunks
                                     )
            time_agg = path.name.split("_")[1]
            time = f"{time_agg[0:4]}-{time_agg[4:6]}-{time_agg[6:8]}"
            dt = datetime.strptime(time, "%Y-%m-%d")
            dt = pd.to_datetime(dt)
            da = da.assign_coords(time=dt)
            da.expand_dims(dim="time")
            list_da.append(da)
            list_da1 = list_da
            print(path)
        da = xr.concat(list_da1, dim='time')
        da = da.sel(band=1)
        dict_da[path_index.name] = da
    return dict_da


dict_da = read_index()
ds = xr.Dataset(dict_da)


# %% Provide dictionnary with associate bands and calculate all indices from the list ''

bands_parameters = {"A": ds.B2,
                    "B": ds.B3,
                    "G": ds.B4,
                    "N": ds.B10,
                    "N2": ds.B11,
                    "R": ds.B7,
                    "RE1": ds.B8,
                    "RE2": ds.B9,
                    "RE3": ds.B10,
                    "WV": ds.B12,
                    "Y": ds.B5,
                    "L": 0.5
                    }

list_indices = ['NDVI', 'NDWI', 'SAVI']

for index in list_indices:
    da = spyndex.computeIndex(index, bands_parameters)
    time_list = da.coords['time'].values
    ds[index] = da
    for date in time_list:
        year = str(date).split("-")[0]
        month = str(date).split("-")[1]
        day = str(date).split("-")[2].split("T")[0]
        name = f'VNS_{year}{month}{day}_{index}.tif'
        print(name)
        name_path = pathlib.Path(venus_indices).joinpath(index, name)
        data = da.sel(time=date)
        pathlib.Path(name_path).parents[0].mkdir(parents=True, exist_ok=True)
        data.rio.to_raster(name_path)

# %% Other index to calculate not in spyndex


def BI(ds, bands_parameters):
    da = np.sqrt(((bands_parameters["R"]**2) + (bands_parameters["G"]**2))/2)
    ds["BI"] = da
    time_list = da.coords['time'].values
    for date in time_list:
        year = str(date).split("-")[0]
        month = str(date).split("-")[1]
        day = str(date).split("-")[2].split("T")[0]
        name = f'S2_{year}{month}{day}_BI.tif'
        print(name)
        name_path = pathlib.Path(venus_indices).joinpath("BI", name)
        data = da.sel(time=date)
        pathlib.Path(name_path).parents[0].mkdir(parents=True, exist_ok=True)
        data.rio.to_raster(name_path)


BI(ds, bands_parameters)

# %% Calculate statistics by index for each plot

stats = ["count", "min", "max", "mean", "median", "std"]
plots_limit_path = pathlib.Path(rep).joinpath('extent_plots', 'parcelles.shp')
plots_limit = gpd.read_file(plots_limit_path)
df_parc = pd.DataFrame(plots_limit)["Station_ID"]
venus_stats = pathlib.Path(rep).joinpath('venus_stats')
venus_stats.mkdir(parents=True, exist_ok=True)
vns_indices = list(venus_indices.glob("*"))
df_stats = []
path_out = venus_stats.joinpath('stats.csv')
for num, indices in enumerate(vns_indices):
    bands = list(indices.glob('*.tif'))
    nb_bands = len(bands)
    for i, band in enumerate(bands):
        band_list = band.name.split("_")
        date = band_list[1]
        date = datetime.strptime(date, "%Y%m%d").date()
        indice = band_list[-1].split(".")[0]
        # Calcul des statistiques zonales
        print(band)
        z_stats = zonal_stats(str(plots_limit_path),
                              str(band),
                              stats=stats,
                              all_touched=False)
        df_zonal = pd.DataFrame(z_stats)
        df_zonal.index = df_parc
        df_zonal["Indice"] = f"{indice}"
        df_zonal["Date"] = f"{date}"
        df_stats.append(df_zonal)
df_parc_stats = pd.concat(df_stats)
df_parc_stats.to_csv(path_out)

# %% For each plot create dataset
dict_ds = {}
plots_limit.set_index("ID")
for plots_values in plots_limit.iterrows():
    print(plots_values[1]["geometry"])
    ds_clipped = ds.rio.clip([plots_values[1]["geometry"]])
    dict_ds[plots_values[1]["ID"]] = ds_clipped


for plot_name, values in dict_ds.items():
    var_list = dict_ds[plot_name].keys()
    print(var_list)
    for name_var in var_list:
        time_list = values.coords['time'].values
        # print(time_list)
        for date in time_list:
            print(date)
            year = str(date).split("-")[0]
            month = str(date).split("-")[1]
            day = str(date).split("-")[2].split("T")[0]
            name = f'VNS_{plot_name}_{year}{month}{day}_{name_var}.tif'
            name_path = pathlib.Path(venus_indices).joinpath("parcelles", plot_name, name_var, name)

            data = dict_ds[plot_name].sel(time=date)[name_var]
            print(data)
            pathlib.Path(name_path).parents[0].mkdir(parents=True, exist_ok=True)
            data.rio.to_raster(name_path)
