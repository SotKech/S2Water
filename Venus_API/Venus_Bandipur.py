#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 12 15:29:44 2023
@author: merleth
editor: Sotirios Kechagias
"""

# Import packages
import pathlib
import json
import requests
import urllib3
import pandas as pd
from datetime import datetime


# %% Functions
def images_date(imgs_date_all):  # Count number of images
    # Creating a list of found images by date
    # Recherche des dates uniques
    list_dates_unique = set([datetime.strptime(
        img["startDate"], "%Y-%m-%dT%H:%M:%SZ").date() for img in imgs_date_all])
    # Creation of the list
    img_date = [[img for img in imgs_date_all if datetime.strptime(
        img["startDate"], "%Y-%m-%dT%H:%M:%SZ").date() == d] for d in list_dates_unique]
    print(f"Number of dates : {len(list_dates_unique)}", "|", f"Number of images : {len(imgs_date_all)}")
    return img_date


def get_token(config_file="config_theia.cfg"):  # Get verification token
    df = pd.read_csv(config_file, sep="=",
                     header=None, names=["Key", "Value"])
    df.Key = df.Key.str.strip()  # Remove spaces before and after text
    df.Value = df.Value.str.strip()
    token = requests.post("https://theia.cnes.fr/atdistrib/services/authenticate/",
                          data={"ident": df.Value[df.Key == "login_theia"],
                                "pass": df.Value[df.Key == "password_theia"]},
                          verify=False).text
    return token


def download_images(vns_date, raw_data):
    image_name = vns_date["productIdentifier"]
    path_zip = pathlib.Path(raw_data) / f"{image_name}.zip"
    if path_zip.exists():
        print(f"Image {image_name} - Exists locally")
    else:
        token = get_token()  # Define your get_token function
        headers = {'Authorization': f'Bearer {token}'}
        params = (('issuerId', 'theia'),)
        url = vns_date["services"]["download"]["url"]
        img = requests.get(url, headers=headers, params=params, verify=False, stream=True)
        print(url)
        if img.ok:
            with open(path_zip, 'wb') as fd:
                for chunk in img.iter_content(chunk_size=8192):
                    fd.write(chunk)
            print(f"Image {image_name} - Downloaded")
        else:
            print("Problem with request.")


# %% Create directories
working_dir = r"C:/Projects/Venus_download"
raw_data = pathlib.Path(working_dir).joinpath("raw_data")
if not (pathlib.Path(raw_data)).exists():
    pathlib.Path(raw_data).mkdir(parents=True, exist_ok=True)

# %% Search for images
URL = "https://theia.cnes.fr/atdistrib/resto2/api/collections/VENUSVM05/search.json"
param = {"processingLevel": "LEVEL2A",
         "startDate": "2023-01-01",
         "completionDate": "2023-08-01",
         "location": "BANDIPUR",
         "maxRecords": 500,
         "cloudCover": "[0,50]"}
req = requests.get(URL, params=param)
if req.ok:
    req_imgs = json.loads(req.content)
    feat_imgs = [features["properties"] for features in req_imgs["features"]]
    for image in feat_imgs:
        del image["keywords"]
        del image["license"]
        del image["links"]
else:
    print("Problem with request")

# %% Print the images availability
image_info_list = []
for vns_date in feat_imgs:
    image_info_list.append(vns_date)
image_df = pd.DataFrame(image_info_list)
print(image_df.head())
image_df.to_csv("./BANDIPUR.csv", sep=",")

# %% Download Images
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)  # Connection to the server
i = 0
for vns_date in feat_imgs:
    i += 1
    download_images(vns_date, raw_data)
    percentage = (i / (len(feat_imgs))) * 100
    print(f"\rProgress: {percentage:.2f}% \n", end='', flush=True)

