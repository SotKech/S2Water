"""
            .o88o.   .oPYo.   8      8           8               
            8            `8   8      8           8               
            `Yooo.      oP'   8      8  .oPYo.  o8P .o88o.  oPYo.
                `8   .oP'     8  db  8  .oooo8   8  8oooo8  8  `'
                 8   8'       `b.PY.d'  8    8   8  8       8
            `Y88P'   8ooooo    `8  8'   `YooP8   8  `8ooo'  8
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Created on Wed Apr 12 15:29:44 2023
@author: merleth
"""

# Import packages
import pathlib
import json
import requests
import urllib3
import pandas as pd
from datetime import datetime

# Architecture of repository
rep = r"C:/Projects/Venus_download"
venus_zip = pathlib.Path(rep).joinpath("venus_zip")
venus_extract = pathlib.Path(rep).joinpath("venus_extract")
# Create repository for venus pictures
pathlib.Path(venus_zip).mkdir(parents=True, exist_ok=True)
pathlib.Path(venus_extract).mkdir(parents=True, exist_ok=True)

# Define parameters for the research
param = {
    "processingLevel": "LEVEL2A",
    "startDate": "2023-01-01",
    "completionDate": "2023-08-01",
    "location": "BANDIPUR",
    "maxRecords": 500,
    "cloudCover": "[0,20]"
}

# Search for images
URL = "https://theia.cnes.fr/atdistrib/resto2/api/collections/VENUSVM05/search.json"
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

# Count number of images
def images_date(imgs_date_all):
    #Creating a list of found images by date
    # Recherche des dates uniques
    list_dates_unique = set([datetime.strptime(img["startDate"], "%Y-%m-%dT%H:%M:%SZ").date() for img in imgs_date_all])
    # Création de la liste
    img_date = [[img for img in imgs_date_all if datetime.strptime(
        img["startDate"], "%Y-%m-%dT%H:%M:%SZ").date() == d] for d in list_dates_unique]
    print(f"Number of dates : {len(list_dates_unique)}")
    print(f"Number of images : {len(imgs_date_all)}")
    return img_date


a = images_date(feat_imgs)

# Connection to the server

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)


def get_token(config_file="config_theia.cfg"):
    df = pd.read_csv(config_file, sep="=",
                     header=None, names=["Key", "Value"])
    df.Key = df.Key.str.strip()  # Remove spaces before and after text
    df.Value = df.Value.str.strip()
    token = requests.post("https://theia.cnes.fr/atdistrib/services/authenticate/",
                          data={"ident": df.Value[df.Key == "login_theia"],
                                "pass": df.Value[df.Key == "password_theia"]},
                          verify=False).text
    return token


# Download Images

for vns_date in feat_imgs:
    picture_name = vns_date["productIdentifier"]
    path_zip = pathlib.PurePath(venus_zip).joinpath(f"{picture_name}.zip")
    if not pathlib.Path(path_zip).exists():
        # Connection to the server
        token = get_token()
        headers = {'Authorization': 'Bearer %s' % token, }
        params = (('issuerId', 'theia'),)
        url = vns_date["services"]["download"]["url"]
        print(url)
        img = requests.get(url,
                           headers=headers,
                           params=params,
                           verify=False,
                           stream=True)
        if img.ok:
            with open(path_zip, 'wb') as fd:  # wb for writing and binary
                for chunk in img:
                    fd.write(chunk)
        else:
            print("Problem with request.\n")
    else:
        print(f"Image {picture_name} Already downloaded\n")
