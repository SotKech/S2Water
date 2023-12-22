<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/SotKech/S2Water/">
    <img src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fwww.pngplay.com%2Fwp-content%2Fuploads%2F1%2FLetter-S-PNG-Photo.png&f=1&nofb=1&ipt=676c17b6eed1fbff95ce152952826d3574d8909f253b9424883625d7b3bff3c3&ipo=images" width="80" height="80">
  </a>
</div>

<!-- ABOUT THE PROJECT -->
## About The Project
The S2Water repository was created for my academic internship titled "Monitoring Water Surface of Agricultural Reservoirs in Semi-arid Regions Using Multi-spectral Remote Sensing."
<br/>
<br/>
The scripts in this repository focus on downloading Sentinel-2 images and calculating water detection spectral indices for semi-arid agricultural small reservoirs in Tunisia and India. After the calculation, temporal maps and graphs are generated, followed by a comparison with ground data.

<!-- BUILT WITH -->
## Built With
The download of Remote Sensing images was achieved using the [Sen2R](https://github.com/ranghetti/sen2r/tree/main) API and the [theia_download](https://github.com/olivierhagolle/theia_download) API.* Although images were downloaded for the Venμs satellite using [theia_download](https://github.com/olivierhagolle/theia_download) API, Venμs images were not used in the subsequent scripts.
<br/>
\* It is necessary to mention that [Sen2R](https://github.com/ranghetti/sen2r/tree/main) will not be usable after 2023.
<br/>
<br/>
The version of R used was: 4.3.1

## Prerequisites
To download images using the Sen2R API from `Sen2r_API/sen2r_API_Lebna.R`, you need to download [Sen2Cor](https://step.esa.int/main/snap-supported-plugins/sen2cor/). More instructions can be found in the [Sen2R](https://github.com/ranghetti/sen2r/tree/main) GitHub repository.

<!-- USAGE -->
## Usage
- `Sen2r_API/sen2r_API_Lebna.R`: This API takes input parameters and downloads Sentinel-2 Images, then converts them to a cropped Area Of Interest (AOI) TIF with only the usable bands.
- `01IndexCalculation.R`: Retrieves `Sen2r_API/sen2r_API_Lebna.R` TIF and creates TIF images of spectral indices in ./Output/.
- `02IndexPloting.R`: Retrieves the TIF images of spectral indices and generates PNG plots (maps) of all indices in ./Output/Plots/.
- `03IndexDataframe.R`: Retrieves the TIF products of `01IndexCalculation.R` and creates dataframes (CSV) files of each index for all the reservoirs - WARNING: These dataframes contain dates with cloud contamination.
- `04DataframesPlotting.R`: Retrieves dataframes (CSV) files from `03IndexDataframe.R` and plots of the indices estimation WITHOUT clouds contamination dates in ./Output/Graphs/. Optional output: Plots of the indices estimation WITHOUT clouds WITH FLAGS ~ Either L1C images or manually downloaded images with cloud over tile <10% (Needs ./Indices/Flags.csv as an input).
- `05InsituComparison.R`: Retrieves dataframes (CSV) files from `03IndexDataframe.R` + the ground data and (1) plots the indices estimation and ground data (in-situ) for either Lebna or Kamech Reservoirs in ./Output/Graphs/ and (2) CSV file of statistical comparison (RMSE, R2, BIAS) between ground data and indices in ./Output/.
- `LOESS_plot.R`: Retrieves dataframes (CSV) files from `03IndexDataframe.R` and plots LOESS water estimation for all reservoirs in ./Output/Graphs/.
- `Other_Scripts` contains scripts that either were not used or are incomplete.

<!-- LICENSE -->
## License
Distributed under the [CC BY-NC-SA 4.0 License](https://creativecommons.org/licenses/by-nc-sa/4.0/). See `License.md` for more information.

<!-- CONTACT -->
## Contact
- Sotirios Kechagias: sot.kechagias@gmail.com
- Project Link: [https://github.com/SotKech/S2Water/](https://github.com/SotKech/S2Water/)
<!-- ACKNOWLEDGMENTS -->
## Acknowledgments
<a href="https://en.ird.fr/"> 
<img src= "https://external-content.duckduckgo.com/iu/?u=http%3A%2F%2Fwww.afd.fr%2Fsites%2Fafd%2Ffiles%2F2017-08%2Flogo-ird_0.png&f=1&nofb=1&ipt=c58896d88b6913518b45959756999d505717ac064b754b2426b675df2b4de74b&ipo=images" width="350" height=90">
  
<a href="https://www.umr-lisah.fr/?q=en">
<img src= "https://pbs.twimg.com/profile_images/1180051796653416448/xYIKFZzt_400x400.png" width="100" height=100">

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[product-screenshot]: images/screenshot.png
[Sen2r.js]: https://luigi.ranghetti.info/img/sen2r_logo_200px.png
[Sen2r-url]: https://github.com/ranghetti/sen2r/tree/main
