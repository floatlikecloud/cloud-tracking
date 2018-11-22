# cloud-tracking
cloud tracking tool written in modern FORTRAN (for WRF output)<br/>
Cloud Tracking Toolbox which is written by floatlikecloud (Wei-Ting Hsiao)<br/>
The programs should be used, modified, and spread under the MIT License.
_Nov.22 2018_<br/>
<br/>
_Contact: floatlikecloud@gmail.com_
<br/>
### Main Programs
**cloud_labeling.f95**: <br/>
the main program that inputs the raw model output and gives you the cloud-label field <br/>
**timeHistory.f95**: <br/>
following the cloud labeling, the main program that performs the time tracking <br/>
### Time-Tracking Programs 
**isDeepConvSeed.f95**: <br/>
the program that grabs out the deep convection clouds in snapshots <br/>
**searchFamilyUnion.f95** <br/>
the program that search for the families of a list of seeds through the entire time domain <br/>
**searchSingleFamily.f95** <br/>
the program that search for the family of a single seed through the entire time domain <br/>
### Tools
**get_cloud_property.f95** <br/>
the program that obtain the properties of every single cloud <br/>
<br/>
<br/>
<br/>
**USE MAKEFILE to compile the programs.**<br/>
For inputs/outputs, usage, details, and references, please refer to the description in the programs.
