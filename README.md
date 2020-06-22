# BEMTOOL2.5

1. Download the BEMTOOL 2.5 folder;
2. Copy on your C disk the BMT_INPUT folder and create an empty folder named BMT_OUTPUT;
3. Open R 3.0 and the script BEMTOOL.r;
4. Set the directory where BEMTOOL.r script is located as working directory;
5. Install the package ALADYMTools from the zip file in ALADYMtools library and run the script RUNme.r to install or load all the needed libraries;
6. Run the script BEMTOOL.r;
7. Click on Load case study definition and select the file bmtconfigSimulation_GSA10_DPS_HKE_MUT.csv, lcoated in the foldet BMT_INPUT;
8. Click three times on NEXT button, verifying that all the tabs are filled in and, then click on RUN NEW Diagnosis. You need to run the new diagnosis only the first time that you rn the hindcasting phase. Next time, you can load existing diagnosis from the button in the GUI and go directly to the forecast.
9. After clicking in RUN NEW DIAGNOSIS, the ALADYM (biological/pressure component of BEMTOOL) GUI for the first stock will appear;
10.Load all input parameters, clicking on LOAD SIMULATION PARAMETERS and select the file aladym_DPS_GSA10_Zmode.csv in BMT_INPUT folder, sub-folder aladym (these are the input needed to run the hindcasting for P. longirostris) and then RUN SIMULATION;
11. Then, the ALADYM GUi for M. barbatus will appear. Click on LOAD SIMULATION PARAMETERS and select the file aladym_MUT_GSA10_Zmode.csv, then RUN SIMULATION;
12. Same for M. merluccius (aladym_HKE_GSA10_Zmode.csv).
13.For the three species, the first time that you run the hindcasting phase, the reference point are estimated (it needs more time). 
14. When M. merluccius will be completed, the bEMTOOL tab of diagnosis will appear and you can consult the graphs of times series by stock and fleet segment, selecting the different tabs.
15. Clicking on NEXT you can go on with forecast.
16. LOAD Scenario configuration and select the forecast parameterization for the economic sub-models bmtconfigForecast_GSA10_DPS_HKE_MUT.csv. The status quo scenario is set by default in this example file, after loading it, you can modify the management measures from the BEMTOOL GUI.
17. When you finished, click on RUN NEW SCENARIO. 
18.For each species, ALADYM GUI will appear, with the possibility to include some infos to be included in the projections (e.g. uncertainty, selectivity, etc... see manual for details);
19. Before running the second species, close the window FORECAST in progress... and so on when will appear the third species;
18. When the third species will be completed, the BEMTOOL GUI will appear again to show the results of the projections. 
