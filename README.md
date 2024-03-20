**Note on the files and folders**:

- You can access the RMarkdown file used to generate the Supplementary Materials: check SupplementaryMaterials.Rmd file.
- All figures from the Supplementary Materials are available in the Figures folder. The figures present in the paper were modified using Inkscape; you can find them in the "Inkscape" folder.
- cached-results folder contains some analysis that took some time to generate, and help save time when running the SupplemetaryMaterials.Rmd file
- Netlogo folder contains the Netlogo file used to generate the networks as well as an appendix to explain its functionalities
- InputFiles contains the networks (raw and transformed)

**For replication:**

- 1. Open the Netlogo file: `NetworkStructure_Variation.nlogo`, available in the Netlogo folder, in Netlogo. An appendix is available in this same folder to understand the program. If you only want to replicate, you can directly go to BehaviorSpace where the parameters used for generating the networks are already saved.
- 2. Store the obtained file in a "Raw" folder in the Results. Open the `CleanData.Rmd` file to clean these outputs. 
- 3. Run the analysis in `SupplementaryMaterials.Rmd`

For any questions please contact Mathilde Josserand at mathilde.josserand@gmail.com
