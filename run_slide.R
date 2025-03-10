library(SLIDE)
library(SeuratDisk)

args <- commandArgs(trailingOnly = TRUE)
yaml_path <- args[1]

input_params <- yaml::yaml.load_file(yaml_path)
SLIDE::checkDataParams(input_params)
SLIDE::SLIDEcv(yaml_path, nrep = 20, k = 5)
SLIDE::plotCorrelationNetworks(input_params)

