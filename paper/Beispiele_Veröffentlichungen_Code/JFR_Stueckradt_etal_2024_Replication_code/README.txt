Replication files for 
Stückradt, K., Jaspers, E., van Gaalen, R. & Machado, W.. "Parents’ hourly wages in 
female same-sex and different-sex couples: The role of partner’s gender and employers"


The underlying directory structure used in the CBS RA is shown below. 
Scripts are executed within the corresponding R project and, except when 
loading original microdata files, use relative paths. 


infra/
|-- infra.Rproj
|-- script/
    |-- parentswages_create_dataset.R
|-- data/
    |-- processed/
        |-- *.fst files


parentswages/
|-- parentswages.Rproj
|-- script/
    |-- 0_prep.R
    |-- 1_descriptives.R
    |-- 2_models.R
|-- data/
|-- output/