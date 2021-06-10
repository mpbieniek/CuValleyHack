FROM jupyter/datascience-notebook:r-3.6.3

RUN conda install -c conda-forge \
'r-vroom' \
'r-furrr' \
'r-here' \
'r-writexl' \
'r-readxl' \
'r-stringi' \
'r-v8' \
'r-tictoc' \
'r-svMisc' \
'r-r.utils' \
'r-rpart' \
'r-reshape' \
'r-gridExtra' \
'r-ggcorrplot' \
'r-egg' \
'r-data.table' \
'r-knitr' \
'r-uuid' \
'r-formatR' \
'r-selectr' \
'r-caTools' \
'r-pROC' \
'r-ROCR' \
'r-tsfeatures' \
'r-Hmisc' \
&& \
conda clean --all -f -y && \
fix-permissions $CONDA_DIR && \
fix-permissions /home/$NB_USER
