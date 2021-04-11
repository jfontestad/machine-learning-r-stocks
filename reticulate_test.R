library(reticulate)
Sys.setenv(RETICULATE_PYTHON = "C:/Users/sergio/Miniconda3/envs/tensorflor-r/python.exe")
#Sys.setenv(PYTHONIOENCODING = "utf-8")
#sessionInfo()
py_discover_config()
#Sys.getenv()


use_python("C:/Users/sergio/Miniconda3/envs/tensorflor-r/python.exe", required = TRUE)
use_condaenv("C:/Users/sergio/Miniconda3/envs/tensorflor-r", required = TRUE)


reticulate::py_config()
sys <- import("sys")
reticulate::py_run_string("print('python string')")



