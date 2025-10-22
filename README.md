Hello! 

This directory includes all files necessary to reproduce all outputs for my bios611 clustering HW assignment. It contains:

(1) Dockerfile
(2) clustering.R
(3) clustering.Rmd
(4) clustering.html
(5) /figures
(6) start.sh
(7) Makefile
(8) And lastly this README.md of course..

First, run start.sh to build the Docker image and then run the Docker container. You can do this by chmod+x start.sh and then run ./start.sh

Then, connect to the local host http://localhost:8787 and log into the rstudio server. I have included the results already in this directory but you can use the R terminal to reproduce the entire analysis. 

First, make sure you are in the /home/rstudio/work directory. Then, to delete everything and start from scratch, type make clean in the R terminal. Then type, make in the terminal. Through make, all final figures and documents will be rendered.

Thank you for reading me! 

