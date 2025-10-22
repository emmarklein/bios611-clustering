#!/bin/bash

docker build . -t clustering

# --- Config ---
PASSWORD="emma"
PORT=8787

# --- Run container ---
docker run \
  -p $PORT:8787 \
  -e PASSWORD=$PASSWORD \
  -v $(pwd):/home/rstudio/work \
  -it clustering

echo "RStudio Server is running!"
echo "Open http://localhost:$PORT in your browser"
echo "Username: rstudio"
echo "Password: $PASSWORD"

