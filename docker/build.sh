#!/bin/bash
set -e

# Build layered Docker images for study-wrangler

echo "Building base image with study.wrangler dependencies..."
docker build -f docker/Dockerfile.base -t veupathdb/study-wrangler:base .

echo "Building RStudio development image..."
docker build -f docker/Dockerfile.rstudio -t veupathdb/study-wrangler:rstudio .

echo "Building RServe service image..." 
docker build -f docker/Dockerfile.rserve -t veupathdb/study-wrangler:rserve .

echo "All images built successfully!"
echo "- veupathdb/study-wrangler:base    (shared dependencies)"
echo "- veupathdb/study-wrangler:rstudio (development environment)"
echo "- veupathdb/study-wrangler:rserve  (MCP server backend)"
