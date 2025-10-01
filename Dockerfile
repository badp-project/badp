FROM rocker/r-ver:4.5.1

# Install system libraries needed for R package compilation
RUN apt-get update && apt-get install -y \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libglpk-dev \
    cmake \
    texlive-full \
    && rm -rf /var/lib/apt/lists/*

# Install R package 'renv'
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

# Set working directory
WORKDIR /package

# Set a writable directory for renv library
ENV RENV_PATHS_LIBRARY=/package/renv/library

# Copy package files
COPY . /package

# Restore packages from renv.lock
RUN Rscript -e "renv::restore(confirm = FALSE)"

# Install additional R packages required for the package check
RUN R -e "install.packages(c('rmarkdown', 'spelling', 'testthat', 'RcppArmadillo'), repos = 'https://cloud.r-project.org')"

# Build the R package
RUN R CMD build --compact-vignettes=both . && \
    mkdir -p /output && \
    mv *.tar.gz /output

# Open bash by default
CMD ["bash"]
