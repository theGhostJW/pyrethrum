# Use the latest official Ubuntu image as a base
FROM ubuntu:latest

# Set environment variables to avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Update the package list and install some basic utilities
RUN apt-get update && \
    apt-get install -y \
    build-essential \
    curl \
    wget \
    git \
    sudo \
    && apt-get clean

# Install Python 3 and pip
# RUN apt-get update && \
#     apt-get install -y \
#     python3 \
#     python3-pip \
#     && apt-get clean

# Create a non-root user and switch to it
RUN useradd -ms /bin/bash vscode
USER vscode

# Set the working directory
WORKDIR /home/vscode

# Copy any local files to the container (if needed)
# COPY . /home/dockeruser/

# Expose a port if the application needs it
# EXPOSE 8080

# Specify the default command to run when the container starts
CMD ["bash"]
