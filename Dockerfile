# Use the latest official Ubuntu image as a base
FROM theghostjw/haskell:haskell

# Set environment variables to avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Create a non-root user and switch to it
# RUN useradd -ms /bin/bash vscode
# USER vscode

# Set the working directory
WORKDIR /home/vscode

#!/bin/bash

# Clone the specific branch 'dev-container' from the repository
RUN git clone --branch dev-container https://github.com/theGhostJW/pyrethrum.git \
    && cd pyrethrum \
    && cabal build --only-dependencies \
    && cd .. \
    && rm -rf pyrethrum

# Copy any local files to the container (if needed)
# COPY . /home/dockeruser/

# Expose a port if the application needs it
# EXPOSE 8080

# Specify the default command to run when the container starts
# CMD ["bash"]
ENTRYPOINT ["/bin/bash"]