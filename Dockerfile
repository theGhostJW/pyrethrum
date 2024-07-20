# Use the latest official Ubuntu image as a base
FROM theghostjw/haskell:haskell

# Set environment variables to avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Create a non-root user and switch to it
# RUN useradd -ms /bin/bash vscode
# USER vscode

# Set the working directory
WORKDIR /home/vscode

#  cabal build was failing
#  error: chmod on /workspaces/8217ff4d204bea625e2888e280e4f0a2eb33b8f77c89a67967029ca09aaa44db/dist-newstyle/src/chronos-e3374e440d991c5a/.git/config.lock failed: Operation not permitted
# RUN chmod -R 777 /workspaces/

# Copy any local files to the container (if needed)
# COPY . /home/dockeruser/

# Expose a port if the application needs it
# EXPOSE 8080

# Specify the default command to run when the container starts
# CMD ["bash"]
ENTRYPOINT ["/bin/bash"]