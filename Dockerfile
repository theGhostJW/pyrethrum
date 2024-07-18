# Use the latest official Ubuntu image as a base
FROM theghostjw/haskell:haskell

# Set environment variables to avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

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
# CMD ["bash"]
ENTRYPOINT ["/bin/bash"]

# TODO : copy this https://github.com/vzarytovskii/haskell-dev-env/blob/master/.devcontainer/Dockerfile