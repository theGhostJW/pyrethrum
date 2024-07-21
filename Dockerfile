# Use the latest official Ubuntu image as a base
FROM theghostjw/haskell:haskell

# Set environment variables to avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Create a non-root user and switch to it
# RUN useradd -ms /bin/bash vscode
# USER vscode

# Set the working directory
WORKDIR /home/vscode

ARG REPO=https://github.com/theGhostJW/pyrethrum.git
ARG BRANCH=dev-container

# sadly this hast to all be in a single block so REPO_DIR can be used
# Keep in mind that Docker does not directly support storing command output in variables within a Dockerfile as you might in a shell script. 
# The example combines commands using && to ensure they are executed in the same shell, 
# allowing the use of the variable ${REPO_DIR} within the scope of that RUN command.

# calculate local repo dir
RUN REPO_DIR=$(basename -s .git "${REPO}") \
    # clone into home (note single branch)
    && git clone --single-branch --branch ${BRANCH} ${REPO} \
    # cd to
    && cd "${REPO_DIR}" \
    # build it all (go have coffee for an hour or more)
    # libraries will be cached by cabal
    # local modules => dist-newstyle
    && cabal build \
    # local copy module build artifacts to home dir
    && cp -r ./dist-newstyle /home/vscode/dist-newstyle \
    # CD out
    && cd .. \
    # delete the repo
    && rm -rf "${REPO_DIR}"

# Expose a port if the application needs it
# EXPOSE 8080

# Specify the default command to run when the container starts
CMD ["bash"]