#!/bin/bash

# needs execute permissions
# may need to run chmod +x DockerEverything.sh DockerBuildPushHaskell.sh DockerBuildPushPyrethrum.sh

# Run bs1.sh
./DockerBuildPushHaskell.sh && \
./DockerBuildPushPyrethrum.sh