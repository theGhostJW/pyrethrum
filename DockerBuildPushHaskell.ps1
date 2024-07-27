#  need to be logged into theghostjw account on docker hub
docker pull ubuntu:latest
docker build -t "haskell" -f DockerfileHaskell .
docker tag haskell theghostjw/haskell:latest
docker push theghostjw/haskell:latest