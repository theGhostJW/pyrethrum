#  need to be logged into theghostjw account on docker hub
docker pull theghostjw/haskell:latest
docker build -t "pyrethrum" .
docker tag pyrethrum theghostjw/pyrethrum:latest
docker push theghostjw/pyrethrum:latest