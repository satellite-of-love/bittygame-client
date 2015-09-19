think() {
 game=$1
 curl -XGET localhost:8080/game/$game/think
}

begin() {
  game=$1
  echo "begin $game"
  curl localhost:8080/scenario/$game/begin
}

random() {
  curl localhost:8080/random | jq .created | sed 's/"//g'
}
