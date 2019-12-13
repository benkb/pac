lang=$1

[ -n "$lang" ] || { echo usage lang; exit 2; }

for t in t/*.t; do

  sh write-expect.sh $lang  $t
done
