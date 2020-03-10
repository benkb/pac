

USAGE='lang input'

lang=$1
input=$2

die () { echo $@; exit 2; }

usage () { die "usage: $USAGE" ; }

[ -n "$lang" ] || usage
[ -n "$input" ] || usage


write_expect () {
  local testfile=$1

  basedir=$(dirname $testfile)
  basefile=$(basename $testfile)
  name=${basefile%.*}

  echo --------test-------
  cat $testfile
  echo -----------------
  echo " "

  tmpfile=$(mktemp)
  echo --------expect for: $testfile -------
  perl pac.pl -l $lang $testfile | tee $tmpfile
  echo -----------------
  echo " "

  echo "Ok with the result? (n or no)"

  read answ

  case "$answ" in
    n|no)
      die "aborted"
      ;;
    *)
      cat $tmpfile > "$basedir/$name.${lang}-expect"
      echo written into  $basedir/$name.${lang}-expect
      ;;
  esac

}

if [ -f "$input" ] ; then
   write_expect $input 
elif [ -d "$input" ] ; then
  for t in $input/*.t; do
    bt=$(basename $t)
    btname=${bt%.*}
    [ -f "$input/$btname.${lang}-expect" ] || write_expect $t
  done
else
  die "Err: invalid file"
fi
