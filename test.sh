

# Copyright (c) 2019-present ben@benkb.org
# Licensed und the MIT License

USAGE=''
HELP='run all test in t/*.t against corresponding t/*.expect'


lang=$1
dir=$2

[ -n "$dir" ] || dir=t


die () { echo $@; exit 2; }

[ -n "$lang" ] || die "usage: lang"
[ -n "$dir" ] || die "Err: dir $dir not exists"

# setup
#for t in t/*.t; do
#  [ -f "$t" ] || continue
#   bt=$(basename $t)
#   bt_name=${bt%.*}
#   expect=$bt_name.${lang}-expect
#   [ -f "t/$expect" ] ||  die "Err: no expect file for test $expect"
#   rm -f t/$bt_name.*-out
#done

echo running tests:

noexpect=
for t in $(ls $dir/*.t | sort) ; do
  [ -f "$t" ] || continue
   bt=$(basename $t)
   bt_name=${bt%.*}
   expect="$dir/$bt_name.${lang}-expect"
   bypass="$dir/$bt_name.${lang}-bypass"
   if [ -f "$expect" ]; then
     echo testing $t
     test_out="$dir/$bt_name.${lang}-out"
     perl pac.pl -d $lang $t > "$test_out" 
     if diff -q "$expect" "$test_out" ; then
        printf  " $t "
     else
        diff  "$expect"  "$test_out"
        die "test $t failed"
     fi
   else
     noexpect=$t
   fi
done

echo ""
[ -z "$noexpect" ] || echo "Warn: not all tested such as $noexpect"

echo ""
echo test suite successfull for lang=$lang!
echo ':-)'
