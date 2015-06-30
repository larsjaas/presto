
if test x"$SHLVL" = x1 || test x"$1" == x"--from-script"; then
    if test -d src/main && test -d src/test; then
        ROOTDIR=`pwd`
        export CHIBI_MODULE_PATH=$ROOTDIR/src/main:$CHIBI_MODULE_PATH
    else
        echo "Change to the root presto directory when sourcing setup.sh"
    fi
else
    echo "You need to source setup.sh, not launch it."
fi

