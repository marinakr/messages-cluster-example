#!/bin/sh
cd `dirname $0`
exec erl -sname node1@myhost -setcookie testappcookie  -pa $PWD/ebin $PWD/deps/*/ebin -config message  -s message #-boot start_sasl 

