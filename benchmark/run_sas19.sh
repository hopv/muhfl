#!/bin/bash
/opt/home2/git/fptprove_muarith/_build/default/bin/muapprox_main.exe --algorithm rec-limit --format hes -e nu --verbose --kill-processes "$1" > /tmp/stdout_1.txt 2> /tmp/stderr_1.txt
