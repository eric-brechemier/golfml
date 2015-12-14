#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking golfmlcoursewriterlinux64
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --build-id --dynamic-linker=/lib64/ld-linux-x86-64.so.2    -L. -o golfmlcoursewriterlinux64 link.res
if [ $? != 0 ]; then DoExitLink golfmlcoursewriterlinux64; fi
IFS=$OFS
