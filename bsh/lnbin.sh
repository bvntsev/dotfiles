#!/bin/bash

PWD=$(pwd)

name=$1

ln -s "$PWD/$name" "/usr/local/bin/$name"
