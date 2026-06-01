#!/bin/sh
stack clean
stack build --copy-bins --local-bin-path ./dist 