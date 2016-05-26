#!/bin/bash
set -e
cabal build
exec ./dist/build/tman/tman

