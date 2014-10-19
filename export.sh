#!/bin/bash
zip -r bc.zip * -x "*.git*" "*dist*" "*.DS_Store" "*.cabal-*" "cabal.*"
