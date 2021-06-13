module TestArgs where

import Hedgehog

slowPropertyTest :: Property -> Property
slowPropertyTest = withTests 300 . withShrinks 80

fastPropertyTest :: Property -> Property
fastPropertyTest = withTests 2000 . withShrinks 100
