module TestArgs where

import Hedgehog

slowPropertyTest :: Property -> Property
slowPropertyTest = withTests 2 . withShrinks 5

fastPropertyTest :: Property -> Property
fastPropertyTest = withTests 100 . withShrinks 10

