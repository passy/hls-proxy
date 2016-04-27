{-# LANGUAGE TemplateHaskell #-}

module Lib.Files where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)

eventEmptyEndedM3U8File :: BS.ByteString
eventEmptyEndedM3U8File = $(embedFile "shared/m3u8/event_empty_ended.m3u8")
