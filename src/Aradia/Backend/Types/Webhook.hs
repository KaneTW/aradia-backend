module Aradia.Backend.Types.Webhook where

import Control.Lens

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

import Aradia.Backend.Utils
import Aradia.Backend.Types

type Webhook = Object
