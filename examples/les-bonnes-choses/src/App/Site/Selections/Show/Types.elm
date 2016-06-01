module App.Site.Selections.Show.Types exposing (..)

import App.Documents.Types as Documents
import Prismic.Types as P


type alias Model =
    { selection : Result P.PrismicError (Maybe Documents.Selection)
    , products : Result P.PrismicError (List Documents.Product)
    }


type Msg
    = SetSelection (Result P.PrismicError ( P.Response Documents.Selection, P.Cache ))
    | SetProducts (Result P.PrismicError ( P.Response Documents.Product, P.Cache ))
