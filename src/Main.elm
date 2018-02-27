import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Round exposing (..)
import WebSocket
import Json.Decode exposing (Decoder, int, string, list, decodeString)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encoder

main =
    Html.program
        {   init = init,
            view = view,
            update = update,
            subscriptions = gdaxListen
        }

-- MODEL

type alias Model = {
    responses: List TickerResp,
    subMessage: SubscribeResp
}

init: (Model, Cmd Msg)
init =
    (Model [] (SubscribeResp "" [Channel "" []]), Cmd.none)

-- UPDATE

type Msg = Subscribe | Unsubscribe | Update String

update: Msg -> Model -> (Model, Cmd Msg)
update msg {responses, subMessage} =
    case msg of
    Subscribe ->
        (Model [] subMessage, WebSocket.send url genSubRequest)
    Unsubscribe ->
        (Model [] subMessage, WebSocket.send url genUnsubRequest)
    Update updateResponse->
        let ticker = (getResp updateResponse) in
            let _ = Debug.log "JSON Recieved: " updateResponse in
            if String.contains "subscriptions" updateResponse || (ticker.msg_type == "") then
                let response = (getSub updateResponse) in
                    (Model responses response, Cmd.none)
            else
                (Model (List.append responses [ticker]) subMessage, Cmd.none)

gdaxListen: Model -> Sub Msg
gdaxListen _ =
    WebSocket.listen url Update

-- VIEW

view : Model -> Html Msg
view model =
    div [body] [
        div [controlStyle] [printData model],
        div [buttonDiv] [
            div [buttonContainer] [subButton], 
            div [buttonContainer] [unsubButton]
        ],
        div [authorStyle] [
            div [] [ text "Created by Bryan Martin" ],
            div [] [ text "For CSI-380-52" ]
        ]
    ]


printData : Model -> Html Msg
printData {responses, subMessage} =
    let average = averagePrice (priceList responses) in
        let currentTicker = latestTicker responses in
            div [infoPanel]
                [ div [titleDiv] [text ("GDAX Bitcoin Ticker")]
                , if List.length responses == 0
                then div [dataDiv] [text "Click Subscribe To See Live Pricing"]
                else div [dataDiv] [text ("1 BTC = $" ++ (Round.round 2 average))]
                , div [] [text ("Last Updated: " ++ (formatTime(latestTicker responses).time))]
            ]

unsubButton: Html Msg
unsubButton =
    button [buttonStyle, onClick Unsubscribe] [text "Unsubscribe"]

subButton: Html Msg
subButton =
    button [buttonStyle, onClick Subscribe] [text "Subscribe"]

-- Types --

type alias TickerResp = {
    msg_type: String,
    sequence: Int,
    product_id: String,
    price: String,
    open_24h: String,
    volume_24h: String,
    low_24h: String,
    high_24h: String,
    volume_30d: String,
    best_bid: String,
    best_ask: String,
    time: String
}

type alias Channel = {
    name: String,
    product_ids: List String
}

type alias SubscribeResp = {
    msg_type: String,
    channels: List Channel
}

type alias SubscribeReq = {
    msg_type: String,
    product_ids: List String,
    channels: List Channel
}

-- Static --
url: String
url = 
    "wss://ws-feed.gdax.com"

subRequest: SubscribeReq
subRequest = 
    SubscribeReq "subscribe" ["BTC-USD"] [Channel "ticker" ["BTC-USD"]]

unsubRequest: SubscribeReq
unsubRequest = 
    SubscribeReq "unsubscribe" ["BTC-USD"] [Channel "ticker" ["BTC-USD"]]


---------------------
-- Json Management --
---------------------
-- Encoding --
channelEncoder: Channel -> Encoder.Value
channelEncoder input =
    Encoder.object [
        ("name", Encoder.string input.name),
        ("product_ids", Encoder.list (List.map Encoder.string input.product_ids))
    ]

subEncoder: SubscribeReq -> Encoder.Value
subEncoder input = 
    Encoder.object [
        ("type", Encoder.string input.msg_type),
        ("product_ids", Encoder.list (List.map Encoder.string input.product_ids)),
        ("channels", Encoder.list (List.map channelEncoder input.channels))
    ]

genSubRequest: String
genSubRequest =
    Encoder.encode 0 (subEncoder subRequest)

genUnsubRequest: String
genUnsubRequest =
    Encoder.encode 0 (subEncoder unsubRequest)

-- Decoding --
channelDecoder: Decoder Channel
channelDecoder =
    decode Channel
        |> required "name" string
        |> required "product_ids" (list string)

unsubDecoder: Decoder SubscribeResp
unsubDecoder =
    decode SubscribeResp
        |> required "type" string
        |> required "channels" (list channelDecoder)

respDecoder: Decoder TickerResp
respDecoder =
    decode TickerResp
        |> required "type" string
        |> required "sequence" int
        |> required "product_id" string
        |> required "price" string
        |> required "open_24h" string
        |> required "volume_24h" string
        |> required "low_24h" string
        |> required "high_24h" string
        |> required "volume_30d" string
        |> required "best_bid" string
        |> required "best_ask" string
        |> required "time" string

getResp: String -> TickerResp
getResp data =
    case (decodeString respDecoder data) of
        Ok output ->
            output
        Err msg ->
            TickerResp "" 0 "" "" "" "" "" "" "" "" "" ""

getSub: String -> SubscribeResp
getSub data =
    let _ = Debug.log "GOT - " data in
    case (decodeString unsubDecoder data) of
        Ok output ->
            output
        Err msg ->
            SubscribeResp "" [Channel "" [""]]

-- Price Calculations --
extractPrice: TickerResp -> Float
extractPrice response =
    case (String.toFloat response.price) of
        Ok output ->
            output
        Err msg ->
            0

priceList: List TickerResp -> List Float
priceList responseList =
    List.map extractPrice responseList

averagePrice: List Float -> Float
averagePrice values =
    if (List.length values) > 5 then
        (List.sum (List.drop ((List.length values)-5) values)) / 5
    else
        (List.sum values) / toFloat (List.length values)

-- Get last ticker in the list --
latestTicker: List TickerResp -> TickerResp
latestTicker tickerList =
     case List.head (List.drop ((List.length tickerList)-1) tickerList) of
        Just ticker ->
            ticker
        Nothing ->
            TickerResp "" 0 "" "" "" "" "" "" "" "" "" ""

formatTime: String -> String
formatTime input =
    if input /= "" then
        (String.slice 11 19 input) ++ " " ++ (String.slice 5 7 input) ++ "/" ++ (String.slice 8 10 input) ++ "/" ++ (String.slice 0 4 input)
    else
        "Never"

-- STYLES --
-- Defines the styles that are utilized within the webpage
body =
  style
    [ ("width", "100%")
    , ("padding", "auto")
    , ("font-size", "1em")
    , ("text-align", "center")
    , ("background-color", "gray")
    , ("height", "100%")
   ]

controlStyle =
    style
    [ ("padding", "20px")
    ]

buttonDiv =
    style
    [ ("width", "100%")
    , ("text-align", "center")
    ]

buttonContainer =
    style
    [ ("padding", "10px")
    , ("display", "inline-block")
    ]

buttonStyle =
    style
    [ ("width", "250px")
    , ("padding", "15px")
    , ("border-radius", "10px")
    , ("font-size", "1.5em")
    , ("text-align", "center")
    ]

infoPanel = 
    style
    [ ("padding", "25px")
    , ("background-color", "lightgreen")
    , ("border-radius", "5px")
    ]

titleDiv =
    style
    [ ("border-style", "outset")
    , ("border-radius", "20px")
    , ("border-color", "green")
    , ("background-color", "green")
    , ("padding", "15px")
    , ("font-size", "3em")
    ]

dataDiv =
    style
    [ ("padding", "40px")
    , ("font-size", "2em")
    ]

authorStyle =
    style
    [ ("padding", "15px")
    ]