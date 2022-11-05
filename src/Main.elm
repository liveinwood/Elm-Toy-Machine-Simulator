port module Main exposing (..)

import Array exposing (..)
import Browser exposing (..)
import Cmd.Extra exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Result exposing (Result)
import String exposing (toInt)
import Tuple exposing (first)


type alias Label =
    String


type alias LabelTable =
    Dict Label Int


type alias Memory =
    Array ( Label, Entry )


type Entry
    = Code Opecode Operand
    | Data Int


type Opecode
    = Stop
    | Get
    | Print
    | Load
    | Store
    | Add
    | Sub
    | Goto
    | Ifzero
    | Ifpos


type Operand
    = NoArg
    | Immed Int
    | Label Label


type alias Model =
    { pc : Int
    , accumulator : Int
    , memory : Memory
    , output : List String
    , stopped : Bool
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pc = 0
      , accumulator = 0
      , memory = initMemory
      , output = []
      , stopped = False
      }
    , Cmd.none
    )



-- メモリの初期化
-- 3回入力を受け取ってその合計値を出力するプログラム


initMemory : Memory
initMemory =
    [ ( "", Code Load (Label "CNT") )
    , ( "LOOP", Code Ifzero (Label "BREAK") )
    , ( "", Code Get NoArg )
    , ( "", Code Add (Label "SUM") )
    , ( "", Code Store (Label "SUM") )
    , ( "", Code Load (Label "CNT") )
    , ( "", Code Sub (Immed 1) )
    , ( "", Code Store (Label "CNT") )
    , ( "", Code Goto (Label "LOOP") )
    , ( "BREAK", Code Load (Label "SUM") )
    , ( "", Code Print NoArg )
    , ( "", Code Stop NoArg )
    , ( "CNT", Data 3 )
    , ( "SUM", Data 0 )
    ]
        |> Array.fromList



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- UPDATE


type Msg
    = Inst ( Label, Entry )
    | Recv String
    | Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Inst ( _, Code opecode operand ) ->
            case exec opecode operand model of
                Ok ( m, cmd ) ->
                    ( m, cmd )

                Err errMsg ->
                    ( { model | stopped = True, output = model.output ++ [ errMsg ] }, Cmd.none )

        Inst ( _, Data _ ) ->
            ( { model | stopped = True, output = model.output ++ [ "invalid instruction" ] }, Cmd.none )

        Recv s ->
            case toInt s of
                Just n ->
                    ( model |> setAccum n |> incPc, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Step ->
            case fetchInst model of
                Ok e ->
                    if model.stopped then
                        ( model, Cmd.none )

                    else
                        ( model, perform (Inst e) )

                Err errMsg ->
                    ( { model | stopped = True, output = model.output ++ [ errMsg ] }, Cmd.none )



-- 現在のプログラムカウンタ位置のメモリエントリを取得する


fetchInst : Model -> Result String ( Label, Entry )
fetchInst m =
    case Array.get m.pc m.memory of
        Just e ->
            Ok e

        Nothing ->
            Err "segmentation fault"


exec : Opecode -> Operand -> Model -> Result String ( Model, Cmd Msg )
exec opecode operand model =
    case ( opecode, operand ) of
        ( Stop, _ ) ->
            stop model |> Result.andThen (\m -> Ok ( m, Cmd.none ))

        ( Get, _ ) ->
            Ok ( model, sendMessage "input number" )

        ( Print, _ ) ->
            print (toString model.accumulator) model |> Result.andThen (\m -> Ok ( m |> incPc, Cmd.none ))

        ( Load, oprd ) ->
            load oprd model |> Result.andThen (\m -> Ok ( m |> incPc, Cmd.none ))

        ( Store, oprd ) ->
            store oprd model |> Result.andThen (\m -> Ok ( m |> incPc, Cmd.none ))

        ( Add, oprd ) ->
            add oprd model |> Result.andThen (\m -> Ok ( m |> incPc, Cmd.none ))

        ( Sub, oprd ) ->
            sub oprd model |> Result.andThen (\m -> Ok ( m |> incPc, Cmd.none ))

        ( Goto, oprd ) ->
            goto oprd model |> Result.andThen (\m -> Ok ( m, Cmd.none ))

        ( Ifzero, oprd ) ->
            ifzero oprd model |> Result.andThen (\m -> Ok ( m, Cmd.none ))

        ( Ifpos, oprd ) ->
            ifpos oprd model |> Result.andThen (\m -> Ok ( m, Cmd.none ))


stop : Model -> Result String Model
stop m =
    { m | stopped = True } |> setOutput "stopped" |> Ok


get : Model -> Result String Model
get m =
    m |> Ok


print : String -> Model -> Result String Model
print s m =
    m |> setOutput s |> Ok


load : Operand -> Model -> Result String Model
load operand m =
    case operand of
        NoArg ->
            Err "Load needs operand"

        Immed n ->
            m |> setAccum n |> Ok

        Label lab ->
            m
                |> read lab
                |> Result.andThen
                    (\e ->
                        case e of
                            ( _, Data d ) ->
                                setAccum d m |> Ok

                            ( _, _ ) ->
                                Err "can't load instruction into accumulator"
                    )


store : Operand -> Model -> Result String Model
store operand m =
    case operand of
        NoArg ->
            Err "Store needs operand"

        Immed _ ->
            Err "can't store immediate value"

        Label lab ->
            m |> write lab m.accumulator


add : Operand -> Model -> Result String Model
add operand m =
    calc (+) "Add" operand m


sub : Operand -> Model -> Result String Model
sub operand m =
    calc (-) "Sub" operand m


goto : Operand -> Model -> Result String Model
goto operand m =
    case operand of
        NoArg ->
            Err "Goto needs operand"

        Immed _ ->
            Err "can't goto immediate value"

        Label lab ->
            m |> jump lab


ifzero : Operand -> Model -> Result String Model
ifzero operand m =
    condJump ((==) 0) "Ifzero" operand m


ifpos : Operand -> Model -> Result String Model
ifpos operand m =
    condJump ((>=) 0) "Ifpos" operand m



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- VIEW


view : Model -> Html Msg
view m =
    Html.div
        []
        [ viewMemory m
        , viewAccum m
        , viewOutput m
        , Html.button [ onClick Step ] [ Html.text "exec" ]
        ]


viewMemory : Model -> Html Msg
viewMemory m =
    Html.div
        []
        [ Html.p [] [ Html.text "memory" ]
        , Html.table
            [ class "memory" ]
            (Html.tr
                []
                [ Html.th [] [ Html.text "Label" ]
                , Html.th [ colspan 2 ] [ Html.text "Entry" ]
                ]
                :: (Array.indexedMap (\index entry -> viewEntry (index == m.pc) entry) m.memory |> Array.toList)
            )
        ]


viewEntry : Bool -> ( Label, Entry ) -> Html Msg
viewEntry currentPc ( label, entry ) =
    case entry of
        Code opecode operand ->
            Html.tr
                (if currentPc then
                    [ class "currentpc" ]

                 else
                    []
                )
                [ Html.td [] [ Html.text (viewLabel label) ]
                , Html.td [] [ Html.text (viewOpecode opecode) ]
                , Html.td [] [ Html.text (viewOperand operand) ]
                ]

        Data data ->
            Html.tr
                (if currentPc then
                    [ class "currentpc" ]

                 else
                    []
                )
                [ Html.td [] [ Html.text (viewLabel label) ]
                , Html.td [] [ Html.text (viewData data) ]
                , Html.td [] [ Html.text "" ]
                ]


viewLabel : Label -> String
viewLabel lab =
    lab


viewOpecode : Opecode -> String
viewOpecode =
    toString


viewOperand : Operand -> String
viewOperand operand =
    case operand of
        NoArg ->
            ""

        Immed n ->
            toString n

        Label lab ->
            viewLabel lab


viewData : Int -> String
viewData =
    toString


viewAccum : Model -> Html Msg
viewAccum m =
    Html.div
        []
        [ Html.p [] [ Html.text "accumulator" ]
        , Html.input [ type_ "text", value (toString m.accumulator) ] []
        ]


viewOutput : Model -> Html Msg
viewOutput m =
    Html.div
        []
        [ Html.p [] [ Html.text "output" ]
        , Html.textarea [ cols 50, rows 10, List.foldl (\e acc -> acc ++ e ++ "\n") "" m.output |> value ] []
        ]



-- UPDATE HELPER


calc : (Int -> Int -> Int) -> String -> Operand -> Model -> Result String Model
calc func opecodeName operand m =
    case operand of
        NoArg ->
            Err (opecodeName ++ " needs operand")

        Immed data ->
            m |> setAccum (func m.accumulator data) |> Ok

        Label lab ->
            m
                |> read lab
                |> Result.andThen
                    (\e ->
                        case e of
                            ( _, Data d ) ->
                                setAccum (m.accumulator + d) m |> Ok

                            ( _, _ ) ->
                                Err ("can't " ++ opecodeName ++ " instruction data")
                    )


condJump : (Int -> Bool) -> String -> Operand -> Model -> Result String Model
condJump pred opecodeName operand m =
    case operand of
        NoArg ->
            Err (opecodeName ++ " needs operand")

        Immed _ ->
            Err "can't goto immediate value"

        Label lab ->
            if pred m.accumulator then
                m |> jump lab

            else
                m |> incPc |> Ok


setOutput : String -> Model -> Model
setOutput s m =
    { m | output = m.output ++ [ s ] }


setAccum : Int -> Model -> Model
setAccum n m =
    { m | accumulator = n }


getAccum : Model -> Int
getAccum m =
    m.accumulator



-- 指定のラベル位置にあるメモリエントリを返す


read : Label -> Model -> Result String ( Label, Entry )
read lab m =
    case searchEntry lab m of
        [] ->
            Err ("label `" ++ lab ++ "' not found")

        e :: [] ->
            Ok e

        _ ->
            Err ("label `" ++ lab ++ "' is ambiguous")


write : Label -> Int -> Model -> Result String Model
write lab data m =
    case searchIndex lab m of
        [] ->
            Err ("label `" ++ lab ++ "' not found")

        i :: [] ->
            m |> writeAt i lab data |> Ok

        _ ->
            Err ("label `" ++ lab ++ "' is ambiguous")


jump : Label -> Model -> Result String Model
jump lab m =
    case searchIndex lab m of
        [] ->
            Err ("label `" ++ lab ++ "' not found")

        i :: [] ->
            m |> setPc i |> Ok

        _ ->
            Err ("label `" ++ lab ++ "' is ambiguous")



-- 指定のラベルでメモリ番地を検索する


searchIndex : Label -> Model -> List Int
searchIndex lab m =
    toIndexedList m.memory |> List.filter (\( _, e ) -> first e == lab) |> List.map first



-- 指定のラベルでメモリエントリを検索する


searchEntry : Label -> Model -> List ( Label, Entry )
searchEntry lab m =
    filter (\e -> first e == lab) m.memory |> toList



-- 指定のメモリ番地に書き込む


writeAt : Int -> Label -> Int -> Model -> Model
writeAt index lab data m =
    { m | memory = Array.set index ( lab, Data data ) m.memory }


incPc : Model -> Model
incPc m =
    { m | pc = m.pc + 1 }


setPc : Int -> Model -> Model
setPc n m =
    { m | pc = n }
