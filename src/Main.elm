port module Main exposing (..)

import Array exposing (..)
import Browser exposing (..)
import Char exposing (isAlpha, isAlphaNum)
import Cmd.Extra exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (reverse)
import Parser exposing (..)
import Result exposing (Result)
import Set exposing (..)
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
    , src : String
    , memory : Memory
    , output : List String
    , stopped : Bool
    }


initialSrc =
    """Load CNT;
LOOP Ifzero BREAK;
Get;
Add SUM;
Store SUM;
Load CNT;
Sub 1;
Store CNT;
Goto LOOP;
BREAK Load SUM;
Print;
Stop;
CNT 3;
SUM 0;
"""


initialModel : Model
initialModel =
    { pc = 0
    , accumulator = 0
    , src = initialSrc
    , memory = Array.empty
    , output = []
    , stopped = False
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
    ( initialModel, Cmd.none )



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- UPDATE


type Msg
    = Inst ( Label, Entry )
    | Recv String
    | InputSrc String
    | Compile
    | Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Compile ->
            case compile model.src of
                Ok memory ->
                    ( { model | memory = memory, output = [] }, Cmd.none )

                Err errorMsg ->
                    ( initialModel |> setOutput errorMsg |> setSrc model.src, Cmd.none )

        InputSrc src ->
            ( { model | src = src }, Cmd.none )

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
        [ viewSrc m
        , Html.button [ onClick Compile ] [ Html.text "compile" ]
        , viewMemory m
        , Html.button [ onClick Step ] [ Html.text "step" ]
        , viewAccum m
        , viewOutput m
        ]


viewSrc : Model -> Html Msg
viewSrc model =
    Html.div
        []
        [ Html.p [] [ Html.text "src code" ]
        , Html.textarea [ cols 20, rows 20, value model.src, onInput InputSrc ] []
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


setSrc : String -> Model -> Model
setSrc s m =
    { m | src = s }



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
    Array.filter (\e -> first e == lab) m.memory |> Array.toList



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



--------------------------------------- Parser -------------------------------


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\t')


labelP : Parser Label
labelP =
    variable
        { start = isAlpha
        , inner = isAlphaNum
        , reserved = Set.fromList [ "Stop", "Get", "Print", "Load", "Store", "Add", "Sub", "Goto", "Ifzero", "Ifpos" ]
        }



-- 引数を取らない命令のparser
-- stop get print


noOperandCodeP : String -> Opecode -> Parser Entry
noOperandCodeP opname opecode =
    succeed identity
        |. whitespace
        |. keyword opname
        |= succeed (Code opecode NoArg)
        |. whitespace
        |. symbol ";"
        |. spaces


stopP : Parser Entry
stopP =
    noOperandCodeP "Stop" Stop


getP : Parser Entry
getP =
    noOperandCodeP "Get" Get


printP : Parser Entry
printP =
    noOperandCodeP "Print" Print



-- ラベルを引数にとる命令のparser
-- load store goto ifzero ifpos


labelOperandCodeP : String -> Opecode -> Parser Entry
labelOperandCodeP opname opecode =
    succeed (\lab -> Code opecode (Label lab))
        |. whitespace
        |. keyword opname
        |. whitespace
        |= labelP
        |. whitespace
        |. symbol ";"
        |. spaces


loadP : Parser Entry
loadP =
    labelOperandCodeP "Load" Load


storeP : Parser Entry
storeP =
    labelOperandCodeP "Store" Store


gotoP : Parser Entry
gotoP =
    labelOperandCodeP "Goto" Goto


ifzeroP : Parser Entry
ifzeroP =
    labelOperandCodeP "Ifzero" Ifzero


ifposP : Parser Entry
ifposP =
    labelOperandCodeP "Ifpos" Ifpos


operandCodeP : String -> Opecode -> Parser Entry
operandCodeP opname opecode =
    succeed (\arg -> Code opecode arg)
        |. whitespace
        |. keyword opname
        |. whitespace
        |= oneOf
            [ succeed Immed |= int
            , succeed Label |= labelP
            ]
        |. whitespace
        |. symbol ";"
        |. spaces


addP : Parser Entry
addP =
    operandCodeP "Add" Add


subP : Parser Entry
subP =
    operandCodeP "Sub" Sub


dataP : Parser Entry
dataP =
    succeed Data
        |. whitespace
        |= int
        |. whitespace
        |. symbol ";"
        |. spaces


codeP : Parser Entry
codeP =
    oneOf
        (List.map backtrackable
            [ stopP
            , getP
            , printP
            , loadP
            , storeP
            , addP
            , subP
            , gotoP
            , ifzeroP
            , ifposP
            ]
        )


codeStatementP : Parser ( Label, Entry )
codeStatementP =
    oneOf
        [ backtrackable (succeed (\label -> \code -> ( label, code )) |. spaces |= labelP |= codeP)
        , backtrackable (succeed (\code -> ( "", code )) |. spaces |= codeP)
        , backtrackable (succeed (\label -> \data -> ( label, data )) |. spaces |= labelP |= dataP)
        ]


compiler : Parser (List ( Label, Entry ))
compiler =
    Parser.loop
        []
        (\stmts ->
            oneOf
                [ succeed (\stmt -> Loop (stmt :: stmts))
                    |= codeStatementP
                , succeed ()
                    |> Parser.map (\_ -> Done (reverse stmts))
                ]
        )
        |. end


compile : String -> Result String Memory
compile src =
    case run compiler src of
        Ok list ->
            Ok (Array.fromList list)

        Err deadEnd ->
            Err (Debug.toString deadEnd)



-- Err (deadEndsToString deadEnd)
