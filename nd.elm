module Main exposing (Model, Msg(..), Tree(..), init, insert, main, outermostStyle, pile, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (..)
import Set.Extra exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL ------------------------


type alias Model =
    { input : String
    , input2 : String
    , input3 : String
    , tree : Tree
    }


type Tree
    = Empty
    | Node String Tree Tree Tree String


init : Model
init =
    { input = ""
    , input2 = ""
    , input3 = ""
    , tree = Empty
    }



-- UPDATE ------------------------


type Msg
    = Input String
    | Input2 String
    | Input3 String
    | Extend_root
    | Extend String
    | Extend2 String
    | Extend3 String
    | Delete


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input str ->
            { model | input = str }

        Input2 str ->
            { model | input2 = str }

        Input3 str ->
            { model | input3 = str }

        Extend_root ->
            { model
                | tree = insert_root model.tree (reform model.input)
                , input = ""
            }

        Extend str ->
            { model
                | tree = insert 1 model.tree (reform model.input) "" "" str
                , input = ""
            }

        Extend2 str ->
            { model
                | tree = insert 2 model.tree (reform model.input) (reform model.input2) "" str
                , input = ""
                , input2 = ""
            }

        Extend3 str ->
            { model
                | tree = insert 3 model.tree (reform model.input) (reform model.input2) (reform model.input3) str
                , input = ""
                , input2 = ""
                , input3 = ""
            }

        Delete ->
            { model
                | input = ""
                , input2 = ""
                , input3 = ""
                , tree = Empty
            }



-- 目的：文字列 s を記号にして括弧を必要なら足して、スペースを消去する


reform : String -> String
reform s =
    elimSpace (addParenthses (symbolize s))



--目的：文字列 s の該当する文字列を記号に変換


symbolize : String -> String
symbolize s =
    let
        hojo gs i str =
            let
                sl =
                    String.length gs
            in
            if not (i < sl) then
                str

            else if String.slice i (i + 2) gs == "to" then
                hojo gs (i + 2) (str ++ "→")

            else if String.slice i (i + 2) gs == "or" then
                hojo gs (i + 2) (str ++ "∨")

            else if String.slice i (i + 2) gs == "bi" then
                hojo gs (i + 2) (str ++ "↔")

            else if String.slice i (i + 3) gs == "and" then
                hojo gs (i + 3) (str ++ "∧")

            else if String.slice i (i + 3) gs == "not" then
                hojo gs (i + 3) (str ++ "￢")

            else if String.slice i (i + 3) gs == "bot" then
                hojo gs (i + 3) (str ++ "⊥")

            else
                hojo gs (i + 1) (str ++ String.slice i (i + 1) gs)
    in
    hojo s 0 ""



--目的：原子式以外の文字列 s の両サイドに括弧がない場合、両サイドに括弧を補う


addParenthses : String -> String
addParenthses s =
    if
        (1 < String.length s)
            && not (s == "bot")
            && not
                (String.slice 0 1 s
                    == "("
                    && String.slice -1 (String.length s) s
                    == ")"
                )
    then
        "(" ++ s ++ ")"

    else
        s



--目的：文字列 s から空白を削除


elimSpace : String -> String
elimSpace s =
    String.split " " s
        |> String.concat



--目的：ルート形成


insert_root : Tree -> String -> Tree
insert_root tree f =
    Node f Empty Empty Empty "root"



--目的：木の拡張


insert : Int -> Tree -> String -> String -> String -> String -> Tree
insert n tree fl fr ft s =
    case tree of
        Empty ->
            Empty

        Node f1 t11 t12 t13 s1 ->
            if not (s == s1) then
                Node f1 (insert n t11 fl fr ft s) (insert n t12 fl fr ft s) (insert n t13 fl fr ft s) s1

            else if n == 1 then
                Node f1 (Node fl Empty Empty Empty (s ++ "l")) t12 t13 s1

            else if n == 2 then
                Node f1 (Node fl Empty Empty Empty (s ++ "l")) (Node fr Empty Empty Empty (s ++ "r")) t13 s1

            else
                -- n == 3 のみを想定
                Node f1 (Node fl Empty Empty Empty (s ++ "l")) (Node fr Empty Empty Empty (s ++ "r")) (Node ft Empty Empty Empty (s ++ "t")) s1



-- VIEW ------------------------


view : Model -> Html Msg
view model =
    div outermostStyle
        [ div []
            [ Html.form formStyle
                [ input [ style "display" "block", value model.input, onInput Input ] [] ]
            , Html.form formStyle
                [ input [ style "display" "block", value model.input2, onInput Input2 ] [] ]
            , Html.form formStyle
                [ input [ style "display" "block", value model.input3, onInput Input3 ] [] ]
            ]
        , div []
            [ div (styleWffOrNot model.input)
                [ text (elimSpace (symbolize model.input)) ]
            , div (styleWffOrNot model.input2)
                [ text (elimSpace (symbolize model.input2)) ]
            , div (styleWffOrNot model.input3)
                [ text (elimSpace (symbolize model.input3)) ]
            ]
        , if model.tree == Empty then
            div proof [ button [ onClick Extend_root ] [ text "root" ] ]

          else
            div proof [ pile model.tree ]
        , div [] [ button [ onClick Delete ] [ text "Delete" ] ]
        , div [] [ derivationOrNot model.tree ]
        ]



--目的：木 t が導出かどうかを表示


derivationOrNot : Tree -> Html Msg
derivationOrNot t =
    if t == Empty then
        text "導出を描いてみてください。"

    else if isDerivation t then
        text
            ("これは{"
                ++ List.foldl (++) "" (List.intersperse ", " (Set.toList (assumptionsOf t)))
                ++ "}から"
                ++ formulaOf t
                ++ "への導出です。"
            )

    else
        text "規則を外れています。"



--目的：木 t から証明図を描画


pile : Tree -> Html Msg
pile t =
    case trees t of
        ( Empty, Empty, Empty ) ->
            div
                []
                [ button
                    [ onClick (Extend (numberingOf t)) ]
                    [ text "1" ]
                , div conclusion [ text (formulaOf t) ]
                , button
                    [ onClick (Extend2 (numberingOf t)) ]
                    [ text "2" ]
                , button
                    [ onClick (Extend3 (numberingOf t)) ]
                    [ text "3" ]
                ]

        ( a, Empty, Empty ) ->
            div
                []
                [ div nestStyle [ pile a ]
                , div conclusion [ text (formulaOf t) ]
                ]

        ( a, b, Empty ) ->
            div
                []
                [ div nestStyle
                    [ div [] [ pile a ]
                    , div [] [ text "\u{3000}\u{3000}" ]
                    , div [] [ pile b ]
                    ]
                , div conclusion [ text (formulaOf t) ]
                ]

        ( a, b, c ) ->
            div
                []
                [ div nestStyle
                    [ div [] [ pile a ]
                    , div [] [ text "\u{3000}\u{3000}" ]
                    , div [] [ pile b ]
                    , div [] [ text "\u{3000}\u{3000}" ]
                    , div [] [ pile c ]
                    ]
                , div conclusion [ text (formulaOf t) ]
                ]



-- Styles


styleWffOrNot : String -> List (Attribute Msg)
styleWffOrNot s =
    if isWff (elimSpace (addParenthses (symbolize s))) then
        formulaStyle

    else
        formulaStyleR


formStyle =
    [ style "display" "inline-block" ]


formulaStyle =
    [ style "height" "40px"
    , style "width" "300px"
    , style "border" "1px solid blue"
    , style "display" "inline-block"
    ]


formulaStyleR =
    style "color" "red" :: formulaStyle


outermostStyle =
    [ style "width" "100%"
    , style "padding-top" "10px"
    , style "font-size" "25px"
    , style "text-align" "center"
    ]


proof =
    [ style "padding-top" "60px"
    , style "padding-bottom" "60px"
    , style "display" "inline-flex"
    , style "flex-direction" "column"
    , style "text-align" "center"
    ]


conclusion =
    [ style "display" "inline-flex" ]


nestStyle =
    [ style "display" "flex"
    , style "flex-direction" "row"
    , style "align-items" "flex-end"
    , style "border-bottom" "solid 1px"
    ]



-- Functions


alphabetList =
    [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "X", "Y", "Z", "⊥" ]


connectiveList =
    [ "→", "∨", "↔", "∧", "￢", "⊥" ]



--目的：文字列 s が原子式かどうか判定


isAtom : String -> Bool
isAtom s =
    Set.member s (Set.fromList alphabetList)



--目的：文字列 s が演算子かどうか判定


isConnective : String -> Bool
isConnective s =
    Set.member s (Set.fromList connectiveList)



--目的：文字列 s が否定式かどうか判定


isNeg : String -> Bool
isNeg s =
    String.startsWith "(￢" s && String.endsWith ")" s



--目的：文字列 s が二項演算子からなる複合式かどうか判定


isComp : String -> Bool
isComp s =
    if not (String.startsWith "(" s && String.endsWith ")" s) then
        False

    else
        let
            pBalance str =
                List.length (String.indexes "(" str) == List.length (String.indexes ")" str)
        in
        let
            hojo str i =
                if i < String.length str then
                    if pBalance (String.slice 1 i str) && isConnective (String.slice i (i + 1) str) then
                        True

                    else
                        hojo str (i + 1)

                else
                    False
        in
        hojo s 1



--目的：否定式 s から否定を削除


notBreak : String -> String
notBreak s =
    String.slice 2 -1 s



--目的：二項演算子からなる複合式 s を構成要素のタプルに分解


compBreak : String -> ( String, String, String )
compBreak s =
    let
        pBalance str =
            List.length (String.indexes "(" str) == List.length (String.indexes ")" str)
    in
    let
        hojo str i =
            if i < String.length str then
                if pBalance (String.slice 1 i str) && isConnective (String.slice i (i + 1) str) then
                    ( String.slice 1 i str, String.slice i (i + 1) str, String.slice (i + 1) -1 str )

                else
                    hojo str (i + 1)

            else
                ( "", "", "" )
    in
    hojo s 1



--目的：文字列 s が整式かどうか判定


isWff : String -> Bool
isWff s =
    if isAtom s then
        True

    else if isNeg s then
        isWff (notBreak s)

    else if isComp s then
        case compBreak s of
            ( a, b, c ) ->
                isWff a && isWff c

    else
        False



--目的：ノード t の枝を返す


trees : Tree -> ( Tree, Tree, Tree )
trees t =
    case t of
        Empty ->
            ( Empty, Empty, Empty )

        Node f t1 t2 t3 s ->
            ( t1, t2, t3 )



--目的：木 t のルートの式を返す


formulaOf : Tree -> String
formulaOf t =
    case t of
        Empty ->
            ""

        Node f t1 t2 t3 s ->
            f



--目的：木 t の通し番号を返す


numberingOf : Tree -> String
numberingOf t =
    case t of
        Empty ->
            ""

        Node f t1 t2 t3 s ->
            s



-- 目的：証明図 d のルートが仮定かどうかを判定する


isAssumption : Tree -> Bool
isAssumption t =
    case t of
        Node f Empty Empty Empty s ->
            True

        _ ->
            False



-- 目的：式 a、結合子 op、式 b から複合式を構成する


reconst : String -> String -> String -> String
reconst a op b =
    "(" ++ a ++ op ++ b ++ ")"



-- 目的：証明図 d のルートが連言導入であるかどうかを判定する


isAndIntro : Tree -> Bool
isAndIntro t =
    case t of
        Node f t1 t2 Empty s ->
            case compBreak f of
                ( a, b, c ) ->
                    b == "∧" && a == formulaOf t1 && c == formulaOf t2

        _ ->
            False



-- 目的：証明図 d のルートが連言除去であるかどうかを判定する


isAndElim : Tree -> Bool
isAndElim t =
    case t of
        Node f t1 Empty Empty s ->
            case compBreak (formulaOf t1) of
                ( a, b, c ) ->
                    b == "∧" && (f == a || f == c)

        _ ->
            False



-- 目的：証明図 d のルートが条件法導入であるかどうかを判定する


isImpIntro : Tree -> Bool
isImpIntro t =
    case t of
        Node f t1 Empty Empty s ->
            case compBreak f of
                ( a, b, c ) ->
                    b == "→" && c == formulaOf t1

        _ ->
            False



-- 目的：証明図 d のルートが条件法除去であるかどうかを判定する


isImpElim : Tree -> Bool
isImpElim t =
    case t of
        Node f t1 t2 Empty s ->
            case compBreak (formulaOf t1) of
                ( a, b, c ) ->
                    b == "→" && f == c && a == formulaOf t2

        _ ->
            False



-- 目的：証明図 d のルートが否定導入であるかどうかを判定する


isNegIntro : Tree -> Bool
isNegIntro t =
    case t of
        Node f t1 Empty Empty s ->
            isNeg f && formulaOf t1 == "⊥"

        _ ->
            False



-- 目的：証明図 d のルートが条件法除去であるかどうかを判定する


isNegElim : Tree -> Bool
isNegElim t =
    case t of
        Node f t1 t2 Empty s ->
            isNeg (formulaOf t1)
                && formulaOf t2
                == notBreak (formulaOf t1)
                && f
                == "⊥"

        _ ->
            False



-- 目的：証明図 d のルートが選言導入であるかどうかを判定する


isOrIntro : Tree -> Bool
isOrIntro t =
    case t of
        Node f t1 Empty Empty s ->
            case compBreak f of
                ( a, b, c ) ->
                    b == "∨" && (formulaOf t1 == a || formulaOf t1 == c)

        _ ->
            False



-- 目的：証明図 d のルートが選言除去であるかどうかを判定する


isOrElim : Tree -> Bool
isOrElim t =
    case t of
        Node f t1 t2 t3 s ->
            case compBreak (formulaOf t1) of
                ( a, b, c ) ->
                    b == "∨" && f == formulaOf t2 && f == formulaOf t3

        _ ->
            False



-- 目的：証明図 d のルートがEFQであるかどうかを判定する


isEFQ : Tree -> Bool
isEFQ t =
    case t of
        Node f t1 Empty Empty s ->
            formulaOf t1 == "⊥"

        _ ->
            False



-- 目的：証明図 d のルートがルールに沿っているかを判定する


checkRoot : Tree -> Bool
checkRoot t =
    isAssumption t
        || isAndIntro t
        || isAndElim t
        || isImpIntro t
        || isImpElim t
        || isNegIntro t
        || isNegElim t
        || isOrIntro t
        || isOrElim t
        || isEFQ t


isDerivation : Tree -> Bool
isDerivation t =
    case t of
        Empty ->
            True

        Node f t1 t2 t3 s ->
            checkRoot t && isDerivation t1 && isDerivation t2 && isDerivation t3



-- 目的：証明図 t から仮定の集合を文字列の集合として取り出す
-- 注意：消去された仮定は取り出さない


assumptionsOf : Tree -> Set String
assumptionsOf t =
    let
        hojo tt set =
            case tt of
                Empty ->
                    set

                Node f tt1 tt2 tt3 s ->
                    case ( tt1, tt2, tt3 ) of
                        ( Empty, Empty, Empty ) ->
                            Set.insert f set

                        _ ->
                            if isImpIntro tt then
                                case compBreak f of
                                    ( a, b, c ) ->
                                        Set.remove a (hojo tt1 set)

                            else if isNegIntro tt then
                                Set.remove (notBreak f) (hojo tt1 set)

                            else if isOrElim tt then
                                case compBreak (formulaOf tt1) of
                                    ( a, b, c ) ->
                                        hojo tt1
                                            (Set.union
                                                (Set.remove a
                                                    (hojo tt2 set)
                                                )
                                                (Set.remove
                                                    c
                                                    (hojo tt3 set)
                                                )
                                            )

                            else
                                hojo tt1
                                    (hojo tt1 (hojo tt2 (hojo tt3 set)))
    in
    hojo t Set.empty



-- 目的：証明図 d が gamma から sigma への導出かどうかを調べる


isDerivationFromTo : ( Set String, String ) -> Tree -> Bool
isDerivationFromTo ( gamma, sigma ) t =
    if not (isDerivation t) then
        False

    else
        case t of
            Empty ->
                False

            Node f t1 t2 t3 s ->
                f == sigma && Set.Extra.subset (assumptionsOf t) gamma
