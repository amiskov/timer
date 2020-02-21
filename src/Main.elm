port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, selected, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Time


port loadSample : String -> Cmd msg


port resumeAudioContext : () -> Cmd msg


port play : String -> Cmd msg


port stopAllSounds : () -> Cmd msg


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Timer =
    { rounds : Int
    , work : Int
    , rest : Int
    , circuits : Int
    , circuitRest : Int
    }


type Phase
    = Work Int
    | Rest Int
    | RestBetweenCircuits Int


type Step
    = Setup
    | Countdown
    | Running Phase
    | Paused Phase
    | Finished


type alias Model =
    { step : Step
    , timer : Timer
    , countdown : Int
    , currentRound : Int
    , currentCircuit : Int
    }


initialCountdown : Int
initialCountdown =
    3


init : () -> ( Model, Cmd Msg )
init _ =
    ( { step = Setup
      , timer =
            { rounds = 1
            , work = 5
            , rest = 5
            , circuits = 1
            , circuitRest = 0
            }
      , currentRound = 1
      , currentCircuit = 1
      , countdown = initialCountdown
      }
    , Cmd.batch
        [ loadSample "count_tick"
        , loadSample "count_beep"
        , loadSample "alert"
        , loadSample "bell"
        , loadSample "final"
        ]
    )


type Msg
    = Tick Time.Posix
    | RunTimer
    | Cancel
    | TogglePause
    | ShowCountdown
    | UpdateCircuits String
    | UpdateCircuitRest String
    | UpdateRoundsQuantity String
    | UpdateWork String
    | UpdateRest String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RunTimer ->
            ( { model | step = Running (Work model.timer.work) }
            , Cmd.none
            )

        Cancel ->
            ( { model
                | step = Setup
                , countdown = initialCountdown
                , currentRound = 1
                , currentCircuit = 1
              }
            , stopAllSounds ()
            )

        TogglePause ->
            case model.step of
                Paused phase ->
                    ( { model | step = Running phase }
                    , Cmd.none
                    )

                Running phase ->
                    ( { model | step = Paused phase }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ShowCountdown ->
            ( { model | step = Countdown }
            , Cmd.batch [ resumeAudioContext (), play "count_tick" ]
            )

        UpdateRoundsQuantity r ->
            let
                upd t v =
                    { t | rounds = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd r }
            , Cmd.none
            )

        UpdateRest newRest ->
            let
                upd t v =
                    { t | rest = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd newRest }
            , Cmd.none
            )

        UpdateWork newWork ->
            let
                upd t v =
                    { t | work = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd newWork }
            , Cmd.none
            )

        UpdateCircuits newCircuits ->
            let
                upd t v =
                    { t | circuits = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd newCircuits }
            , Cmd.none
            )

        UpdateCircuitRest newValue ->
            let
                upd t v =
                    { t | circuitRest = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd newValue }
            , Cmd.none
            )

        Tick _ ->
            case model.step of
                Countdown ->
                    if model.countdown <= 0 then
                        ( { model | step = Running (Work model.timer.work) }
                        , play "bell"
                        )

                    else if model.countdown > 1 then
                        ( { model | countdown = model.countdown - 1 }
                        , play "count_tick"
                        )

                    else
                        ( { model | countdown = model.countdown - 1 }
                        , play "count_beep"
                        )

                Running phase ->
                    case phase of
                        Work t ->
                            case t of
                                0 ->
                                    if model.currentRound == model.timer.rounds then
                                        if model.currentCircuit == model.timer.circuits then
                                            ( { model | step = Finished }
                                            , play "final"
                                            )

                                        else
                                            ( { model
                                                | step = Running (RestBetweenCircuits model.timer.circuitRest)
                                              }
                                            , play "alert"
                                            )

                                    else
                                        ( { model | step = Running (Rest model.timer.rest) }
                                        , play "alert"
                                        )

                                _ ->
                                    ( { model | step = Running (Work (t - 1)) }
                                    , Cmd.none
                                    )

                        Rest t ->
                            if t == 0 then
                                ( { model
                                    | step = Running (Work model.timer.work)
                                    , currentRound = model.currentRound + 1
                                  }
                                , play "bell"
                                )

                            else
                                ( { model | step = Running (Rest (t - 1)) }, Cmd.none )

                        RestBetweenCircuits t ->
                            if t == 0 then
                                ( { model
                                    | step = Running (Work model.timer.work)
                                    , currentRound = 1
                                    , currentCircuit = model.currentCircuit + 1
                                  }
                                , play "bell"
                                )

                            else
                                ( { model | step = Running (RestBetweenCircuits (t - 1)) }
                                , Cmd.none
                                )

                _ ->
                    ( model, Cmd.none )


updateTimerSetting timer updater newValue =
    let
        newVal =
            Maybe.withDefault 0 (String.toInt newValue)
    in
    updater timer newVal


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.step of
        Running _ ->
            Time.every 1000 Tick

        Countdown ->
            Time.every 1000 Tick

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    div
        []
        [ case model.step of
            Setup ->
                viewTimerForm model.timer

            Finished ->
                viewFinished model

            Paused phase ->
                viewTimer model phase

            Countdown ->
                div
                    [ class "countdown"
                    , class
                        (if model.countdown > 0 then
                            "countdown_tick"

                         else
                            "countdown_go"
                        )
                    ]
                    (if model.countdown > 0 then
                        [ text (String.fromInt model.countdown) ]

                     else
                        [ text "Go!" ]
                    )

            Running phase ->
                viewTimer model phase
        ]


viewFinished model =
    div [ class "final" ]
        [ h1 [] [ text "Well Done!" ]
        , div [ class "row row_btn row_btn-ok" ]
            [ button
                [ class "btn btn_ok"
                , onClick Cancel
                ]
                [ text "← Back" ]
            ]
        , div [ class "final-image" ]
            [ img [ src "./static/img/arny_thumbs_up.png" ] []
            ]
        ]


viewTimer model phase =
    let
        isPaused =
            case model.step of
                Paused _ ->
                    True

                _ ->
                    False

        ( phaseText, phaseTime ) =
            case phase of
                Work t ->
                    ( "Work", t )

                Rest t ->
                    ( "Rest", t )

                RestBetweenCircuits t ->
                    ( "Circuit rest", t )
    in
    div
        [ class "wrapper"
        , class
            (if isPaused then
                "paused"

             else
                ""
            )
        ]
        [ div
            [ class "phase"
            , class ("phase_" ++ String.toLower (String.replace " " "-" phaseText))
            ]
            [ h2 [ class "phase-title" ]
                [ if phaseTime == 0 then
                    s [] [ text phaseText ]

                  else
                    text (phaseText ++ ": " ++ String.fromInt phaseTime)
                ]
            , p [ class "round" ]
                [ text
                    ("Round "
                        ++ String.fromInt model.currentRound
                        ++ " of "
                        ++ String.fromInt model.timer.rounds
                    )
                ]
            , p [ class "circuit" ]
                [ text
                    ("Circuit "
                        ++ String.fromInt model.currentCircuit
                        ++ " of "
                        ++ String.fromInt model.timer.circuits
                    )
                ]
            , div [ class "row row_btn row_pause" ]
                [ button
                    [ onClick TogglePause
                    , class "btn"
                    , class
                        (if isPaused then
                            "btn_continue"

                         else
                            "btn_pause"
                        )
                    ]
                    [ text
                        (if isPaused then
                            "Continue"

                         else
                            "Pause"
                        )
                    ]
                ]
            , div [ class "row row_btn row_cancel" ]
                [ button
                    [ onClick Cancel
                    , class "btn btn_cancel"
                    ]
                    [ text "Cancel" ]
                ]
            ]
        ]


viewTimerForm { rounds, work, rest, circuits, circuitRest } =
    form
        [ onSubmit ShowCountdown ]
        [ section [ class "repeats" ]
            [ section [ class "rounds" ]
                [ section [ class "phases" ]
                    [ div [ class "row row_work" ]
                        [ label [] [ text "Work" ]
                        , select [ onInput UpdateWork ] (viewRenderOptions 1 240 5 work)
                        ]
                    , div [ class "row row_rest" ]
                        [ label [] [ text "Rest" ]
                        , select [ onInput UpdateRest ] (viewRenderOptions 1 120 5 rest)
                        ]
                    ]
                , div [ class "row row_rounds" ]
                    [ label [] [ text "Rounds" ]
                    , select [ onInput UpdateRoundsQuantity ] (viewRenderOptions 1 20 1 rounds)
                    ]
                ]
            , div [ class "row row_circuit" ]
                [ div [ class "row__inner" ]
                    [ label [] [ text "Circuits" ]
                    , select [ onInput UpdateCircuits ] (viewRenderOptions 1 10 1 circuits)
                    ]
                , div [ class "row__inner" ]
                    [ label [ class "label_repeat-with" ] [ text "with rest" ]
                    , if circuits <= 1 then
                        select
                            [ onInput UpdateCircuitRest
                            , disabled True
                            ]
                            [ option [] [ text "0" ] ]

                      else
                        select
                            [ onInput UpdateCircuitRest
                            ]
                            (viewRenderOptions 0 240 5 circuitRest)
                    ]
                ]
            ]
        , div [ class "row row_btn row_go" ]
            [ button
                [ type_ "submit"
                , class "btn btn_go"
                ]
                [ text "Get it done!" ]
            ]
        ]


rangeWithStep : Int -> Int -> Int -> List Int
rangeWithStep start end step =
    let
        stepper n acc =
            if remainderBy step n == 0 then
                List.append acc [ n ]

            else
                acc
    in
    List.foldl stepper [] (List.range start end)


viewRenderOptions start end step selectedOptionNum =
    let
        renderOption n =
            option
                [ selected (selectedOptionNum == n)
                , value (String.fromInt n)
                ]
                [ text (String.fromInt n)
                ]
    in
    List.map renderOption (rangeWithStep start end step)
