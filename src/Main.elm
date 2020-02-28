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
    { exercises : Int
    , work : Int
    , rest : Int
    , rounds : Int
    , roundRest : Int
    }


type Phase
    = Work Int
    | Rest Int
    | RestBetweenRounds Int


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
    , currentExercise : Int
    , currentRound : Int
    }


initialCountdown : Int
initialCountdown =
    3


init : () -> ( Model, Cmd Msg )
init _ =
    ( { step = Setup
      , timer =
            { exercises = 10
            , work = 5
            , rest = 5
            , rounds = 3
            , roundRest = 120
            }
      , currentExercise = 1
      , currentRound = 1
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
    | UpdateRounds String
    | UpdateRoundRest String
    | UpdateExercisesQuantity String
    | UpdateWork String
    | UpdateRest String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ timer, step, countdown, currentExercise, currentRound } as model) =
    case msg of
        RunTimer ->
            ( { model | step = Running (Work timer.work) }
            , Cmd.none
            )

        Cancel ->
            ( { model
                | step = Setup
                , countdown = initialCountdown
                , currentExercise = 1
                , currentRound = 1
              }
            , stopAllSounds ()
            )

        TogglePause ->
            case step of
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

        UpdateExercisesQuantity r ->
            let
                upd t v =
                    { t | exercises = v }
            in
            ( { model | timer = updateTimerSetting timer upd r }
            , Cmd.none
            )

        UpdateRest newRest ->
            let
                upd t v =
                    { t | rest = v }
            in
            ( { model | timer = updateTimerSetting timer upd newRest }
            , Cmd.none
            )

        UpdateWork newWork ->
            let
                upd t v =
                    { t | work = v }
            in
            ( { model | timer = updateTimerSetting timer upd newWork }
            , Cmd.none
            )

        UpdateRounds newRounds ->
            let
                upd t v =
                    { t | rounds = v }
            in
            ( { model | timer = updateTimerSetting timer upd newRounds }
            , Cmd.none
            )

        UpdateRoundRest newValue ->
            let
                upd t v =
                    { t | roundRest = v }
            in
            ( { model | timer = updateTimerSetting timer upd newValue }
            , Cmd.none
            )

        Tick _ ->
            case step of
                Countdown ->
                    if countdown == 0 then
                        ( { model | step = Running (Work timer.work) }
                        , play "bell"
                        )

                    else
                        ( { model | countdown = countdown - 1 }
                        , silenceOrCountdown countdown
                        )

                Running phase ->
                    case phase of
                        Work t ->
                            let
                                isLastExercise =
                                    currentExercise == timer.exercises

                                isLastRound =
                                    currentRound == timer.rounds
                            in
                            case ( t, isLastExercise, isLastRound ) of
                                ( 0, True, True ) ->
                                    ( { model | step = Finished }
                                    , play "final"
                                    )

                                ( 0, True, _ ) ->
                                    ( { model
                                        | step = Running (RestBetweenRounds timer.roundRest)
                                      }
                                    , play "alert"
                                    )

                                ( 0, _, _ ) ->
                                    ( { model | step = Running (Rest timer.rest) }
                                    , play "alert"
                                    )

                                _ ->
                                    ( { model | step = Running (Work (t - 1)) }
                                    , silenceOrCountdown t
                                    )

                        Rest t ->
                            if t == 0 then
                                ( { model
                                    | step = Running (Work timer.work)
                                    , currentExercise = currentExercise + 1
                                  }
                                , play "bell"
                                )

                            else
                                ( { model | step = Running (Rest (t - 1)) }
                                , silenceOrCountdown t
                                )

                        RestBetweenRounds t ->
                            if t == 0 then
                                ( { model
                                    | step = Running (Work timer.work)
                                    , currentExercise = 1
                                    , currentRound = currentRound + 1
                                  }
                                , play "bell"
                                )

                            else
                                ( { model | step = Running (RestBetweenRounds (t - 1)) }
                                , silenceOrCountdown t
                                )

                _ ->
                    ( model, Cmd.none )


silenceOrCountdown : Int -> Cmd msg
silenceOrCountdown t =
    if t == 1 then
        play "count_beep"

    else if t <= 4 then
        play "count_tick"

    else
        Cmd.none


updateTimerSetting : Timer -> (Timer -> Int -> Timer) -> String -> Timer
updateTimerSetting timer updater newValue =
    let
        newVal =
            Maybe.withDefault 0 (String.toInt newValue)
    in
    updater timer newVal


subscriptions : Model -> Sub Msg
subscriptions { step } =
    case step of
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
                viewFinished

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


viewFinished =
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
            [ img [ src "./img/arny_thumbs_up.png" ] []
            ]
        ]


viewTimer { step, timer, currentRound, currentExercise } phase =
    let
        isPaused =
            case step of
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

                RestBetweenRounds t ->
                    ( "Round rest", t )
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
            , p [ class "exercise" ]
                [ text
                    ("Exercise "
                        ++ String.fromInt currentExercise
                        ++ " of "
                        ++ String.fromInt timer.exercises
                    )
                ]
            , p [ class "round" ]
                [ text
                    ("Round "
                        ++ String.fromInt currentRound
                        ++ " of "
                        ++ String.fromInt timer.rounds
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


viewTimerForm { exercises, work, rest, rounds, roundRest } =
    form
        [ onSubmit ShowCountdown ]
        [ section [ class "repeats" ]
            [ section [ class "exercises" ]
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
                , div [ class "row row_exercises" ]
                    [ label [] [ text "Exercises" ]
                    , select [ onInput UpdateExercisesQuantity ] (viewRenderOptions 1 20 1 exercises)
                    ]
                ]
            , div [ class "row row_round" ]
                [ div [ class "row__inner" ]
                    [ label [] [ text "Rounds" ]
                    , select [ onInput UpdateRounds ] (viewRenderOptions 1 10 1 rounds)
                    ]
                , div [ class "row__inner" ]
                    [ label [ class "label_repeat-with" ] [ text "with rest" ]
                    , if rounds <= 1 then
                        select
                            [ onInput UpdateRoundRest
                            , disabled True
                            ]
                            [ option [] [ text "0" ] ]

                      else
                        select
                            [ onInput UpdateRoundRest
                            ]
                            (viewRenderOptions 0 240 5 roundRest)
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
