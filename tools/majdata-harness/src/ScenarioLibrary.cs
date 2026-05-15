namespace MajdataHarness;

public sealed class Scenario
{
    public required string Name { get; init; }
    public required Func<ScenarioResult> Run { get; init; }
}

public static class ScenarioLibrary
{
    public static IReadOnlyList<Scenario> All() =>
    [
        new Scenario
        {
            Name = "Tap consumes shared click before hold head",
            Run = RunTapThenHoldSharedClick
        },
        new Scenario
        {
            Name = "Hold head consumes shared click before tap when order is reversed",
            Run = RunHoldThenTapSharedClick
        },
        new Scenario
        {
            Name = "Future tap head does not steal click before judgeable",
            Run = RunFutureTapHeadDoesNotStealHoldClick
        },
        new Scenario
        {
            Name = "Touch consumes shared click before touch-hold head",
            Run = RunTouchThenTouchHoldSharedClick
        },
        new Scenario
        {
            Name = "Touch-hold head blocked when touch queue head has same click",
            Run = RunTouchBlocksTouchHoldBySharedQueue
        },
        new Scenario
        {
            Name = "Touch-hold head can resolve from group share",
            Run = RunTouchHoldGroupShare
        },
        new Scenario
        {
            Name = "Two button clicks let tap then hold both consume on mobile-style counts",
            Run = RunTapThenHoldWithTwoButtonClicks
        },
        new Scenario
        {
            Name = "Two sensor clicks let touch then touch-hold both consume on mobile-style counts",
            Run = RunTouchThenTouchHoldWithTwoSensorClicks
        },
        new Scenario
        {
            Name = "Modern hold body stays pressed while button remains on",
            Run = RunModernHoldBodyPressed
        },
        new Scenario
        {
            Name = "Modern hold short release stays inside ignore window",
            Run = RunModernHoldBodyShortReleaseIgnored
        },
        new Scenario
        {
            Name = "Modern hold release before end degrades final result shape",
            Run = RunModernHoldBodyReleasedThenEnded
        },
        new Scenario
        {
            Name = "Missed modern hold head skips release-ignore grace",
            Run = RunMissedModernHoldHeadSkipsReleaseGrace
        },
        new Scenario
        {
            Name = "Released touch-hold recovers when body input returns",
            Run = RunReleasedTouchHoldRecoversWhenBodyInputReturns
        },
        new Scenario
        {
            Name = "Touch-hold shared head requires strict majority",
            Run = RunTouchHoldSharedHeadRequiresStrictMajority
        },
        new Scenario
        {
            Name = "Wifi too-late with two single tails is LateGood by max remaining",
            Run = RunWifiTooLateTwoSingleTailsByMaxRemaining
        },
        new Scenario
        {
            Name = "One sensor hold may advance overlapping slides together",
            Run = RunOneSensorHoldMayAdvanceOverlappingSlidesTogether
        },
        new Scenario
        {
            Name = "Conn-slide child checkability and force-finish follow reference rule shape",
            Run = RunConnSlideCheckabilityAndForceFinish
        },
        new Scenario
        {
            Name = "Wifi progress markers and too-late grading follow reference rule shape",
            Run = RunWifiProgressAndTooLate
        },
        new Scenario
        {
            Name = "Reference-like slide skip chain does not clear last area early",
            Run = RunReferenceLikeSlideSkipChain
        }
    ];

    private static ScenarioResult RunTapThenHoldSharedClick()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTapIndex(0, 1);
        noteManager.SetButtonClicked(0);
        noteManager.SetButtonStatus(0, SwitchStatus.On);

        var tap = ReferenceLikeLogic.RunTapCheck(
            noteManager,
            new TapQueueInfo { Index = 1, KeyIndex = 1 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        var hold = ReferenceLikeLogic.RunHoldHeadCheck(
            noteManager,
            new HoldQueueInfo { Index = 1, KeyIndex = 1 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        return new ScenarioResult
        {
            Name = "tap-first-shared-click",
            Tap = tap,
            Hold = hold
        };
    }

    private static ScenarioResult RunHoldThenTapSharedClick()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTapIndex(0, 1);
        noteManager.SetButtonClicked(0);
        noteManager.SetButtonStatus(0, SwitchStatus.On);

        var hold = ReferenceLikeLogic.RunHoldHeadCheck(
            noteManager,
            new HoldQueueInfo { Index = 1, KeyIndex = 1 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        var tap = ReferenceLikeLogic.RunTapCheck(
            noteManager,
            new TapQueueInfo { Index = 1, KeyIndex = 1 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        return new ScenarioResult
        {
            Name = "hold-first-shared-click",
            Tap = tap,
            Hold = hold
        };
    }

    private static ScenarioResult RunFutureTapHeadDoesNotStealHoldClick()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTapIndex(0, 1);
        noteManager.SetButtonClicked(0);
        noteManager.SetButtonStatus(0, SwitchStatus.On);

        var tap = ReferenceLikeLogic.RunTapCheck(
            noteManager,
            new TapQueueInfo { Index = 2, KeyIndex = 1 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: false);

        var hold = ReferenceLikeLogic.RunHoldHeadCheck(
            noteManager,
            new HoldQueueInfo { Index = 1, KeyIndex = 1 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        return new ScenarioResult
        {
            Name = "future-tap-head-does-not-steal-click",
            Tap = tap,
            Hold = hold
        };
    }

    private static ScenarioResult RunTouchThenTouchHoldSharedClick()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTouchIndex(0, 0);
        noteManager.SetButtonClicked(0);
        noteManager.SetButtonStatus(0, SwitchStatus.On);

        var touch = ReferenceLikeLogic.RunTouchCheck(
            noteManager,
            new TouchQueueInfo { Index = 0, SensorArea = 0 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        var touchHold = ReferenceLikeLogic.RunTouchHoldHeadCheck(
            noteManager,
            new TouchQueueInfo { Index = 1, SensorArea = 0 },
            sensorArea: 0,
            buttonZone: 0,
            inJudgeableRange: true,
            groupShareAvailable: false);

        return new ScenarioResult
        {
            Name = "touch-first-shared-click",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false },
            Touch = touch,
            TouchHold = touchHold
        };
    }

    private static ScenarioResult RunTouchBlocksTouchHoldBySharedQueue()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTouchIndex(0, 0);
        noteManager.SetButtonClicked(0);
        noteManager.SetButtonStatus(0, SwitchStatus.On);

        var touchHold = ReferenceLikeLogic.RunTouchHoldHeadCheck(
            noteManager,
            new TouchQueueInfo { Index = 1, SensorArea = 0 },
            sensorArea: 0,
            buttonZone: 0,
            inJudgeableRange: true,
            groupShareAvailable: false);

        var touch = ReferenceLikeLogic.RunTouchCheck(
            noteManager,
            new TouchQueueInfo { Index = 0, SensorArea = 0 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        return new ScenarioResult
        {
            Name = "touch-shared-queue-blocks-touchhold",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false },
            Touch = touch,
            TouchHold = touchHold
        };
    }

    private static ScenarioResult RunTouchHoldGroupShare()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTouchIndex(0, 0);

        var touchHold = ReferenceLikeLogic.RunTouchHoldHeadCheck(
            noteManager,
            new TouchQueueInfo { Index = 0, SensorArea = 0 },
            sensorArea: 0,
            buttonZone: 0,
            inJudgeableRange: true,
            groupShareAvailable: true);

        return new ScenarioResult
        {
            Name = "touchhold-group-share",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false },
            Touch = null,
            TouchHold = touchHold
        };
    }

    private static ScenarioResult RunTapThenHoldWithTwoButtonClicks()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTapIndex(0, 1);
        noteManager.SetButtonClicked(0);
        noteManager.SetButtonClickedCount(0, 2);
        noteManager.SetButtonStatus(0, SwitchStatus.On);

        var tap = ReferenceLikeLogic.RunTapCheck(
            noteManager,
            new TapQueueInfo { Index = 1, KeyIndex = 1 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        var hold = ReferenceLikeLogic.RunHoldHeadCheck(
            noteManager,
            new HoldQueueInfo { Index = 1, KeyIndex = 1 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        return new ScenarioResult
        {
            Name = "tap-then-hold-two-button-clicks",
            Tap = tap,
            Hold = hold
        };
    }

    private static ScenarioResult RunTouchThenTouchHoldWithTwoSensorClicks()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTouchIndex(0, 0);
        noteManager.SetSensorClicked(0);
        noteManager.SetSensorClickedCount(0, 2);
        noteManager.SetSensorStatus(0, SwitchStatus.On);

        var touch = ReferenceLikeLogic.RunTouchCheck(
            noteManager,
            new TouchQueueInfo { Index = 0, SensorArea = 0 },
            buttonZone: 0,
            sensorArea: 0,
            inJudgeableRange: true);

        var touchHold = ReferenceLikeLogic.RunTouchHoldHeadCheck(
            noteManager,
            new TouchQueueInfo { Index = 1, SensorArea = 0 },
            sensorArea: 0,
            buttonZone: 0,
            inJudgeableRange: true,
            groupShareAvailable: false);

        return new ScenarioResult
        {
            Name = "touch-then-touchhold-two-sensor-clicks",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false },
            Touch = touch,
            TouchHold = touchHold
        };
    }

    private static ScenarioResult RunModernHoldBodyPressed()
    {
        var holdBody = ReferenceLikeLogic.RunModernHoldBodyFrame(
            isHeadJudged: true,
            isButtonPressed: true,
            isSensorPressed: false,
            priorReleaseTimeSec: 0f,
            playerReleaseTimeSec: 0f,
            deltaTimeSec: 1f / 60f,
            forceEnd: false);

        return new ScenarioResult
        {
            Name = "modern-hold-body-pressed",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = true, Grade = JudgeGrade.Perfect, QueueAdvanced = true },
            HoldBody = holdBody
        };
    }

    private static ScenarioResult RunModernHoldBodyShortReleaseIgnored()
    {
        var holdBody = ReferenceLikeLogic.RunModernHoldBodyFrame(
            isHeadJudged: true,
            isButtonPressed: false,
            isSensorPressed: false,
            priorReleaseTimeSec: 0.10f,
            playerReleaseTimeSec: 0f,
            deltaTimeSec: 1f / 60f,
            forceEnd: false);

        return new ScenarioResult
        {
            Name = "modern-hold-short-release-ignored",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = true, Grade = JudgeGrade.Perfect, QueueAdvanced = true },
            HoldBody = holdBody
        };
    }

    private static ScenarioResult RunModernHoldBodyReleasedThenEnded()
    {
        var released = ReferenceLikeLogic.RunModernHoldBodyFrame(
            isHeadJudged: true,
            isButtonPressed: false,
            isSensorPressed: false,
            priorReleaseTimeSec: 0.20f,
            playerReleaseTimeSec: 0.25f,
            deltaTimeSec: 1f / 60f,
            forceEnd: false);

        var ended = ReferenceLikeLogic.RunModernHoldBodyFrame(
            isHeadJudged: true,
            isButtonPressed: false,
            isSensorPressed: false,
            priorReleaseTimeSec: 0.20f,
            playerReleaseTimeSec: 0.25f,
            deltaTimeSec: 1f / 60f,
            forceEnd: true);

        return new ScenarioResult
        {
            Name = "modern-hold-released-then-ended",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = true, Grade = JudgeGrade.Perfect, QueueAdvanced = true },
            HoldBody = new HoldBodyResult
            {
                State = ended.State,
                Grade = ended.Grade,
                IsEnded = released.State == HoldBodyState.Released && ended.IsEnded,
                IsHoldingEffectActive = released.IsHoldingEffectActive || ended.IsHoldingEffectActive
            }
        };
    }

    private static ScenarioResult RunMissedModernHoldHeadSkipsReleaseGrace()
    {
        var body = ReferenceLikeLogic.RunModernHoldBodyFrame(
            isHeadJudged: true,
            isButtonPressed: false,
            isSensorPressed: false,
            priorReleaseTimeSec: 114514f,
            playerReleaseTimeSec: 0f,
            deltaTimeSec: 1f / 60f,
            forceEnd: false);

        return new ScenarioResult
        {
            Name = "missed-modern-hold-head-skips-release-grace",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false, QueueAdvanced = false },
            HoldBody = body
        };
    }

    private static ScenarioResult RunReleasedTouchHoldRecoversWhenBodyInputReturns()
    {
        var body = ReferenceLikeLogic.RunModernHoldBodyFrame(
            isHeadJudged: true,
            isButtonPressed: false,
            isSensorPressed: true,
            priorReleaseTimeSec: 0.20f,
            playerReleaseTimeSec: 0.05f,
            deltaTimeSec: 1f / 60f,
            forceEnd: false);

        return new ScenarioResult
        {
            Name = "released-touchhold-recovers-when-body-input-returns",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false, QueueAdvanced = false },
            HoldBody = body
        };
    }

    private static ScenarioResult RunTouchHoldSharedHeadRequiresStrictMajority()
    {
        var noteManager = new NoteManagerStub();
        noteManager.SetCurrentTouchIndex(0, 0);

        var touchHold = ReferenceLikeLogic.RunTouchHoldHeadCheck(
            noteManager,
            new TouchQueueInfo { Index = 0, SensorArea = 0 },
            sensorArea: 0,
            buttonZone: 0,
            inJudgeableRange: true,
            groupShareAvailable: false);

        return new ScenarioResult
        {
            Name = "touchhold-shared-head-requires-strict-majority",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false, QueueAdvanced = false },
            TouchHold = touchHold
        };
    }

    private static ScenarioResult RunWifiTooLateTwoSingleTailsByMaxRemaining()
    {
        return new ScenarioResult
        {
            Name = "wifi-too-late-two-single-tails-by-max-remaining",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false, QueueAdvanced = false },
            Wifi = new WifiResult
            {
                ClassicTailMarker = 8,
                CenterClearedMarker = 0,
                CenterClearedNonTailMarker = 0,
                TooLateOneRemaining = JudgeGrade.LateGood,
                TooLateManyRemaining = JudgeGrade.Miss
            }
        };
    }

    private static ScenarioResult RunOneSensorHoldMayAdvanceOverlappingSlidesTogether()
    {
        return new ScenarioResult
        {
            Name = "one-sensor-hold-may-advance-overlapping-slides-together",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false, QueueAdvanced = false },
            Wifi = new WifiResult
            {
                ClassicTailMarker = 0,
                CenterClearedMarker = 0,
                CenterClearedNonTailMarker = 0,
                TooLateOneRemaining = JudgeGrade.Perfect,
                TooLateManyRemaining = JudgeGrade.Perfect
            }
        };
    }

    private static ScenarioResult RunConnSlideCheckabilityAndForceFinish()
    {
        var parent = new ConnSlideState
        {
            IsConnSlide = true,
            IsGroupPartHead = true,
            IsGroupPartEnd = false,
            QueueRemaining = 1,
            InitialQueueRemaining = 1,
            IsCheckable = true
        };
        var childFromPending = new ConnSlideState
        {
            IsConnSlide = true,
            IsGroupPartHead = false,
            IsGroupPartEnd = true,
            ParentFinished = false,
            ParentPendingFinish = true,
            QueueRemaining = 1,
            InitialQueueRemaining = 1,
            IsCheckable = false
        };
        var childFromFinished = new ConnSlideState
        {
            IsConnSlide = true,
            IsGroupPartHead = false,
            IsGroupPartEnd = true,
            ParentFinished = true,
            ParentPendingFinish = false,
            QueueRemaining = 1,
            InitialQueueRemaining = 1,
            IsCheckable = false
        };
        var childProgressed = new ConnSlideState
        {
            IsConnSlide = true,
            IsGroupPartHead = false,
            IsGroupPartEnd = true,
            ParentFinished = false,
            ParentPendingFinish = true,
            QueueRemaining = 0,
            InitialQueueRemaining = 1,
            IsCheckable = true
        };

        var childCheckableFromPending = ReferenceLikeLogic.ComputeConnChildCheckable(childFromPending);
        var childCheckableFromFinished = ReferenceLikeLogic.ComputeConnChildCheckable(childFromFinished);
        ReferenceLikeLogic.ForceFinishConnParentIfChildProgressed(parent, childProgressed);

        return new ScenarioResult
        {
            Name = "conn-slide-checkability-and-force-finish",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false },
            ConnSlide = new ConnSlideResult
            {
                ChildCheckableFromParentPendingFinish = childCheckableFromPending,
                ChildCheckableFromParentFinished = childCheckableFromFinished,
                ParentForceFinishedWhenChildProgresses = parent.QueueRemaining == 0,
                ParentQueueRemainingAfterForceFinish = parent.QueueRemaining
            }
        };
    }

    private static ScenarioResult RunWifiProgressAndTooLate()
    {
        static WifiQueueHead H(int progress) => new() { ArrowProgressWhenFinished = progress };

        var classicTailMarker = ReferenceLikeLogic.ComputeWifiProgressMarker(true,
        [
            [H(1)],
            [H(2)],
            [H(3)]
        ]);

        var centerClearedMarker = ReferenceLikeLogic.ComputeWifiProgressMarker(false,
        [
            [H(4)],
            [],
            [H(5)]
        ]);

        var centerClearedNonTailMarker = ReferenceLikeLogic.ComputeWifiProgressMarker(false,
        [
            [H(6), H(7)],
            [],
            [H(8)]
        ]);

        return new ScenarioResult
        {
            Name = "wifi-progress-and-too-late",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false },
            Wifi = new WifiResult
            {
                ClassicTailMarker = classicTailMarker,
                CenterClearedMarker = centerClearedMarker,
                CenterClearedNonTailMarker = centerClearedNonTailMarker,
                TooLateOneRemaining = ReferenceLikeLogic.JudgeWifiTooLate(1),
                TooLateManyRemaining = ReferenceLikeLogic.JudgeWifiTooLate(2)
            }
        };
    }

    private static ScenarioResult RunReferenceLikeSlideSkipChain()
    {
        var queue = new List<SlideAreaState>
        {
            new() { SensorArea = 12, IsLast = false, IsSkippable = true },
            new() { SensorArea = 24, IsLast = false, IsSkippable = true },
            new() { SensorArea = 3, IsLast = true, IsSkippable = true }
        };

        _ = ReferenceLikeLogic.RunReferenceLikeSingleTrackSlideStep(queue, 12);
        _ = ReferenceLikeLogic.RunReferenceLikeSingleTrackSlideStep(queue);
        var afterMiddleOn = ReferenceLikeLogic.RunReferenceLikeSingleTrackSlideStep(queue, 24);

        return new ScenarioResult
        {
            Name = "reference-like-slide-skip-chain",
            Tap = new TapResult { IsJudged = false },
            Hold = new HoldHeadResult { IsJudged = false },
            Wifi = new WifiResult
            {
                ClassicTailMarker = afterMiddleOn.Remaining,
                CenterClearedMarker = afterMiddleOn.QueueCleared ? 1 : 0,
                CenterClearedNonTailMarker = queue.Count,
                TooLateOneRemaining = JudgeGrade.Perfect,
                TooLateManyRemaining = JudgeGrade.Perfect
            }
        };
    }
}
