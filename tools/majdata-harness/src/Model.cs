namespace MajdataHarness;

public enum SwitchStatus
{
    Off,
    On
}

public enum JudgeGrade
{
    Perfect,
    LateGood,
    Miss
}

public sealed class NoteManagerStub
{
    private readonly bool[] _buttonClickedInThisFrame = new bool[8];
    private readonly bool[] _buttonUsedInThisFrame = new bool[8];
    private readonly int[] _buttonClickedCountInThisFrame = new int[8];
    private readonly int[] _buttonUsedCountInThisFrame = new int[8];
    private readonly bool[] _sensorClickedInThisFrame = new bool[33];
    private readonly bool[] _sensorUsedInThisFrame = new bool[33];
    private readonly int[] _sensorClickedCountInThisFrame = new int[33];
    private readonly int[] _sensorUsedCountInThisFrame = new int[33];
    private readonly SwitchStatus[] _buttonStatusInThisFrame = new SwitchStatus[8];
    private readonly SwitchStatus[] _sensorStatusInThisFrame = new SwitchStatus[33];
    private readonly int[] _noteCurrentIndex = new int[8];
    private readonly int[] _touchCurrentIndex = new int[33];

    public void SetButtonClicked(int zone) => _buttonClickedInThisFrame[zone] = true;
    public void SetButtonClickedCount(int zone, int count) => _buttonClickedCountInThisFrame[zone] = count;
    public void SetSensorClicked(int area) => _sensorClickedInThisFrame[area] = true;
    public void SetSensorClickedCount(int area, int count) => _sensorClickedCountInThisFrame[area] = count;
    public void SetButtonStatus(int zone, SwitchStatus status) => _buttonStatusInThisFrame[zone] = status;
    public void SetSensorStatus(int area, SwitchStatus status) => _sensorStatusInThisFrame[area] = status;
    public void SetCurrentTapIndex(int keyIndexZeroBased, int currentIndex) => _noteCurrentIndex[keyIndexZeroBased] = currentIndex;
    public void SetCurrentTouchIndex(int sensorAreaZeroBased, int currentIndex) => _touchCurrentIndex[sensorAreaZeroBased] = currentIndex;

    public bool IsCurrentNoteJudgeable(int keyIndexOneBased, int noteIndex) =>
        noteIndex <= _noteCurrentIndex[keyIndexOneBased - 1];

    public bool IsCurrentTouchJudgeable(int sensorAreaZeroBased, int noteIndex) =>
        noteIndex <= _touchCurrentIndex[sensorAreaZeroBased];

    public void NextTouch(int sensorAreaZeroBased) => _touchCurrentIndex[sensorAreaZeroBased]++;

    public bool IsButtonClickedInThisFrame(int zone) => _buttonClickedInThisFrame[zone];
    public bool IsSensorClickedInThisFrame(int area) => _sensorClickedInThisFrame[area];
    public bool TryUseButtonClickEvent(int zone)
    {
        if (_buttonClickedCountInThisFrame[zone] > 0)
        {
            if (_buttonUsedCountInThisFrame[zone] >= _buttonClickedCountInThisFrame[zone])
                return false;

            _buttonUsedCountInThisFrame[zone]++;
            return true;
        }
        if (_buttonUsedInThisFrame[zone])
            return false;
        _buttonUsedInThisFrame[zone] = true;
        return _buttonClickedInThisFrame[zone];
    }

    public bool TryUseSensorClickEvent(int area)
    {
        if (_sensorClickedCountInThisFrame[area] > 0)
        {
            if (_sensorUsedCountInThisFrame[area] >= _sensorClickedCountInThisFrame[area])
                return false;

            _sensorUsedCountInThisFrame[area]++;
            return true;
        }
        if (_sensorUsedInThisFrame[area])
            return false;
        _sensorUsedInThisFrame[area] = true;
        return _sensorClickedInThisFrame[area];
    }

    public bool CheckButtonStatusInThisFrame(int zone, SwitchStatus target) => _buttonStatusInThisFrame[zone] == target;
    public bool CheckSensorStatusInThisFrame(int area, SwitchStatus target) => _sensorStatusInThisFrame[area] == target;
}

public sealed class TapQueueInfo
{
    public required int Index { get; init; }
    public required int KeyIndex { get; init; }
}

public sealed class HoldQueueInfo
{
    public required int Index { get; init; }
    public required int KeyIndex { get; init; }
}

public sealed class TouchQueueInfo
{
    public required int Index { get; init; }
    public required int SensorArea { get; init; }
}

public sealed class TapResult
{
    public bool IsJudged { get; set; }
    public JudgeGrade? Grade { get; set; }
}

public sealed class HoldHeadResult
{
    public bool IsJudged { get; set; }
    public JudgeGrade? Grade { get; set; }
    public bool QueueAdvanced { get; set; }
}

public enum HoldBodyState
{
    HeadNotJudged,
    HeadJudged,
    Pressed,
    Released,
    Ended
}

public sealed class HoldBodyResult
{
    public required HoldBodyState State { get; init; }
    public required JudgeGrade Grade { get; init; }
    public required bool IsEnded { get; init; }
    public required bool IsHoldingEffectActive { get; init; }
}

public sealed class TouchHoldHeadResult
{
    public bool IsJudged { get; set; }
    public JudgeGrade? Grade { get; set; }
    public bool QueueAdvanced { get; set; }
    public bool UsedGroupShare { get; set; }
}

public sealed class ScenarioResult
{
    public required string Name { get; init; }
    public required TapResult Tap { get; init; }
    public required HoldHeadResult Hold { get; init; }
    public TapResult? Touch { get; init; }
    public TouchHoldHeadResult? TouchHold { get; init; }
    public HoldBodyResult? HoldBody { get; init; }
    public ConnSlideResult? ConnSlide { get; init; }
    public WifiResult? Wifi { get; init; }

    public string Format() =>
        $"tap: judged={Tap.IsJudged}, grade={Tap.Grade?.ToString() ?? "none"}\n" +
        $"holdHead: judged={Hold.IsJudged}, grade={Hold.Grade?.ToString() ?? "none"}, queueAdvanced={Hold.QueueAdvanced}" +
        (Touch is null ? string.Empty : $"\ntouch: judged={Touch.IsJudged}, grade={Touch.Grade?.ToString() ?? "none"}") +
        (TouchHold is null ? string.Empty : $"\ntouchHoldHead: judged={TouchHold.IsJudged}, grade={TouchHold.Grade?.ToString() ?? "none"}, queueAdvanced={TouchHold.QueueAdvanced}, usedGroupShare={TouchHold.UsedGroupShare}") +
        (HoldBody is null ? string.Empty : $"\nholdBody: state={HoldBody.State}, grade={HoldBody.Grade}, ended={HoldBody.IsEnded}, holdingEffectActive={HoldBody.IsHoldingEffectActive}") +
        (ConnSlide is null ? string.Empty : $"\n{ConnSlide.Format()}") +
        (Wifi is null ? string.Empty : $"\n{Wifi.Format()}");
}

public sealed class ConnSlideState
{
    public required bool IsConnSlide { get; init; }
    public required bool IsGroupPartHead { get; init; }
    public required bool IsGroupPartEnd { get; init; }
    public bool ParentFinished { get; set; }
    public bool ParentPendingFinish { get; set; }
    public required int QueueRemaining { get; set; }
    public required int InitialQueueRemaining { get; init; }
    public bool IsCheckable { get; set; }
}

public sealed class ConnSlideResult
{
    public required bool ChildCheckableFromParentPendingFinish { get; init; }
    public required bool ChildCheckableFromParentFinished { get; init; }
    public required bool ParentForceFinishedWhenChildProgresses { get; init; }
    public required int ParentQueueRemainingAfterForceFinish { get; init; }

    public string Format() =>
        $"connSlide: childCheckableFromPending={ChildCheckableFromParentPendingFinish}, childCheckableFromFinished={ChildCheckableFromParentFinished}, " +
        $"parentForceFinishedWhenChildProgresses={ParentForceFinishedWhenChildProgresses}, parentQueueRemainingAfterForceFinish={ParentQueueRemainingAfterForceFinish}";
}

public sealed class WifiQueueHead
{
    public required int ArrowProgressWhenFinished { get; init; }
}

public sealed class SlideAreaState
{
    public required int SensorArea { get; init; }
    public required bool IsLast { get; init; }
    public required bool IsSkippable { get; init; }
    public bool WasOn { get; set; }
    public bool WasOff { get; set; }

    public bool On => WasOn;
    public bool IsFinished => IsLast ? WasOn : WasOn && WasOff;
}

public sealed class SlideQueueStepResult
{
    public required int Remaining { get; init; }
    public required bool QueueCleared { get; init; }
}

public sealed class WifiResult
{
    public required int ClassicTailMarker { get; init; }
    public required int CenterClearedMarker { get; init; }
    public required int CenterClearedNonTailMarker { get; init; }
    public required JudgeGrade TooLateOneRemaining { get; init; }
    public required JudgeGrade TooLateManyRemaining { get; init; }

    public string Format() =>
        $"wifi: classicTailMarker={ClassicTailMarker}, centerClearedMarker={CenterClearedMarker}, centerClearedNonTailMarker={CenterClearedNonTailMarker}, " +
        $"tooLateOneRemaining={TooLateOneRemaining}, tooLateManyRemaining={TooLateManyRemaining}";
}
