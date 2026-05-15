namespace MajdataHarness;

public static class ReferenceLikeLogic
{
    private const float DeluxeHoldReleaseIgnoreTimeSec = 0.15f;

    public static int ComputeWifiProgressMarker(bool isClassic, List<List<WifiQueueHead>> queues)
    {
        if (queues.Count == 0 || queues.All(q => q.Count == 0))
            return int.MaxValue;

        if (isClassic)
        {
            var isRemainingOne = queues.All(queue => queue.Count <= 1);
            if (isRemainingOne)
                return 8;
        }
        else if (queues.Count >= 3 && queues[1].Count == 0)
        {
            if (queues[0].Count <= 1 && queues[2].Count <= 1)
                return 9;
        }

        var lengths = queues.Select(queue => queue.Count).ToArray();
        var max = lengths.Max();
        var index = Array.FindIndex(lengths, x => x == max);
        return queues[index][0].ArrowProgressWhenFinished;
    }

    public static JudgeGrade JudgeWifiTooLate(int queueRemaining) =>
        queueRemaining == 1 ? JudgeGrade.LateGood : JudgeGrade.Miss;

    public static bool ComputeConnChildCheckable(ConnSlideState child) =>
        child.IsCheckable ||
        (child.IsConnSlide && !child.IsGroupPartHead
            ? child.ParentFinished || child.ParentPendingFinish
            : child.IsCheckable);

    public static void ForceFinishConnParentIfChildProgressed(ConnSlideState parent, ConnSlideState child)
    {
        var shouldForceFinish =
            parent.IsConnSlide &&
            !parent.IsGroupPartEnd &&
            !child.ParentFinished &&
            child.QueueRemaining < child.InitialQueueRemaining;

        if (shouldForceFinish)
            parent.QueueRemaining = 0;
    }

    public static SlideQueueStepResult RunReferenceLikeSingleTrackSlideStep(
        List<SlideAreaState> queue,
        params int[] activeSensors)
    {
        if (queue.Count == 0)
            return new SlideQueueStepResult { Remaining = 0, QueueCleared = true };

        for (;;)
        {
            if (queue.Count == 0)
                return new SlideQueueStepResult { Remaining = 0, QueueCleared = true };

            var first = queue[0];
            if (activeSensors.Contains(first.SensorArea))
                first.WasOn = true;
            else if (first.WasOn)
                first.WasOff = true;

            if (queue.Count >= 2 && (first.IsSkippable || first.On))
            {
                var second = queue[1];
                if (activeSensors.Contains(second.SensorArea))
                    second.WasOn = true;
                else if (second.WasOn)
                    second.WasOff = true;

                if (second.IsFinished)
                {
                    queue.RemoveRange(0, 2);
                    continue;
                }
                else if (second.On)
                {
                    queue.RemoveAt(0);
                    continue;
                }
            }

            if (first.IsFinished)
            {
                queue.RemoveAt(0);
                continue;
            }

            return new SlideQueueStepResult
            {
                Remaining = queue.Count,
                QueueCleared = queue.Count == 0
            };
        }
    }

    public static TapResult RunTouchCheck(
        NoteManagerStub noteManager,
        TouchQueueInfo queueInfo,
        int buttonZone,
        int sensorArea,
        bool inJudgeableRange,
        TouchHoldHeadResult? sharedResult = null)
    {
        var result = new TapResult();

        if (sharedResult is { IsJudged: true, Grade: not null, UsedGroupShare: true })
        {
            result.IsJudged = true;
            result.Grade = sharedResult.Grade;
            noteManager.NextTouch(queueInfo.SensorArea);
            return result;
        }

        if (!inJudgeableRange || !noteManager.IsCurrentTouchJudgeable(queueInfo.SensorArea, queueInfo.Index))
            return result;

        if (noteManager.IsButtonClickedInThisFrame(buttonZone) && noteManager.TryUseButtonClickEvent(buttonZone))
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
            noteManager.NextTouch(queueInfo.SensorArea);
            return result;
        }

        if (noteManager.IsSensorClickedInThisFrame(sensorArea) && noteManager.TryUseSensorClickEvent(sensorArea))
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
            noteManager.NextTouch(queueInfo.SensorArea);
        }

        return result;
    }

    public static TapResult RunTapCheck(
        NoteManagerStub noteManager,
        TapQueueInfo queueInfo,
        int buttonZone,
        int sensorArea,
        bool inJudgeableRange)
    {
        var result = new TapResult();

        if (!inJudgeableRange || !noteManager.IsCurrentNoteJudgeable(queueInfo.KeyIndex, queueInfo.Index))
            return result;

        if (noteManager.IsButtonClickedInThisFrame(buttonZone) && noteManager.TryUseButtonClickEvent(buttonZone))
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
            return result;
        }

        if (noteManager.IsSensorClickedInThisFrame(sensorArea) && noteManager.TryUseSensorClickEvent(sensorArea))
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
        }

        return result;
    }

    public static HoldHeadResult RunHoldHeadCheck(
        NoteManagerStub noteManager,
        HoldQueueInfo queueInfo,
        int buttonZone,
        int sensorArea,
        bool inJudgeableRange)
    {
        var result = new HoldHeadResult();

        if (!inJudgeableRange || !noteManager.IsCurrentNoteJudgeable(queueInfo.KeyIndex, queueInfo.Index))
            return result;

        if (noteManager.IsButtonClickedInThisFrame(buttonZone) && noteManager.TryUseButtonClickEvent(buttonZone))
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
            result.QueueAdvanced = true;
            return result;
        }

        if (noteManager.IsSensorClickedInThisFrame(sensorArea) && noteManager.TryUseSensorClickEvent(sensorArea))
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
            result.QueueAdvanced = true;
        }

        return result;
    }

    public static TouchHoldHeadResult RunTouchHoldHeadCheck(
        NoteManagerStub noteManager,
        TouchQueueInfo queueInfo,
        int sensorArea,
        int buttonZone,
        bool inJudgeableRange,
        bool groupShareAvailable)
    {
        var result = new TouchHoldHeadResult();

        if (groupShareAvailable)
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
            result.QueueAdvanced = true;
            result.UsedGroupShare = true;
            noteManager.NextTouch(queueInfo.SensorArea);
            return result;
        }

        if (!inJudgeableRange || !noteManager.IsCurrentTouchJudgeable(queueInfo.SensorArea, queueInfo.Index))
            return result;

        if (noteManager.IsButtonClickedInThisFrame(buttonZone) && noteManager.TryUseButtonClickEvent(buttonZone))
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
            result.QueueAdvanced = true;
            noteManager.NextTouch(queueInfo.SensorArea);
            return result;
        }

        if (noteManager.IsSensorClickedInThisFrame(sensorArea) && noteManager.TryUseSensorClickEvent(sensorArea))
        {
            result.IsJudged = true;
            result.Grade = JudgeGrade.Perfect;
            result.QueueAdvanced = true;
            noteManager.NextTouch(queueInfo.SensorArea);
        }

        return result;
    }

    public static HoldBodyResult RunModernHoldBodyFrame(
        bool isHeadJudged,
        bool isButtonPressed,
        bool isSensorPressed,
        float priorReleaseTimeSec,
        float playerReleaseTimeSec,
        float deltaTimeSec,
        bool forceEnd)
    {
        if (!isHeadJudged)
        {
            return new HoldBodyResult
            {
                State = HoldBodyState.HeadNotJudged,
                Grade = JudgeGrade.Miss,
                IsEnded = false,
                IsHoldingEffectActive = false
            };
        }

        if (forceEnd)
        {
            return new HoldBodyResult
            {
                State = HoldBodyState.Ended,
                Grade = playerReleaseTimeSec > 0 ? JudgeGrade.LateGood : JudgeGrade.Perfect,
                IsEnded = true,
                IsHoldingEffectActive = false
            };
        }

        var isPressed = isButtonPressed || isSensorPressed;

        if (isPressed)
        {
            return new HoldBodyResult
            {
                State = HoldBodyState.Pressed,
                Grade = JudgeGrade.Perfect,
                IsEnded = false,
                IsHoldingEffectActive = true
            };
        }

        if (priorReleaseTimeSec <= DeluxeHoldReleaseIgnoreTimeSec)
        {
            return new HoldBodyResult
            {
                State = HoldBodyState.HeadJudged,
                Grade = JudgeGrade.Perfect,
                IsEnded = false,
                IsHoldingEffectActive = false
            };
        }

        return new HoldBodyResult
        {
            State = HoldBodyState.Released,
            Grade = JudgeGrade.Perfect,
            IsEnded = false,
            IsHoldingEffectActive = false
        };
    }
}
