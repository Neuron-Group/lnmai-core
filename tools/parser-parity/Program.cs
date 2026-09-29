using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using MajSimai;

while (Console.ReadLine() is string line)
{
    try
    {
        var request = JsonDocument.Parse(line).RootElement;
        var content = request.GetProperty("content").GetString()!;
        var level = request.TryGetProperty("levelIndex", out var l) ? l.GetInt32() : 1;
        var file = SimaiParser.Parse(content, "oracle");
        var notes = new List<object>();
        foreach (var timing in file.Charts[level - 1].NoteTimings)
            foreach (var note in timing.Notes)
                notes.Add(new { timing = timing.Timing + file.Offset, bpm = timing.Bpm,
                    hSpeed = timing.HSpeed, note });
        Console.WriteLine(JsonSerializer.Serialize(new { ok = true, notes, file.Title,
            file.Artist, file.Offset }));
    }
    catch (Exception e)
    {
        Console.WriteLine(JsonSerializer.Serialize(new { ok = false, error = e.ToString() }));
    }
}
