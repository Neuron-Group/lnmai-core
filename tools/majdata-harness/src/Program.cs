using MajdataHarness;

var scenarios = ScenarioLibrary.All();

foreach (var scenario in scenarios)
{
    var result = scenario.Run();
    Console.WriteLine($"[{scenario.Name}]");
    Console.WriteLine(result.Format());
    Console.WriteLine();
}
