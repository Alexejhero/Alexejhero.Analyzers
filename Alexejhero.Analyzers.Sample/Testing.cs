namespace Alexejhero.Analyzers.Sample;

// If you don't see warnings, build the Analyzers Project.

public class Instance;

public static class Testing
{
    public static void Test()
    {
        Instance t = new();
        Instance t2 = t;

        string myStr1 = $"Hello from {t}"; // ALEX001 implicit ToString call
        string myStr2 = "Hello from " + t; // ALEX001 implicit ToString call
        
        bool equalsM = t.Equals(t2); // ALEX002 usage of Equals method on reference type
        bool equalsO = t == t2; // ALEX003 usage of equals operator on reference type
    }
}