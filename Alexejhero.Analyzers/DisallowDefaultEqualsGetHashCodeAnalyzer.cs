using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Alexejhero.Analyzers;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class DisallowDefaultEqualsGetHashCodeAnalyzer : DiagnosticAnalyzer
{
    // TODO: Error on usage with stuff like Dictionary, ConcurrentDictionary, ImmutableDictionary, SortedDictionary
    // HashSet, ImmutableHashSet, Hashtable, Lookup, HashCode.Combine, Distinct, GroupBy, Join, GroupJoin, 
    
    private static readonly DiagnosticDescriptor _rule = new(
        id: "ALEX002",
        title: "Call to default Equals or GetHashCode",
        messageFormat: "Cannot call method '{0}' on type '{1}' which does not override it",
        category: "Correctness",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = new() {_rule};

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution();

        context.RegisterSyntaxNodeAction(AnalyzeInvocation, SyntaxKind.InvocationExpression);
    }

    private static void AnalyzeInvocation(SyntaxNodeAnalysisContext context)
    {
        InvocationExpressionSyntax invocation = (InvocationExpressionSyntax) context.Node;

        string methodName;
        ExpressionSyntax receiverExpr = null!;
        Location location;

        // invocation.Expression can be:
        // - MemberAccessExpressionSyntax (foo.Equals())
        // - MemberBindingExpressionSyntax (Equals() in foo?.Equals())
        switch (invocation.Expression)
        {
            case MemberAccessExpressionSyntax ma:
                methodName = ma.Name.Identifier.Text;
                receiverExpr = ma.Expression;
                location = ma.Name.GetLocation();
                break;

            case MemberBindingExpressionSyntax mbe:
                methodName = mbe.Name.Identifier.Text;
                location = mbe.Name.GetLocation();
                
                // receiver is in the parent ConditionalAccessExpression
                if (invocation.Parent is ConditionalAccessExpressionSyntax conditionalAccess)
                {
                    receiverExpr = conditionalAccess.Expression;
                }
                break;

            default:
                return; // not a call we care about
        }

        if (methodName is not (nameof(Equals) or nameof(GetHashCode)))
            return;

        IMethodSymbol? symbol = context.SemanticModel.GetSymbolInfo(invocation).Symbol as IMethodSymbol;
        if (symbol == null)
            return;

        // Allow Equals(null)
        if (methodName == nameof(Equals) &&
            invocation.ArgumentList.Arguments.Count == 1 &&
            invocation.ArgumentList.Arguments[0].Expression.IsKind(SyntaxKind.NullLiteralExpression))
        {
            return;
        }

        if (symbol.ContainingType?.SpecialType == SpecialType.System_Object)
        {
            ITypeSymbol? type = context.SemanticModel.GetTypeInfo(receiverExpr).Type;

            if (type == null ||
                type.TypeKind == TypeKind.Error ||
                type.SpecialType == SpecialType.System_Object ||
                type.TypeKind == TypeKind.TypeParameter ||
                type.IsValueType ||
                type.SpecialType == SpecialType.System_String)
                return;

            if (!DefinesCustomOverride(type, methodName))
            {
                context.ReportDiagnostic(Diagnostic.Create(_rule, location, methodName, type.ToDisplayString()));
            }
        }
    }

    private static bool DefinesCustomOverride(ITypeSymbol type, string methodName)
    {
        return type.GetMembers(methodName)
                   .OfType<IMethodSymbol>()
                   .Any(m =>
                       m.DeclaredAccessibility == Accessibility.Public &&
                       m.IsOverride &&
                       m.Parameters.Length == (methodName == nameof(Equals) ? 1 : 0) &&
                       m.ReturnType.SpecialType == (methodName == nameof(Equals) ? SpecialType.System_Boolean : SpecialType.System_Int32))
               || (type.BaseType is
               {
                   SpecialType: not SpecialType.System_Object
               } baseType && DefinesCustomOverride(baseType, methodName));
    }
}