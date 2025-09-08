using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Alexejhero.Analyzers;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class DisallowDefaultEqualityOperatorsAnalyzer : DiagnosticAnalyzer
{
    private static readonly DiagnosticDescriptor _rule = new(
        id: "ALEX003",
        title: "Default equality operator usage",
        messageFormat: "Cannot use operator '{1}' with type '{0}' which does not overload it",
        category: "Correctness",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(_rule);

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution();

        context.RegisterSyntaxNodeAction(AnalyzeEqualityOperator, SyntaxKind.EqualsExpression, SyntaxKind.NotEqualsExpression);
    }

    private static void AnalyzeEqualityOperator(SyntaxNodeAnalysisContext context)
    {
        BinaryExpressionSyntax binary = (BinaryExpressionSyntax)context.Node;

        ITypeSymbol? leftType = context.SemanticModel.GetTypeInfo(binary.Left).Type;
        ITypeSymbol? rightType = context.SemanticModel.GetTypeInfo(binary.Right).Type;

        // Ignore if either side is null, string, error, or a primitive
        if (leftType == null || rightType == null ||
            leftType.TypeKind == TypeKind.Error || rightType.TypeKind == TypeKind.Error ||
            leftType.SpecialType == SpecialType.System_String || rightType.SpecialType == SpecialType.System_String ||
            leftType.IsValueType || rightType.IsValueType)
            return;

        SymbolInfo symbolInfo = context.SemanticModel.GetSymbolInfo(binary);
        if (symbolInfo.Symbol is IMethodSymbol methodSymbol)
        {
            // If operator comes from System.Object, it's not overridden
            if (methodSymbol.ContainingType.SpecialType == SpecialType.System_Object)
            {
                string operatorText = binary.Kind() == SyntaxKind.EqualsExpression ? "==" : "!=";
                context.ReportDiagnostic(Diagnostic.Create(_rule, binary.OperatorToken.GetLocation(), leftType.ToDisplayString(), operatorText));
            }
        }
    }
}