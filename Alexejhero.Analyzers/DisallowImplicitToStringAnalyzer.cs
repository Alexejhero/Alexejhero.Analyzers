using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Alexejhero.Analyzers;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public sealed class DisallowImplicitToStringAnalyzer : DiagnosticAnalyzer
{
    // TODO: Error on usage with stuff like Console.WriteLine, string.Format, StringBuilder.Append, etc.
    
    private static readonly DiagnosticDescriptor _rule = new(
        id: "ALEX001",
        title: "Implicit ToString() in string operation",
        messageFormat: "Type '{0}' in used string interpolation or concatenation without overriding ToString()",
        category: "Correctness",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = [_rule];

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution();

        context.RegisterSyntaxNodeAction(AnalyzeInterpolation, SyntaxKind.Interpolation);
        context.RegisterSyntaxNodeAction(AnalyzeAddExpression, SyntaxKind.AddExpression);
    }

    private static void AnalyzeInterpolation(SyntaxNodeAnalysisContext context)
    {
        InterpolationSyntax interpolation = (InterpolationSyntax)context.Node;
        ExpressionSyntax expr = interpolation.Expression;

        ITypeSymbol? type = context.SemanticModel.GetTypeInfo(expr).Type;

        if (NeedsWarning(expr, type, context.SemanticModel))
        {
            context.ReportDiagnostic(CreateDiagnostic(expr.GetLocation(), type!));
        }
    }

    private static void AnalyzeAddExpression(SyntaxNodeAnalysisContext context)
    {
        BinaryExpressionSyntax binary = (BinaryExpressionSyntax)context.Node;

        if (!binary.IsKind(SyntaxKind.AddExpression)) return;

        ExpressionSyntax leftExpr = binary.Left;
        ExpressionSyntax rightExpr = binary.Right;

        ITypeSymbol? leftType = context.SemanticModel.GetTypeInfo(leftExpr).Type;
        ITypeSymbol? rightType = context.SemanticModel.GetTypeInfo(rightExpr).Type;

        if (NeedsWarning(leftExpr, leftType, context.SemanticModel))
            context.ReportDiagnostic(CreateDiagnostic(leftExpr.GetLocation(), leftType!));

        if (NeedsWarning(rightExpr, rightType, context.SemanticModel))
            context.ReportDiagnostic(CreateDiagnostic(rightExpr.GetLocation(), rightType!));
    }

    private static bool NeedsWarning(ExpressionSyntax expr, ITypeSymbol? type, SemanticModel model)
    {
        return type != null &&
               type.SpecialType != SpecialType.System_String &&
               type.TypeKind != TypeKind.Error &&
               !IsExplicitToStringCall(expr, model) &&
               !DefinesCustomToString(type);
    }

    private static bool IsExplicitToStringCall(ExpressionSyntax expr, SemanticModel model)
    {
        if (expr is not InvocationExpressionSyntax
            {
                Expression: MemberAccessExpressionSyntax
                {
                    Name.Identifier.Text: nameof(ToString)
                }
            } invocation)
            return false;

        IMethodSymbol? methodSymbol = model.GetSymbolInfo(invocation).Symbol as IMethodSymbol;
        return methodSymbol is
        {
            Parameters.Length: 0,
            ReturnType.SpecialType: SpecialType.System_String
        };
    }

    private static bool DefinesCustomToString(ITypeSymbol type)
    {
        return type.GetMembers(nameof(ToString))
                   .OfType<IMethodSymbol>()
                   .Any(m => m is
                   {
                       Parameters.Length: 0,
                       DeclaredAccessibility: Accessibility.Public,
                       ReturnType.SpecialType: SpecialType.System_String,
                       IsOverride: true
                   }) ||
               (type.BaseType is { SpecialType: not SpecialType.System_Object } baseType && DefinesCustomToString(baseType));
    }

    private static Diagnostic CreateDiagnostic(Location location, ITypeSymbol type)
    {
        return Diagnostic.Create(_rule, location, type.ToDisplayString());
    }
}