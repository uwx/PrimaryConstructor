using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ISymbol = Microsoft.CodeAnalysis.ISymbol;

namespace PrimaryConstructor;

public readonly record struct MemberSymbolInfo(
    string Type,
    string ParameterName,
    string Name,
    ImmutableArray<AttributeData> Attributes
);

public readonly record struct ConstructorSymbolInfo(
    MemberSymbolInfo[] Parameters
);
    

[Generator]
internal class PrimaryConstructorGenerator : IIncrementalGenerator
{
    private static readonly string AttributeName = nameof(PrimaryConstructorAttribute).Replace("Attribute", "");
    
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
#if DEBUG
        // SpinWait.SpinUntil(() => Debugger.IsAttached);
#endif

        var sources = context.SyntaxProvider
            .CreateSyntaxProvider(IsCandidate, Transform)
            .Where(static s => s != null)
            .WithComparer(SymbolEqualityComparer.IncludeNullability);

        context.RegisterSourceOutput(sources, GenerateCode);
    }

    private static void GenerateCode(SourceProductionContext ctx, INamedTypeSymbol? classSymbol)
    {
        ctx.AddSource(
            $"{classSymbol!.ToDisplayString(FileNameFormat)}-{Guid.NewGuid()}.g.cs",
            CreatePrimaryConstructor(classSymbol)
        );
    }

    private static bool IsCandidate(SyntaxNode node, CancellationToken cancellationToken)
    {
        if (node is not (TypeDeclarationSyntax typeDeclaration and (ClassDeclarationSyntax or StructDeclarationSyntax)) || cancellationToken.IsCancellationRequested)
        {
            return false;
        }

        return typeDeclaration.AttributeLists
            .SelectMany(static l => l.Attributes)
            .Any(static a => a.IsNamed(AttributeName));
    }
        
    private static INamedTypeSymbol? Transform(GeneratorSyntaxContext context, CancellationToken cancellationToken)
    {
        var typeDeclaration = (TypeDeclarationSyntax)context.Node;
        if (cancellationToken.IsCancellationRequested || !typeDeclaration.ContainsAttribute(context.SemanticModel, GetAttributeSymbol(context.SemanticModel)))
        {
            return null;
        }
            
        var classSymbol = context.SemanticModel.GetDeclaredSymbol(typeDeclaration)!;
            
        return classSymbol;
    }

    private static INamedTypeSymbol GetAttributeSymbol(SemanticModel model)
    {
        return model.Compilation.GetSymbolByType<PrimaryConstructorAttribute>();
    }

    private static readonly SymbolDisplayFormat TypeFormat = new(
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters |
                         SymbolDisplayGenericsOptions.IncludeTypeConstraints,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes |
                              SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
                              SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier
    );

    private static readonly SymbolDisplayFormat FileNameFormat = new(
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Omitted,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.None,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.None
    );

    private static string CreatePrimaryConstructor(INamedTypeSymbol classSymbol)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();

        var baseClassConstructorArgs = classSymbol.GetBaseTypeGenerationMembers().ToArray();
        var baseConstructorInheritance = baseClassConstructorArgs.Length > 0
            ? $" : base({string.Join(", ", baseClassConstructorArgs.Select(static m => m.ParameterName))})"
            : "";

        var memberList = classSymbol.GetGenerationMembers().ToArray();
        var arguments = (baseClassConstructorArgs.Length == 0 ? memberList : memberList.Concat(baseClassConstructorArgs))
            .Select(static m => $"{m.Type} {m.ParameterName}");

        var source = new IndentedStringBuilder(' ', 4);

        source.AppendLine($"namespace {namespaceName};");

        var ct = classSymbol.ContainingType;
        while (ct != null)
        {
            var ctFullTypeName = classSymbol.ToDisplayString(TypeFormat);
            var ctGeneric = ctFullTypeName.IndexOf('<') < 0
                ? ""
                : ctFullTypeName.Substring(ctFullTypeName.IndexOf('<'));

            source.AppendLine($"partial {(ct.IsStructType() ? "struct" : "class")} {ct.Name}{ctGeneric}");
            source.AppendLine("{");
            source.IncreaseIndent();
            ct = ct.ContainingType;
        }

        var fullTypeName = classSymbol.ToDisplayString(TypeFormat);
        var generic = fullTypeName.IndexOf('<') < 0
            ? ""
            : fullTypeName.Substring(fullTypeName.IndexOf('<'));

        source.AppendLine($"partial {(classSymbol.IsStructType() ? "struct" : "class")} {classSymbol.Name}{generic}");
        source.AppendLine("{");
        using (source.IncreaseIndent())
        {
            source.AppendLine($"public {classSymbol.Name}({string.Join(", ", arguments)}){baseConstructorInheritance}");
            source.AppendLine("{");
            using (source.IncreaseIndent())
            {
                foreach (var item in memberList)
                {
                    source.AppendLine($@"this.{item.Name} = {item.ParameterName};");
                }
                source.AppendLine("this.Constructor();");
            }
            source.AppendLine("}");
            source.AppendLine("partial void Constructor();");
        }
        source.AppendLine("}");
        
        ct = classSymbol.ContainingType;
        while (ct != null)
        {
            source.DecreaseIndent();
            source.AppendLine("}");
            ct = ct.ContainingType;
        }

        return source.ToString();
    }
}

internal static class RosylnExtensions
{
    private static readonly SymbolDisplayFormat PropertyTypeFormat = new(
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes |
                              SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
                              SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier
    );

    public static bool HasFieldInitializer(this IFieldSymbol symbol)
    {
        var field = symbol.DeclaringSyntaxReferences.ElementAtOrDefault(0)?.GetSyntax() as VariableDeclaratorSyntax;
        return field?.Initializer != null;
    }

    public static bool HasPropertyInitializer(this IPropertySymbol symbol)
    {
        var property = symbol.DeclaringSyntaxReferences.ElementAtOrDefault(0)?.GetSyntax() as PropertyDeclarationSyntax;
        return property?.Initializer != null;
    }

    public static bool HasAttribute(this ISymbol symbol, string name) => symbol
        .GetAttributes()
        .Any(x => x.AttributeClass?.Name == name);

    public static bool IsAutoProperty(this IPropertySymbol propertySymbol)
    {
        // Get fields declared in the same type as the property
        var fields = propertySymbol.ContainingType.GetMembers().OfType<IFieldSymbol>();

        // Check if one field is associated to
        return fields.Any(field => !field.CanBeReferencedByName && SymbolEqualityComparer.Default.Equals(field.AssociatedSymbol, propertySymbol));
    }

    public static IEnumerable<MemberSymbolInfo> GetGenerationMembers(this INamedTypeSymbol classSymbol)
    {
        const string includePrimaryConstructor = nameof(IncludePrimaryConstructorAttribute);
        const string ignorePrimaryConstructor = nameof(IgnorePrimaryConstructorAttribute);
        
        foreach (var symbol in classSymbol.GetMembers())
        {
            if (!symbol.CanBeReferencedByName || symbol.IsStatic) continue;

            if (symbol is IFieldSymbol field)
            {
                if ((!field.IsReadOnly || field.HasFieldInitializer()) && !field.HasAttribute(includePrimaryConstructor)) continue;
                if (field.HasAttribute(ignorePrimaryConstructor)) continue;
    
                yield return new MemberSymbolInfo
                {
                    Type = field.Type.ToDisplayString(PropertyTypeFormat),
                    ParameterName = field.Name.ToCamelCase(),
                    Name = field.Name,
                    Attributes = field.GetAttributes()
                };
            }
            else if (symbol is IPropertySymbol prop)
            {
                if ((!prop.IsReadOnly || prop.HasPropertyInitializer() || !prop.IsAutoProperty()) && !prop.HasAttribute(includePrimaryConstructor)) continue;
                if (prop.HasAttribute(ignorePrimaryConstructor)) continue;
                
                yield return new MemberSymbolInfo
                {
                    Type = prop.Type.ToDisplayString(PropertyTypeFormat),
                    ParameterName = prop.Name.ToCamelCase(),
                    Name = prop.Name,
                    Attributes = prop.GetAttributes()
                };
            }
        }
    }

    public static IEnumerable<MemberSymbolInfo> GetBaseTypeGenerationMembers(this INamedTypeSymbol? classSymbol)
    {
        while ((classSymbol = classSymbol?.BaseType) != null)
        {
            if (classSymbol.Name is nameof(Object) or nameof(ValueType))
            {
                yield break;
            }

            if (classSymbol.HasAttribute(nameof(PrimaryConstructorAttribute)))
            {
                foreach (var member in classSymbol.GetGenerationMembers())
                {
                    yield return member;
                }
            }
            else if (classSymbol.GetRelevantConstructor() is { } relevantConstructor &&
                     relevantConstructor.Parameters.Length > 0)
            {
                foreach (var parameter in relevantConstructor.Parameters)
                {
                    yield return parameter;
                }
            }
        }
    }

    public static ConstructorSymbolInfo? GetRelevantConstructor(this INamedTypeSymbol classSymbol)
    {
        var members = classSymbol.GetMembers()
            .OfType<IMethodSymbol>()
            .Where(static e => e.MethodKind == MethodKind.Constructor);

        // ReSharper disable once PossibleMultipleEnumeration
        var bestConstructor = members
            .FirstOrDefault(static e => e.HasAttribute(nameof(UseForPrimaryConstructorAttribute)));

        if (bestConstructor == null)
        {
            // ReSharper disable once PossibleMultipleEnumeration
            bestConstructor = members
                .OrderByDescending(static e => e.Parameters.Length)
                .FirstOrDefault();
        }

        if (bestConstructor == null)
        {
            return null;
        }

        return new ConstructorSymbolInfo
        {
            Parameters = bestConstructor
                .Parameters
                .Select(static (e, i) => new MemberSymbolInfo
                {
                    Type = e.Type.ToDisplayString(PropertyTypeFormat),
                    ParameterName = string.IsNullOrWhiteSpace(e.Name) ? $"__param{i}" : e.Name,
                    Name = string.IsNullOrWhiteSpace(e.Name) ? $"Param{i}" : e.Name,
                    Attributes = ImmutableArray<AttributeData>.Empty
                })
                .ToArray()
        };
    }

    public static string ToCamelCase(this string name)
    {
        name = name.TrimStart('_');
        return name.Length switch
        {
            0 => "_noName_" + Guid.NewGuid().ToString().Replace("-", ""),
            1 => name.ToLowerInvariant(),
            _ => char.ToLowerInvariant(name[0]) + name.Substring(1)
        };
    }
}
