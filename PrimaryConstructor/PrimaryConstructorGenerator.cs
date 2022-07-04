using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SmartAnalyzers.CSharpExtensions.Annotations;
using ISymbol = Microsoft.CodeAnalysis.ISymbol;

namespace PrimaryConstructor;

public readonly record struct MemberSymbolInfo(
    string Type,
    string ParameterName,
    string Name,
    IEnumerable<AttributeData> Attributes
);

public readonly record struct ConstructorSymbolInfo(
    MemberSymbolInfo[] Parameters
);
    

[Generator]
internal class PrimaryConstructorGenerator : IIncrementalGenerator
{
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
            .SelectMany(l => l.Attributes)
            .Any(a => a.IsNamed("PrimaryConstructor"));
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

        var baseClassConstructorArgs = classSymbol.GetBaseTypeGenerationMembers();
        var baseConstructorInheritance = baseClassConstructorArgs.Count > 0
            ? $" : base({string.Join(", ", baseClassConstructorArgs.Select(it => it.ParameterName))})"
            : "";

        var memberList = classSymbol.GetGenerationMembers();
        var arguments = (baseClassConstructorArgs.Count == 0 ? memberList : memberList.Concat(baseClassConstructorArgs))
            .Select(it => $"{it.Type} {it.ParameterName}");

        var source = new IndentedStringBuilder
        {
            IndentationString = "    "
        };

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

    public static IReadOnlyList<MemberSymbolInfo> GetGenerationMembers(this INamedTypeSymbol classSymbol)
    {
        var members = classSymbol.GetMembers();

        var fields = 
            from x in members.OfType<IFieldSymbol>()
            where
                x.CanBeReferencedByName && !x.IsStatic &&
                (x.IsReadOnly && !x.HasFieldInitializer() || x.HasAttribute(nameof(IncludePrimaryConstructorAttribute))) && 
                !x.HasAttribute(nameof(IgnorePrimaryConstructorAttribute))
            select new MemberSymbolInfo
            {
                Type = x.Type.ToDisplayString(PropertyTypeFormat),
                ParameterName = x.Name.ToCamelCase(),
                Name = x.Name,
                Attributes = x.GetAttributes()
            };

        var props = 
            from x in members.OfType<IPropertySymbol>()
            where
                x.CanBeReferencedByName && !x.IsStatic &&
                (x.IsReadOnly && !x.HasPropertyInitializer() && x.IsAutoProperty() || x.HasAttribute(nameof(IncludePrimaryConstructorAttribute))) &&
                !x.HasAttribute(nameof(IgnorePrimaryConstructorAttribute))
            select new MemberSymbolInfo
            {
                Type = x.Type.ToDisplayString(PropertyTypeFormat),
                ParameterName = x.Name.ToCamelCase(),
                Name = x.Name,
                Attributes = x.GetAttributes()
            };

        fields = fields.Concat(props);

        return fields.ToArray();
    }

    public static IReadOnlyList<MemberSymbolInfo> GetBaseTypeGenerationMembers(this INamedTypeSymbol? classSymbol)
    {
        var fields = Enumerable.Empty<MemberSymbolInfo>();

        while ((classSymbol = classSymbol?.BaseType) != null)
        {
            if (classSymbol.Name is nameof(Object) or nameof(ValueType))
            {
                break;
            }

            if (classSymbol.HasAttribute(nameof(PrimaryConstructorAttribute)))
            {
                fields = fields.Concat(classSymbol.GetGenerationMembers());
            }
            else if (classSymbol.GetRelevantConstructor() is { } relevantConstructor &&
                     relevantConstructor.Parameters.Length > 0)
            {
                fields = fields.Concat(relevantConstructor.Parameters);
            }
        }

        return fields.ToArray();
    }

    public static ConstructorSymbolInfo? GetRelevantConstructor(this INamedTypeSymbol classSymbol)
    {
        var members = classSymbol.GetMembers().OfType<IMethodSymbol>();

        // ReSharper disable once PossibleMultipleEnumeration
        var bestConstructor = members
            .Where(e => e.MethodKind == MethodKind.Constructor)
            .FirstOrDefault(e => e.HasAttribute(nameof(UseForPrimaryConstructorAttribute)));

        if (bestConstructor == null)
        {
            // ReSharper disable once PossibleMultipleEnumeration
            bestConstructor = members
                .Where(e => e.MethodKind == MethodKind.Constructor)
                .OrderByDescending(e => e.Parameters.Length)
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
                .Select((e, i) => new MemberSymbolInfo
                {
                    Type = e.Type.ToDisplayString(PropertyTypeFormat),
                    ParameterName = string.IsNullOrWhiteSpace(e.Name) ? $"__param{i}" : e.Name,
                    Name = string.IsNullOrWhiteSpace(e.Name) ? $"Param{i}" : e.Name,
                    Attributes = Array.Empty<AttributeData>()
                })
                .ToArray()
        };
    }

    public static string ToCamelCase(this string name)
    {
        name = name.TrimStart('_');
        if (name.Length == 0)
        {
            return "_noName_" + Guid.NewGuid().ToString().Replace("-", "");
        }

        if (name.Length == 1)
        {
            return name.ToLowerInvariant();
        }

        return char.ToLowerInvariant(name[0]) + name.Substring(1);
    }
}

// MoreLINQ - Extensions to LINQ to Objects
// Copyright (c) 2008 Jonathan Skeet. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
public static class MoreLinq
{
    // https://github.com/morelinq/MoreLINQ/blob/ee238241083a9b31dc03c39781b29f4189b4fe73/MoreLinq/DistinctBy.cs
    /// <summary>
    /// Returns all distinct elements of the given source, where "distinctness"
    /// is determined via a projection and the specified comparer for the projected type.
    /// </summary>
    /// <remarks>
    /// This operator uses deferred execution and streams the results, although
    /// a set of already-seen keys is retained. If a key is seen multiple times,
    /// only the first element with that key is returned.
    /// </remarks>
    /// <typeparam name="TSource">Type of the source sequence</typeparam>
    /// <typeparam name="TKey">Type of the projected element</typeparam>
    /// <param name="source">Source sequence</param>
    /// <param name="keySelector">Projection for determining "distinctness"</param>
    /// <param name="comparer">The equality comparer to use to determine whether or not keys are equal.
    /// If null, the default equality comparer for <c>TSource</c> is used.</param>
    /// <returns>A sequence consisting of distinct elements from the source sequence,
    /// comparing them by the specified key projection.</returns>

    public static IEnumerable<TSource> DistinctBy<TSource, TKey>(this IEnumerable<TSource> source,
        Func<TSource, TKey> keySelector, IEqualityComparer<TKey>? comparer = null)
    {
        if (source == null) throw new ArgumentNullException(nameof(source));
        if (keySelector == null) throw new ArgumentNullException(nameof(keySelector));

        return _(); IEnumerable<TSource> _()
        {
            var knownKeys = new HashSet<TKey>(comparer);
            foreach (var element in source)
            {
                if (knownKeys.Add(keySelector(element)))
                    yield return element;
            }
        }
    }
}