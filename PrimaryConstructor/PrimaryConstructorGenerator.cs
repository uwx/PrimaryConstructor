using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SmartAnalyzers.CSharpExtensions.Annotations;

namespace PrimaryConstructor
{
    public record MemberSymbolInfo
    {
        [InitRequired] public string Type { get; init; } = null!;
        [InitRequired] public string ParameterName { get; init; } = null!;
        [InitRequired] public string Name { get; init; } = null!;
        [InitRequired] public IEnumerable<AttributeData> Attributes { get; init; } = null!;
    }

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
                .Collect();

            context.RegisterSourceOutput(sources, static (context, symbols) =>
            {
                foreach (var classSymbol in symbols.DistinctBy(static e => e!.ToDisplayString()))
                {
                    GenerateCode(context, classSymbol);
                }
            });
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
                .Any();
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

        private static bool HasFieldInitializer(IFieldSymbol symbol)
        {
            var field = symbol.DeclaringSyntaxReferences.ElementAtOrDefault(0)?.GetSyntax() as VariableDeclaratorSyntax;
            return field?.Initializer != null;
        }

        private static bool HasPropertyInitializer(IPropertySymbol symbol)
        {
            var property = symbol.DeclaringSyntaxReferences.ElementAtOrDefault(0)?.GetSyntax() as PropertyDeclarationSyntax;
            return property?.Initializer != null;
        }

        private static bool HasAttribute(ISymbol symbol, string name) => symbol
            .GetAttributes()
            .Any(x => x.AttributeClass?.Name == name);

        private static readonly SymbolDisplayFormat TypeFormat = new(
            globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
            typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
            genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters |
                             SymbolDisplayGenericsOptions.IncludeTypeConstraints,
            miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes |
                                  SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
                                  SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier
        );

        private static readonly SymbolDisplayFormat PropertyTypeFormat = new(
            globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
            typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
            genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters,
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

            var baseClassConstructorArgs = classSymbol.BaseType != null && HasAttribute(classSymbol.BaseType, nameof(PrimaryConstructorAttribute))
                ? GetMembers(classSymbol.BaseType, true)
                : null;
            var baseConstructorInheritance = baseClassConstructorArgs?.Count > 0
                ? $" : base({string.Join(", ", baseClassConstructorArgs.Select(it => it.ParameterName))})"
                : "";

            var memberList = GetMembers(classSymbol, false);
            var arguments = (baseClassConstructorArgs == null ? memberList : memberList.Concat(baseClassConstructorArgs))
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
                }
                source.AppendLine("}");
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

        private static bool IsAutoProperty(IPropertySymbol propertySymbol)
        {
            // Get fields declared in the same type as the property
            var fields = propertySymbol.ContainingType.GetMembers().OfType<IFieldSymbol>();

            // Check if one field is associated to
            return fields.Any(field => !field.CanBeReferencedByName && SymbolEqualityComparer.Default.Equals(field.AssociatedSymbol, propertySymbol));
        }

        private static IReadOnlyList<MemberSymbolInfo> GetMembers(INamedTypeSymbol classSymbol, bool recursive)
        {
            var fields = 
                from x in classSymbol.GetMembers().OfType<IFieldSymbol>()
                where
                    x.CanBeReferencedByName && !x.IsStatic &&
                    (x.IsReadOnly && !HasFieldInitializer(x) || HasAttribute(x, nameof(IncludePrimaryConstructorAttribute))) && 
                    !HasAttribute(x, nameof(IgnorePrimaryConstructorAttribute))
                select new MemberSymbolInfo
                {
                    Type = x.Type.ToDisplayString(PropertyTypeFormat),
                    ParameterName = ToCamelCase(x.Name),
                    Name = x.Name,
                    Attributes = x.GetAttributes()
                };

            var props = 
                from x in classSymbol.GetMembers().OfType<IPropertySymbol>()
                where
                    x.CanBeReferencedByName && !x.IsStatic &&
                    (x.IsReadOnly && !HasPropertyInitializer(x) && IsAutoProperty(x) || HasAttribute(x, nameof(IncludePrimaryConstructorAttribute))) &&
                    !HasAttribute(x, nameof(IgnorePrimaryConstructorAttribute))
                select new MemberSymbolInfo
                {
                    Type = x.Type.ToDisplayString(PropertyTypeFormat),
                    ParameterName = ToCamelCase(x.Name),
                    Name = x.Name,
                    Attributes = x.GetAttributes()
                };

            fields = fields.Concat(props);

            if (recursive && classSymbol.BaseType != null && HasAttribute(classSymbol.BaseType, nameof(PrimaryConstructorAttribute)))
            {
                fields = fields.Concat(GetMembers(classSymbol.BaseType, true));
            }

            return fields.ToArray();
        }

        private static string ToCamelCase(string name)
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
}
