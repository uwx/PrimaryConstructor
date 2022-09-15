using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
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


// internal class SyntaxReceiver : ISyntaxReceiver
// {
//     public IList<ClassDeclarationSyntax> CandidateClasses { get; } = new List<ClassDeclarationSyntax>();
//
//     /// <summary>
//     /// Called for every syntax node in the compilation, we can inspect the nodes and save any information useful for generation
//     /// </summary>
//     public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
//     {
//         // any field with at least one attribute is a candidate for property generation
//         if (syntaxNode is ClassDeclarationSyntax { AttributeLists.Count: > 0 } classDeclarationSyntax)
//         {
//             CandidateClasses.Add(classDeclarationSyntax);
//         }
//     }
// }

[Generator]
internal class PrimaryConstructorGenerator : IIncrementalGenerator
{
    private static readonly string AttributeName = nameof(PrimaryConstructorAttribute).Replace("Attribute", "");
    
    
    // public void Initialize(GeneratorInitializationContext context)
    // {
    //     context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
    // }
    //
    // public void Execute(GeneratorExecutionContext context)
    // {
    //     if (context.SyntaxReceiver is not SyntaxReceiver receiver)
    //         return;
    //
    //     var classSymbols = GetClassSymbols(context, receiver);
    //     var classNames = new Dictionary<string, int>();
    //     foreach (var classSymbol in classSymbols)
    //     {
    //         classNames.TryGetValue(classSymbol.Name, out var i);
    //         var name = i == 0 ? classSymbol.Name : $"{classSymbol.Name}{i + 1}";
    //         classNames[classSymbol.Name] = i + 1;
    //         context.AddSource($"{name}-{Guid.NewGuid()}.PrimaryConstructor.g.cs",
    //             SourceText.From(CreatePrimaryConstructor(classSymbol), Encoding.UTF8));
    //     }
    // }
    //
    // private static IEnumerable<INamedTypeSymbol> GetClassSymbols(GeneratorExecutionContext context, SyntaxReceiver receiver)
    // {
    //     var compilation = context.Compilation;
    //
    //     return from clazz in receiver.CandidateClasses
    //         let model = compilation.GetSemanticModel(clazz.SyntaxTree)
    //         select model.GetDeclaredSymbol(clazz)! into classSymbol
    //         where classSymbol.HasAttribute(nameof(PrimaryConstructorAttribute))
    //         select classSymbol;
    // }
    
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
// #if DEBUG
        // SpinWait.SpinUntil(() => Debugger.IsAttached);
// #endif

        var sources = context.SyntaxProvider
            .CreateSyntaxProvider(IsCandidate, Transform)
            .Where(static s => s.classSymbol != null);
            // .WithComparer(SymbolEqualityComparer.IncludeNullability);

        context.RegisterSourceOutput(sources, GenerateCode);
    }

    private static void GenerateCode(SourceProductionContext ctx, (INamedTypeSymbol? classSymbol, Compilation compilation) a)
    {
        var (classSymbol, compilation) = a;
        ctx.AddSource(
            $"{classSymbol!.ToDisplayString(FileNameFormat)}-{Guid.NewGuid()}.g.cs",
            CreatePrimaryConstructor(classSymbol, compilation)
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
        
    private static (INamedTypeSymbol classSymbol, Compilation Compilation) Transform(GeneratorSyntaxContext context, CancellationToken cancellationToken)
    {
        var typeDeclaration = (TypeDeclarationSyntax)context.Node;
        if (cancellationToken.IsCancellationRequested || !typeDeclaration.ContainsAttribute(context.SemanticModel, GetAttributeSymbol(context.SemanticModel)))
        {
            return default;
        }
            
        var classSymbol = context.SemanticModel.GetDeclaredSymbol(typeDeclaration)!;
        
        return (classSymbol, context.SemanticModel.Compilation);
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

    private static string CreatePrimaryConstructor(INamedTypeSymbol classSymbol, Compilation compilation)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();

        var baseClassConstructorArgs = classSymbol.GetBaseTypeGenerationMembers(compilation).ToArray();
        var baseConstructorInheritance = baseClassConstructorArgs.Length > 0
            ? $" : base({string.Join(", ", baseClassConstructorArgs.Select(static m => m.ParameterName))})"
            : "";

        var memberList = classSymbol.GetGenerationMembers().ToArray();
        var arguments = (baseClassConstructorArgs.Length == 0 ? memberList : memberList.Concat(baseClassConstructorArgs)).ToArray();

        var source = new IndentedStringBuilder(' ', 4);

        source.AppendLine($"namespace {namespaceName};");

        var cts = new List<INamedTypeSymbol>();
        {
            var ct = classSymbol.ContainingType;
            while (ct != null)
            {
                cts.Add(ct);
                ct = ct.ContainingType;
            }
        }
        cts.Reverse();
        
        foreach (var ct in cts)
        {
            var ctFullTypeName = classSymbol.ToDisplayString(TypeFormat);
            var ctGeneric = ctFullTypeName.IndexOf('<') < 0
                ? ""
                : ctFullTypeName.Substring(ctFullTypeName.IndexOf('<'));

            source.AppendLine($"partial {(ct.IsStructType() ? "struct" : "class")} {ct.Name}{ctGeneric}");
            source.AppendLine("{");
            source.IncreaseIndent();
        }
        
        var fullTypeName = classSymbol.ToDisplayString(TypeFormat);
        var generic = fullTypeName.IndexOf('<') < 0
            ? ""
            : fullTypeName.Substring(fullTypeName.IndexOf('<'));

        source.AppendLine($"partial {(classSymbol.IsStructType() ? "struct" : "class")} {classSymbol.Name}{generic}");
        source.AppendLine("{");
        using (source.IncreaseIndent())
        {
            source.AppendLine($"[{nameof(SynthesizedPrimaryConstructorAttribute).Substring(0, nameof(SynthesizedPrimaryConstructorAttribute).Length - "Attribute".Length)}]");
            source.AppendLine($"public {classSymbol.Name}(");
            using (source.IncreaseIndent())
            {
                var i = 0;
                foreach (var (type, parameterName, name, attributeDatas) in arguments)
                {
                    source.AppendLine($"{type} {parameterName}{(i != arguments.Length - 1 ? "," : "")}");
                    i++;
                }
            }
            source.AppendLine($"){baseConstructorInheritance}");
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

        foreach (var _ in cts)
        {
            source.DecreaseIndent();
            source.AppendLine("}");
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
            // _logwriter.WriteLine($"- {symbol.Name}");
            if (!symbol.CanBeReferencedByName || symbol.IsStatic) continue;
            // _logwriter.WriteLine($"-- CanBeReferencedByName && !IsStatic");

            if (symbol is IFieldSymbol field)
            {
                // _logwriter.WriteLine($"-- field");
                if ((!field.IsReadOnly || field.HasFieldInitializer()) && !field.HasAttribute(includePrimaryConstructor)) continue;
                // _logwriter.WriteLine($"-- readonly and doesnt have initializer or has include attr");
                if (field.HasAttribute(ignorePrimaryConstructor)) continue;
                // _logwriter.WriteLine($"-- doesnt have ignore attr");
    
                yield return new MemberSymbolInfo
                {
                    Type = field.Type.GetCompilableName(),
                    ParameterName = field.Name.ToCamelCase(),
                    Name = field.Name,
                    Attributes = field.GetAttributes()
                };
            }
            else if (symbol is IPropertySymbol prop)
            {
                // _logwriter.WriteLine($"-- prop");
                // _logwriter.WriteLine($"-- IsReadOnly {prop.IsReadOnly}");
                // _logwriter.WriteLine($"-- HasPropertyInitializer {prop.HasPropertyInitializer()}");
                // _logwriter.WriteLine($"-- IsAutoProperty {prop.IsAutoProperty()}");
                if ((!prop.IsReadOnly || prop.HasPropertyInitializer() || !prop.IsAutoProperty()) && !prop.HasAttribute(includePrimaryConstructor)) continue;
                // _logwriter.WriteLine($"-- readonly and doesnt have initializer or has include attr");
                if (prop.HasAttribute(ignorePrimaryConstructor)) continue;
                // _logwriter.WriteLine($"-- doesnt have ignore attr");
                
                yield return new MemberSymbolInfo
                {
                    Type = prop.Type.GetCompilableName(),
                    ParameterName = prop.Name.ToCamelCase(),
                    Name = prop.Name,
                    Attributes = prop.GetAttributes()
                };
            }
        }
    }
    
    private const string GlobalNamespaceValue = "<global namespace>";

    // https://github.com/dotnet/runtime/blob/main/src/libraries/System.Text.Json/gen/Reflection/TypeExtensions.cs
    public static string GetCompilableName(this ITypeSymbol typeSymbol)
    {
        if (typeSymbol is IArrayTypeSymbol)
        {
            return GetCompilableName(typeSymbol) + "[]";
        }

        if (typeSymbol.TypeKind == TypeKind.TypeParameter)
        {
            return typeSymbol.MetadataName;
        }

        StringBuilder sb = new();

        sb.Append("global::");

        var @namespace = typeSymbol.ContainingNamespace?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithGlobalNamespaceStyle(SymbolDisplayGlobalNamespaceStyle.OmittedAsContaining));
        if (!string.IsNullOrEmpty(@namespace) && @namespace != GlobalNamespaceValue)
        {
            sb.Append(@namespace);
            sb.Append('.');
        }

        var argumentIndex = 0;
        AppendTypeChain(sb, typeSymbol, typeSymbol.GetGenericArguments(), ref argumentIndex);

        return sb.ToString();

        static void AppendTypeChain(StringBuilder sb, ITypeSymbol type, ITypeSymbol[] genericArguments, ref int argumentIndex)
        {
            var declaringType = type.ContainingType?.ConstructedFrom;
            if (declaringType != null)
            {
                AppendTypeChain(sb, declaringType, genericArguments, ref argumentIndex);
                sb.Append('.');
            }
            var backTickIndex = type.MetadataName.IndexOf('`');
            if (backTickIndex == -1)
            {
                sb.Append(type.MetadataName);
            }
            else
            {
                sb.Append(type.MetadataName, 0, backTickIndex);

                sb.Append('<');

                var startIndex = argumentIndex;
                argumentIndex = type.GetGenericArguments().Length;
                for (var i = startIndex; i < argumentIndex; i++)
                {
                    if (i != startIndex)
                    {
                        sb.Append(", ");
                    }

                    sb.Append(GetCompilableName(genericArguments[i]));
                }

                sb.Append('>');
            }
        }
    }
    
    private static ITypeSymbol[] GetGenericArguments(this ITypeSymbol typeSymbol)
    {
        if (typeSymbol is not INamedTypeSymbol { IsGenericType: true } namedTypeSymbol)
        {
            return Array.Empty<ITypeSymbol>();
        }

        var args = new List<ITypeSymbol>();
        AddTypeArguments(args, namedTypeSymbol);
        return args.ToArray();

        static void AddTypeArguments(List<ITypeSymbol> args, INamedTypeSymbol typeSymbol)
        {
            if (typeSymbol.ContainingType != null)
            {
                AddTypeArguments(args, typeSymbol.ContainingType);
            }

            args.AddRange(typeSymbol.TypeArguments);
        }
    }

    // private static readonly TextWriter _logwriter = new StreamWriter(@"F:\Stuff1tb\GitHub\Dsharp\Poki\PrimaryConstructor\PrimaryConstructor\saveme." + Process.GetCurrentProcess().MainModule.ModuleName + "." + DateTimeOffset.Now.ToUnixTimeMilliseconds() + ".txt", true);

    public static IEnumerable<MemberSymbolInfo> GetBaseTypeGenerationMembers(
        this INamedTypeSymbol? classSymbol, Compilation compilation
    )
    {
        // _logwriter.WriteLine("In classSymbol: " + classSymbol.GetCompilableName());
        while ((classSymbol = classSymbol?.BaseType) != null)
        {
            if (classSymbol.Name is nameof(Object) or nameof(ValueType))
            {
                yield break;
            }

            // if (classSymbol.MetadataName == "PaneController")
            // {
            //     _logwriter.WriteLine(classSymbol.MetadataName + " memnames: \n" +
            //                                         string.Join(", ", classSymbol.MemberNames) + ";;;;; members: \n" +
            //                                         string.Join(", ", classSymbol.GetMembers().Select(e => e.Name)) + ";;;;;;;;;;;;;;\n" + string.Join(", ", classSymbol.GetAttributes().Select(e => e.AttributeClass?.MetadataName)));
            // }

            // classSymbol = compilation.GetTypeByMetadataName(classSymbol.MetadataName) ?? throw new InvalidOperationException("No INamedTypeSymbol for type: " + classSymbol.MetadataName);
            
            // _logwriter.WriteLine("In baseType: " + classSymbol.GetCompilableName());

            var constructors = classSymbol.GetMembers()
                .OfType<IMethodSymbol>()
                .Where(static e => e.MethodKind == MethodKind.Constructor)
                .ToArray();

            if (constructors.FirstOrDefault(static e => e.HasAttribute(nameof(SynthesizedPrimaryConstructorAttribute))) is {} synthesizedBaseConstructor)
            {
                var i = 0;
                foreach (var parameter in synthesizedBaseConstructor.Parameters)
                {
                    yield return new MemberSymbolInfo
                    {
                        Type = parameter.Type.ToDisplayString(PropertyTypeFormat),
                        ParameterName = string.IsNullOrWhiteSpace(parameter.Name) ? $"__param{i}" : parameter.Name,
                        Name = string.IsNullOrWhiteSpace(parameter.Name) ? $"Param{i}" : parameter.Name,
                        Attributes = ImmutableArray<AttributeData>.Empty
                    };
                    i++;
                }

                yield break; // there is no need to go back further
            }
            else if (classSymbol.HasAttribute(nameof(PrimaryConstructorAttribute)))
            {
                // _logwriter.WriteLine("Got PrimaryConstructorAttr");
                foreach (var member in classSymbol.GetGenerationMembers())
                {
                    // _logwriter.WriteLine("Got member: " + member);
                    yield return member;
                }
            }
            else if (classSymbol.GetRelevantConstructor(constructors) is { } relevantConstructor &&
                     relevantConstructor.Parameters.Length > 0)
            {
                foreach (var parameter in relevantConstructor.Parameters)
                {
                    yield return parameter;
                }
            }
        }
    }

    public static ConstructorSymbolInfo? GetRelevantConstructor(this INamedTypeSymbol classSymbol, IMethodSymbol[] constructors)
    {
        // ReSharper disable once PossibleMultipleEnumeration
        var bestConstructor = constructors
            .FirstOrDefault(static e => e.HasAttribute(nameof(UseForPrimaryConstructorAttribute)));

        if (bestConstructor == null)
        {
            // ReSharper disable once PossibleMultipleEnumeration
            bestConstructor = constructors
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
