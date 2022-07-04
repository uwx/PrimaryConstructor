using System;
using System.Diagnostics.CodeAnalysis;
using Microsoft.CodeAnalysis;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace PrimaryConstructor;

internal static class TypeSymbolExtensions
{
    public static bool IsModuleType([NotNullWhen(true)] this ITypeSymbol? symbol)
        => symbol?.TypeKind == TypeKind.Module;

    public static bool IsInterfaceType([NotNullWhen(true)] this ITypeSymbol? symbol)
        => symbol?.TypeKind == TypeKind.Interface;

    public static bool IsDelegateType([NotNullWhen(true)] this ITypeSymbol? symbol)
        => symbol?.TypeKind == TypeKind.Delegate;

    public static bool IsFunctionPointerType([NotNullWhen(true)] this ITypeSymbol? symbol)
        => symbol?.TypeKind == TypeKind.FunctionPointer;

    public static bool IsStructType([NotNullWhen(true)] this ITypeSymbol? symbol)
        => symbol?.TypeKind == TypeKind.Struct;

    public static bool IsAnonymousType([NotNullWhen(true)] this INamedTypeSymbol? symbol)
        => symbol?.IsAnonymousType == true;

    public static bool HasAttribute(this ISymbol symbol, INamedTypeSymbol attribute)
    {
        return symbol.GetAttributes().Any(a => attribute.Equals(a.AttributeClass, SymbolEqualityComparer.Default));
    }
    
    public static bool ContainsAttribute(this SyntaxNode node, SemanticModel semanticModel, INamedTypeSymbol attributeSymbol)
    {
        var symbol = semanticModel.GetDeclaredSymbol(node);

        return symbol is not null && symbol.HasAttribute(attributeSymbol);
    }

    public static INamedTypeSymbol GetSymbolByType<T>(this Compilation compilation)
    {
        var name = typeof(T).FullName;

        return compilation.GetTypeByMetadataName(name) ?? throw new TypeAccessException($"{name} could not be found in compilation.");
    }
    
    // https://github.com/CollinAlpert/Lombok.NET/blob/0f8419a47ad8ae11d1f7a3e4663b94adcc7368d3/Lombok.NET/Extensions/SyntaxNodeExtensions.cs#L333
    public static bool IsNamed(this AttributeSyntax attribute, string name)
    {
        return attribute.Name.IsKind(SyntaxKind.IdentifierName) && ((IdentifierNameSyntax)attribute.Name).Identifier.Text == name;
    }

}