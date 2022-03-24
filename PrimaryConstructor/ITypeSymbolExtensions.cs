using System;
using System.Diagnostics.CodeAnalysis;
using Microsoft.CodeAnalysis;
using System.Linq;

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
}