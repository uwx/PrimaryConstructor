using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;

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

}