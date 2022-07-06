using System;
using JetBrains.Annotations;

[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct)]
[PublicAPI]
// ReSharper disable once CheckNamespace
public class PrimaryConstructorAttribute : Attribute
{
}
