using System;
using JetBrains.Annotations;

[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
[PublicAPI]
// ReSharper disable once CheckNamespace
public class IncludePrimaryConstructorAttribute : Attribute
{
}
