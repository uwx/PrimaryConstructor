using System;
using JetBrains.Annotations;

[AttributeUsage(AttributeTargets.Constructor)]
[PublicAPI]
// ReSharper disable once CheckNamespace
public class UseForPrimaryConstructorAttribute : Attribute
{
}