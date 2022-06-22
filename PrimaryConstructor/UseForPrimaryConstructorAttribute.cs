using System;

namespace PrimaryConstructor;

[AttributeUsage(AttributeTargets.Constructor)]
public class UseForPrimaryConstructorAttribute : Attribute
{
}