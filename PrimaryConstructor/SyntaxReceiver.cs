using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace PrimaryConstructor
{
	internal class SyntaxReceiver : ISyntaxReceiver
    {
        public IList<TypeDeclarationSyntax> CandidateClasses { get; } = new List<TypeDeclarationSyntax>();

        /// <summary>
        /// Called for every syntax node in the compilation, we can inspect the nodes and save any information useful for generation
        /// </summary>
        public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
        {
            // any field with at least one attribute is a candidate for property generation
            if (syntaxNode is TypeDeclarationSyntax { AttributeLists.Count: > 0 } classDeclarationSyntax)
            {
                CandidateClasses.Add(classDeclarationSyntax);
            }
            
        }
    }
}
