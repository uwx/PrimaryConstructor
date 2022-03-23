using System;
using System.Text;

namespace PrimaryConstructor;

public class IndentedStringBuilder : IDisposable
{
    private readonly StringBuilder _sb;
    private string _indentationString = "\t";
    private string _completeIndentationString = "";
    private int _indent = 0;

    /// <summary>
    ///  Creates an IndentedStringBuilder
    /// </summary>
    public IndentedStringBuilder()
    {
        _sb = new StringBuilder();
    }

    /// <summary>
    /// Appends a string
    /// </summary>
    /// <param name="value"></param>
    public void Append(string value)
    {
        _sb.Append(_completeIndentationString + value);
    }

    /// <summary>
    /// Appends a line
    /// </summary>
    /// <param name="value"></param>
    public void AppendLine(string value)
    {
        Append(value + Environment.NewLine);
    }

    /// <summary>
    /// The string/chars to use for indentation, t by default
    /// </summary>
    public string IndentationString
    {
        get => _indentationString;
        set
        {
            _indentationString = value;

            UpdateCompleteIndentationString();
        }
    }

    /// <summary>
    /// Creates the actual indentation string
    /// </summary>
    private void UpdateCompleteIndentationString()
    {
        _completeIndentationString = "";

        for (var i = 0; i < _indent; i++)
            _completeIndentationString += _indentationString;
    }

    /// <summary>
    /// Increases indentation, returns a reference to an IndentedStringBuilder instance which is only to be used for disposal
    /// </summary>
    /// <returns></returns>
    public IndentedStringBuilder IncreaseIndent()
    {
        _indent++;

        UpdateCompleteIndentationString();

        return this;
    }

    /// <summary>
    /// Decreases indentation, may only be called if indentation > 1
    /// </summary>
    public void DecreaseIndent()
    {
        if (_indent > 0)
        {
            _indent--;

            UpdateCompleteIndentationString();
        }
    }

    /// <summary>
    /// Decreases indentation
    /// </summary>
    public void Dispose()
    {
        DecreaseIndent();
    }

    /// <summary>
    /// Returns the text of the internal StringBuilder
    /// </summary>
    /// <returns></returns>
    public override string ToString()
    {
        return _sb.ToString();
    }
}