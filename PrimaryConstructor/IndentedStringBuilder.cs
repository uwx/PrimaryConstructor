using System;
using System.Runtime.CompilerServices;
using System.Text;

namespace PrimaryConstructor;

internal sealed class IndentedStringBuilder : IDisposable
{
    private readonly char _indentChar;
    private readonly int _indentIncrement;

    private readonly StringBuilder _sb;
    private string _completeIndentationString = string.Empty;

    private int _indent;

    /// <summary>
    /// Creates an IndentedStringBuilder
    /// </summary>
    public IndentedStringBuilder(char indentChar, int indentIncrement)
    {
        _sb = new StringBuilder();
        
        _indentChar = indentChar;
        _indentIncrement = indentIncrement;
    }

    /// <summary>
    /// Appends a string
    /// </summary>
    /// <param name="value"></param>
    public void Append(string value)
    {
        _sb.Append(_completeIndentationString).Append(value);
    }

    /// <summary>
    /// Appends a line
    /// </summary>
    /// <param name="value"></param>
    public void AppendLine(string value)
    {
        Append(value);
        _sb.Append(Environment.NewLine);
    }

    public void Append([InterpolatedStringHandlerArgument("")] ref AppendInterpolatedStringHandler handler)
    {
    }

    public void AppendLine([InterpolatedStringHandlerArgument("")] ref AppendInterpolatedStringHandler handler)
    {
        _sb.Append(Environment.NewLine);
    }

    /// <summary>
    /// Creates the actual indentation string
    /// </summary>
    private void UpdateCompleteIndentationString()
    {
        if (_indent * _indentIncrement == _completeIndentationString.Length)
        {
            return;
        }

        _completeIndentationString = new string(_indentChar, _indent * _indentIncrement);
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
    /// <remarks>
    /// Should only be called from <c>using</c> statement, call <see cref="DecreaseIndent"/> otherwise
    /// </remarks>
    void IDisposable.Dispose()
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

    [InterpolatedStringHandler]
    public readonly struct AppendInterpolatedStringHandler
    {
        private readonly StringBuilder _sb;

        public AppendInterpolatedStringHandler(int literalLength, int formattedCount, IndentedStringBuilder stringBuilder)
        {
            _sb = stringBuilder._sb;
            _sb.Append(stringBuilder._completeIndentationString);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void AppendLiteral(string s) => _sb.Append(s);

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void AppendFormatted<T>(T value)
        {
            if (value != null)
            {
                // ReSharper disable once RedundantToStringCallForValueType
                _sb.Append(value.ToString());
            }
        }

        // ReSharper disable once MethodOverloadWithOptionalParameter
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void AppendFormatted<T>(T value, string? format)
        {
            if (value is IFormattable formattable)
            {
                _sb.Append(formattable.ToString(format, null));
            }
            else if (value != null)
            {
                // ReSharper disable once RedundantToStringCallForValueType
                _sb.Append(value.ToString());
            }
        }
    }
}