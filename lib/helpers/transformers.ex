defmodule AdventOfCode.Helpers.Transformers do
  @moduledoc """
  Module to help transform data structures
  """

  @doc ~S"""
  Extracts lines from a string. `trim` configures whether consequetive empty lines should
  be trimmed or not. This is due to the fact some problems have different meanings for single
  versus multiple line gaps while others do not.

  ## Example

      iex> Transformers.lines("")
      []

      iex> Transformers.lines("Hello")
      ["Hello"]

      iex> Transformers.lines("Line 1\nLine 2")
      ["Line 1", "Line 2"]

      iex> Transformers.lines("Line 1\nLine 2\n ")
      ["Line 1", "Line 2"]

  """
  def lines(raw, trim \\ true) do
    raw
    |> String.split(~r{(\r\n|\r|\n)}, trim: trim)
    |> Enum.reject(fn str ->
      String.trim(str) == ""
    end)
  end

  @doc ~S"""
  Extracts paragraph from a string.

  ### Example

      iex> Transformers.sections("")
      []

      iex> Transformers.sections("a\nb\n\nx\ny\n")
      ["a\nb", "x\ny\n"]

  """
  def sections(raw, trim \\ true), do: String.split(raw, ~r{(\r\n\r\n|\r\r|\n\n)}, trim: trim)

  @doc ~S"""
  Extracts lines as ints from a string.

  ## Example

      iex> Transformers.int_lines("")
      []

      iex> Transformers.int_lines("1")
      [1]

      iex> Transformers.int_lines("1\n2")
      [1, 2]

      iex> Transformers.int_lines("1\n2\n ")
      [1, 2]

  """
  def int_lines(raw) do
    raw
    |> lines()
    |> Enum.map(&String.to_integer/1)
  end

  @doc """
  Decomposes a string or number into digits.

  ## Example

      iex> Transformers.digits(123)
      [1, 2, 3]

      iex> Transformers.digits("123")
      [1, 2, 3]

      iex> Transformers.digits("")
      []

      iex> Transformers.digits(0)
      [0]

  """
  def digits(raw) when is_integer(raw), do: raw |> to_string() |> digits()

  def digits(raw) do
    raw
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
  end

  @doc """
  Separates a string as words, based on separator. Defaults to space.

  ## Example

      iex> Transformers.words("a, b, c", ",")
      ["a", "b", "c"]

      iex> Transformers.words("a b c")
      ["a", "b", "c"]

  """
  def words(sentence, sep \\ " ") do
    sentence
    |> String.split(sep, trim: true)
    |> Enum.map(&String.trim/1)
  end

  @doc """
  Separates a string as words, based on separator. Defaults to space.

  ## Example

      iex> Transformers.int_words("1, 2, 3", ",")
      [1, 2, 3]

      iex> Transformers.int_words("1 2 3")
      [1, 2, 3]

  """
  def int_words(sentence, sep \\ " ") do
    sentence
    |> words(sep)
    |> Enum.map(&String.to_integer/1)
  end

  @doc """
  Transforms a 2x2 matrix

  ## Example

      iex> Transformers.transpose([[]])
      []

      iex> Transformers.transpose([])
      []

      iex> Transformers.transpose([[1, 2], [3, 4]])
      [[1, 3], [2, 4]]

      iex> Transformers.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
      [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

  """
  def transpose(matrix) do
    matrix
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end
