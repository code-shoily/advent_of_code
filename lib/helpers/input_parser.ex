defmodule AdventOfCode.Helpers.InputParser do
  @moduledoc """
  Parses and validates input given in command line
  """

  @years 2015..2019
  @days 1..25

  @doc """
  Parses the input from option parser and returns the year and day.

    ## EXAMPLE
      iex> import AdventOfCode.Helpers.InputParser, only: [parse: 1]
      iex> parse(["2019", "2"])
      {2019, 2}
      iex> parse(["2", "2019"])
      nil
      iex> parse(["--year", "2019", "--day", "2"])
      {2019, 2}
      iex> parse(["--year", "2", "--day", "2019"])
      nil
      iex> parse(["2014", "20"])
      nil
      iex> parse(["2019", "31"])
      nil
  """
  def parse(args) do
    case OptionParser.parse(args, strict: [year: :integer, day: :integer]) do
      {[], opts, _} -> opts |> Enum.map(&String.to_integer/1) |> List.to_tuple() |> validated()
      {opts, [], _} -> opts |> Enum.into(%{}) |> validated()
      _ -> nil
    end
  end

  defp validated(%{year: year, day: day}), do: (validate({year, day}) && {year, day}) || nil
  defp validated(v), do: (validate(v) && v) || nil

  defp validate({year, day}), do: [year in @years, day in @days] |> Enum.all?()
  defp validate(_), do: false
end
