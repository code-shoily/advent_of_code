defmodule AdventOfCode.Y2020.Day4 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/4
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 4

  def run_1, do: input!() |> process() |> Enum.filter(&key_ok?/1) |> length()

  def run_2,
    do:
      input!()
      |> process()
      |> Enum.filter(&(key_ok?(&1) && value_ok?(&1)))
      |> length()

  def run, do: {run_1(), run_2()}

  def process(input \\ input!()) do
    input
    |> String.split("\n")
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.reject(&(&1 == [""]))
    |> Enum.map(&as_map/1)
  end

  defp as_map(list) do
    list
    |> Enum.join(" ")
    |> String.split(" ")
    |> Enum.map(&(String.split(&1, ":") |> List.to_tuple()))
    |> Enum.into(%{})
  end

  @fields ~w/byr cid ecl eyr hcl hgt iyr pid/
  def key_ok?(data) do
    keyset = data |> Map.keys() |> Enum.into(%MapSet{})
    fieldset = @fields |> Enum.into(%MapSet{})
    keys = MapSet.difference(fieldset, keyset) |> Enum.into([])

    case keys do
      [] -> true
      ["cid"] -> true
      _ -> false
    end
  end

  def value_ok?(%{
        "byr" => byr,
        "eyr" => eyr,
        "iyr" => iyr,
        "hgt" => hgt,
        "hcl" => hcl,
        "ecl" => ecl,
        "pid" => pid
      }) do
    [
      byr?(String.to_integer(byr)),
      iyr?(String.to_integer(iyr)),
      eyr?(String.to_integer(eyr)),
      pid?(pid),
      ecl?(ecl),
      hcl?(String.graphemes(hcl)),
      hgt?(hgt)
    ]
    |> Enum.all?()
  end

  defp byr?(byr), do: 1920 <= byr and byr <= 2002
  defp eyr?(eyr), do: 2020 <= eyr and eyr <= 2030
  defp iyr?(iyr), do: 2010 <= iyr and iyr <= 2020
  defp pid?(pid), do: String.length(pid) == 9 && String.to_integer(pid)
  defp ecl?(ecl), do: ecl in ~w/amb blu brn gry grn hzl oth/
  defp hcl?(["#" | nums]), do: length(nums) == 6
  defp hcl?(_), do: false

  defp hgt?(hgt) do
    case Integer.parse(hgt) do
      {height, "cm"} when 150 <= height and height <= 193 -> true
      {height, "in"} when 59 <= height and height <= 76 -> true
      _ -> false
    end
  end
end
