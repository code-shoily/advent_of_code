defmodule AdventOfCode.Y2020.Day4 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/4
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 4

  def run_1,
    do: input!() |> process() |> Enum.filter(&ok?(:fields, &1)) |> length()

  def run_2,
    do: input!() |> process() |> Enum.filter(&ok?(:values, &1)) |> length()

  def run, do: {run_1(), run_2()}

  def process(input) do
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
    |> Enum.map(&List.to_tuple(String.split(&1, ":")))
    |> Enum.into(%{})
  end

  @required_fields ~w/byr cid ecl eyr hcl hgt iyr pid/
  defp ok?(:fields, passport) do
    fields = MapSet.new(Map.keys(passport))
    required = MapSet.new(@required_fields)
    missing = MapSet.to_list(MapSet.difference(required, fields))

    case missing do
      [] -> true
      ["cid"] -> true
      _ -> false
    end
  end

  defp ok?(:values, passport) do
    @required_fields
    |> Enum.map(&ok?(&1, passport))
    |> Enum.all?()
  end

  defp ok?("byr", %{"byr" => byr}) do
    byr = String.to_integer(byr)
    1920 <= byr and byr <= 2002
  end

  defp ok?("eyr", %{"eyr" => eyr}) do
    eyr = String.to_integer(eyr)
    2020 <= eyr and eyr <= 2030
  end

  defp ok?("iyr", %{"iyr" => iyr}) do
    iyr = String.to_integer(iyr)
    2010 <= iyr and iyr <= 2020
  end

  defp ok?("pid", %{"pid" => pid}), do: String.length(pid) == 9 && String.to_integer(pid)
  defp ok?("ecl", %{"ecl" => ecl}), do: ecl in ~w/amb blu brn gry grn hzl oth/

  defp ok?("hcl", %{"hcl" => hcl}) do
    case String.graphemes(hcl) do
      ["#" | digits] -> length(digits) == 6
      _ -> false
    end
  end

  defp ok?("hgt", %{"hgt" => hgt}) do
    case Integer.parse(hgt) do
      {height, "cm"} when 150 <= height and height <= 193 -> true
      {height, "in"} when 59 <= height and height <= 76 -> true
      _ -> false
    end
  end

  defp ok?("cid", _), do: true

  defp ok?(_, _), do: false
end
