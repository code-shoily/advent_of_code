defmodule AdventOfCode.Y2020.Day04 do
  @moduledoc """
  --- Day 4: Passport Processing ---
  Problem Link: https://adventofcode.com/2020/day/4
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 4)

  def run(input \\ input()) do
    input = parse(input)
    {Enum.count(input, &ok?(:k, &1)), Enum.count(input, &ok?(:v, &1))}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.reject(&(&1 == [""]))
    |> Enum.map(&as_pp/1)
  end

  defp as_pp(fields) do
    fields
    |> Enum.join(" ")
    |> String.split(" ")
    |> Enum.map(&List.to_tuple(String.split(&1, ":")))
    |> Enum.into(%{})
    |> Map.put_new("cid", nil)
    |> cast()
  end

  @fields ~w/byr cid ecl eyr hcl hgt iyr pid/

  @birth_year 1920..2002
  @expiration_year 2020..2030
  @eye_colors ~w/amb blu brn gry grn hzl oth/
  @height_cm 59..76
  @height_inch 150..193
  @issuing_year 2010..2020

  defp ok?(:k, pp), do: map_size(Map.take(pp, @fields)) == 8
  defp ok?(:v, pp), do: Enum.all?(Enum.map(@fields, &ok?(&1, pp)))

  defp ok?("cid", %{}), do: true
  defp ok?("byr", %{"byr" => byr}), do: byr in @birth_year
  defp ok?("ecl", %{"ecl" => ecl}), do: ecl in @eye_colors
  defp ok?("eyr", %{"eyr" => eyr}), do: eyr in @expiration_year
  defp ok?("hcl", %{"hcl" => "#" <> hcl}), do: String.length(hcl) == 6
  defp ok?("hgt", %{"hgt" => {hgt, "cm"}}) when hgt in @height_inch, do: true
  defp ok?("hgt", %{"hgt" => {hgt, "in"}}) when hgt in @height_cm, do: true
  defp ok?("hgt", %{"hgt" => _}), do: false
  defp ok?("iyr", %{"iyr" => iyr}), do: iyr in @issuing_year
  defp ok?("pid", %{"pid" => pid}), do: String.length(pid) == 9 && String.to_integer(pid)

  defp ok?(_, _), do: false

  defp cast(%{"byr" => b, "eyr" => e, "hgt" => h, "iyr" => i} = pp) do
    Map.merge(pp, %{
      "byr" => String.to_integer(b),
      "eyr" => String.to_integer(e),
      "hgt" => Integer.parse(h),
      "iyr" => String.to_integer(i)
    })
  end

  defp cast(pp), do: pp
end
